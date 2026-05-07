;;;; web3:defcontract macro
;;;;
;;;; Given a Solidity ABI, expand into a set of Common Lisp wrapper functions
;;;; that accept ordinary CL values (integers, hex strings, booleans, byte
;;;; strings) and return ordinary CL values. View/pure functions issue an
;;;; eth_call against the contract; non-view functions get a `*-data`
;;;; calldata builder you can pass into `wallet-send-transaction` (or sign
;;;; yourself).
;;;;
;;;; The macro parses the ABI JSON at macroexpand time so every generated
;;;; function name and parameter shape is fixed at compile time. Encoding and
;;;; decoding happen at runtime through the existing web3/contract pipeline.
;;;; Networks/addresses are not baked in; the URL is always passed at the
;;;; call site so the same contract definition works against mainnet/anvil/
;;;; testnets.

(cl:in-package #:web3/easy)

;;; =========================================================================
;;; Type-string normalization
;;; =========================================================================

(defun %normalize-solidity-type (type-string)
  "Reduce a Solidity type to a keyword or shape this macro understands.

   Returns:
     :uint :int :address :bool :string :bytes :bytes-fixed
       — primitive scalars
     (:array  ELEMENT-KIND)            — T[]
     (:fixed-array ELEMENT-KIND SIZE)  — T[N]
     :unsupported                      — tuples, nested arrays we can't yet wrap

   The recursion handles uint256[][] etc., though we don't currently emit
   nested-array push helpers; both array dimensions still resolve to a flat
   AbiArrayVal carrying AbiArrayVal — uncommon enough that we leave it as
   :unsupported until a use case appears."
  (let* ((s (string-downcase type-string))
         (len (length s)))
    (cond
      ;; Array suffix T[] or T[N]
      ((and (>= len 2) (char= (char s (1- len)) #\]))
       (let ((open-bracket (position #\[ s :from-end t)))
         (cond
           ((null open-bracket) :unsupported)
           (t
            (let* ((base-str (subseq s 0 open-bracket))
                   (size-str (subseq s (1+ open-bracket) (1- len)))
                   (base-kind (%normalize-solidity-type base-str)))
              (cond
                ;; Reject nested arrays / unsupported elements for now
                ((or (eq base-kind :unsupported)
                     (consp base-kind))
                 :unsupported)
                ((zerop (length size-str)) (list :array base-kind))
                (t (let ((n (parse-integer size-str :junk-allowed t)))
                     (if n
                         (list :fixed-array base-kind n)
                         :unsupported)))))))))
      ((or (string= s "uint")
           (and (>= len 4) (string= s "uint" :end1 4)))
       :uint)
      ((or (string= s "int")
           (and (>= len 3) (string= s "int" :end1 3)
                (or (= len 3)
                    (digit-char-p (char s 3)))))
       :int)
      ((string= s "address") :address)
      ((string= s "bool") :bool)
      ((string= s "string") :string)
      ((string= s "bytes") :bytes)
      ((and (>= len 5) (string= s "bytes" :end1 5))
       :bytes-fixed)
      (t :unsupported))))

(defun %array-kind? (kind)
  "Predicate: is KIND a (:array ...) or (:fixed-array ...)?"
  (and (consp kind) (member (car kind) '(:array :fixed-array))))

;;; =========================================================================
;;; ABI JSON parsing (CL-side, macroexpand time)
;;; =========================================================================

(defun %abi-params (raw-params)
  "Convert cl-json's parsed params into the plist shape we use internally."
  (mapcar (lambda (p)
            (list :name (cdr (assoc :name p))
                  :type (cdr (assoc :type p))
                  :indexed (and (assoc :indexed p)
                                (cdr (assoc :indexed p)))))
          raw-params))

(defun %abi-functions (abi-json-string)
  "Return the ABI's function entries as a list of plists shaped like:
     (:name STRING :inputs (...) :outputs (...) :mutability KEYWORD)
   Mutability keyword is one of :view :pure :nonpayable :payable."
  (let ((parsed (cl-json:decode-json-from-string abi-json-string))
        (functions '()))
    (dolist (item parsed (nreverse functions))
      (let ((kind (cdr (assoc :type item))))
        (when (and (stringp kind) (string-equal kind "function"))
          (push
           (list :name (cdr (assoc :name item))
                 :inputs (%abi-params (cdr (assoc :inputs item)))
                 :outputs (%abi-params (cdr (assoc :outputs item)))
                 :mutability (let ((m (cdr (assoc :state-mutability item))))
                               (cond
                                 ((null m) :nonpayable)
                                 ((string-equal m "view") :view)
                                 ((string-equal m "pure") :pure)
                                 ((string-equal m "payable") :payable)
                                 (t :nonpayable))))
           functions))))))

(defun %abi-events (abi-json-string)
  "Return the ABI's event entries as a list of plists shaped like:
     (:name STRING :inputs ((:name :type :indexed) ...) :anonymous BOOL)"
  (let ((parsed (cl-json:decode-json-from-string abi-json-string))
        (events '()))
    (dolist (item parsed (nreverse events))
      (let ((kind (cdr (assoc :type item))))
        (when (and (stringp kind) (string-equal kind "event"))
          (push
           (list :name (cdr (assoc :name item))
                 :inputs (%abi-params (cdr (assoc :inputs item)))
                 :anonymous (and (assoc :anonymous item)
                                 (cdr (assoc :anonymous item))))
           events))))))

;;; =========================================================================
;;; Name conversion: Solidity camelCase -> Lisp kebab-case symbol
;;; =========================================================================

(defun %camel->kebab (str)
  "Convert balanceOfBatch -> BALANCE-OF-BATCH."
  (with-output-to-string (s)
    (loop for ch across str
          for i from 0
          do (cond
               ((upper-case-p ch)
                (when (plusp i) (write-char #\- s))
                (write-char (char-downcase ch) s))
               (t (write-char ch s))))))

(defun %sym (package &rest parts)
  "Intern a symbol made by joining PARTS into PACKAGE."
  (intern (string-upcase (format nil "~{~A~^-~}" parts)) package))

;;; =========================================================================
;;; Runtime helpers used by macro-expanded code
;;; =========================================================================

(defvar *contract-cache* (make-hash-table :test 'equal)
  "Cache of (address-hex . abi-json-string) -> Coalton Contract instance.")

(defun %get-contract (address-hex abi-json)
  "Return a cached or freshly-parsed Coalton Contract for ADDRESS-HEX/ABI-JSON."
  (let ((key (cons address-hex abi-json)))
    (or (gethash key *contract-cache*)
        (let ((address (%parse-address address-hex)))
          (setf (gethash key *contract-cache*)
                (%check (c (coalton:coalton
                             (web3/contract:contract-from-abi-json
                              (coalton:lisp web3/address:Address () address)
                              (coalton:lisp coalton:String () abi-json))))
                  c))))))

;;; =========================================================================
;;; Per-arg encoding emitters
;;; =========================================================================
;;;
;;; Each of these returns a Coalton form that prepends an arg onto an
;;; AbiValue accumulator. They run inside the (coalton:coalton ...) block
;;; emitted by the macro.

(defun %emit-push (kind cl-var-name acc-form)
  "Generate the Coalton form that pushes CL-VAR-NAME (interpreted as KIND)
   onto ACC-FORM. CL-VAR-NAME is the symbol naming the CL local. KIND is
   either a primitive keyword (:uint :int :address ...) or an array shape
   (:array PRIM) / (:fixed-array PRIM N)."
  (cond
    ((%array-kind? kind)
     (%emit-push-array kind cl-var-name acc-form))
    (t (%emit-push-scalar kind cl-var-name acc-form))))

(defun %emit-push-scalar (kind cl-var-name acc-form)
  (ecase kind
    (:uint
     `(web3/easy-bridge:push-uint
       (coalton:lisp web3/types:U256 ()
         (%integer-to-u256 ,cl-var-name))
       ,acc-form))
    (:int
     `(web3/easy-bridge:push-int
       (coalton:lisp coalton:Integer () ,cl-var-name)
       ,acc-form))
    (:address
     `(web3/easy-bridge:push-address
       (coalton:lisp web3/address:Address ()
         (%parse-address ,cl-var-name))
       ,acc-form))
    (:bool
     `(web3/easy-bridge:push-bool
       (coalton:lisp coalton:Boolean ()
         (if ,cl-var-name cl:t cl:nil))
       ,acc-form))
    (:string
     `(web3/easy-bridge:push-string
       (coalton:lisp coalton:String () ,cl-var-name)
       ,acc-form))
    (:bytes
     `(web3/easy-bridge:push-bytes
       (coalton:lisp web3/types:Bytes ()
         (%hex-to-bytes ,cl-var-name))
       ,acc-form))
    (:bytes-fixed
     `(web3/easy-bridge:push-bytes-fixed
       (coalton:lisp web3/types:Bytes ()
         (%hex-to-bytes ,cl-var-name))
       ,acc-form))))

(defun %array-coalton-list-form (element-kind cl-var)
  "Emit a (coalton:lisp (List <element>) () ...) form whose CL body converts
   the CL list bound to CL-VAR into a Coalton-typed list of element values.

   Coalton's List is :repr :native cl:list, so a `mapcar` over the CL list
   produces a value that Coalton can re-interpret as the declared list type."
  (ecase element-kind
    (:uint
     `(coalton:lisp (coalton:List web3/types:U256) ()
        (mapcar #'%integer-to-u256 ,cl-var)))
    (:int
     `(coalton:lisp (coalton:List coalton:Integer) () ,cl-var))
    (:address
     `(coalton:lisp (coalton:List web3/address:Address) ()
        (mapcar #'%parse-address ,cl-var)))
    (:bool
     ;; Map CL truthiness to Coalton booleans up-front, then escape.
     `(coalton:lisp (coalton:List coalton:Boolean) ()
        (mapcar (lambda (b) (if b cl:t cl:nil)) ,cl-var)))
    (:string
     `(coalton:lisp (coalton:List coalton:String) () ,cl-var))
    ((:bytes :bytes-fixed)
     `(coalton:lisp (coalton:List web3/types:Bytes) ()
        (mapcar #'%hex-to-bytes ,cl-var)))))

(defun %emit-push-array (kind cl-var-name acc-form)
  "Emit a wrap-* call for an (:array E) or (:fixed-array E N) input."
  (let* ((tag         (first kind))
         (element     (second kind))
         (wrapper-sym (ecase element
                        (:uint
                         (if (eq tag :array)
                             'web3/easy-bridge:wrap-uint-array
                             'web3/easy-bridge:wrap-uint-fixed-array))
                        (:int
                         (if (eq tag :array)
                             'web3/easy-bridge:wrap-int-array
                             'web3/easy-bridge:wrap-int-fixed-array))
                        (:bool
                         (if (eq tag :array)
                             'web3/easy-bridge:wrap-bool-array
                             'web3/easy-bridge:wrap-bool-fixed-array))
                        (:string
                         (if (eq tag :array)
                             'web3/easy-bridge:wrap-string-array
                             'web3/easy-bridge:wrap-string-fixed-array))
                        (:address
                         (if (eq tag :array)
                             'web3/easy-bridge:wrap-address-array
                             'web3/easy-bridge:wrap-address-fixed-array))
                        ((:bytes :bytes-fixed)
                         ;; bytes32[] is common; encode as dynamic outer with
                         ;; AbiBytesFixedVal elements. T[N] of bytes is rare;
                         ;; fall back to the same wrapper.
                         'web3/easy-bridge:wrap-bytes-fixed-element-array))))
    `(coalton:Cons
       (,wrapper-sym ,(%array-coalton-list-form element cl-var-name))
       ,acc-form)))

(defun %emit-args-form (kinds cl-vars)
  "Build the Coalton expression that yields the AbiValue list from CL vars,
   in the order declared by the ABI (first arg at the head of the list).

   We assemble by repeatedly pushing onto an accumulator. To end up with
   the declared order, we iterate the args right-to-left so that the
   leftmost arg ends up at the *outermost* push (i.e. at the head)."
  (let ((acc 'web3/easy-bridge:empty-args))
    (loop for kind in (reverse kinds)
          for var  in (reverse cl-vars)
          do (setf acc (%emit-push kind var acc)))
    acc))

;;; =========================================================================
;;; Output decoding emitters
;;; =========================================================================

(defun %emit-extract (kind decoded-coalton-var)
  "Emit a Coalton form that extracts a value of KIND from DECODED-COALTON-VAR
   (a Coalton `(List AbiValue)`). For arrays the value comes back as a
   Coalton list of the element type; for scalars it's a single value."
  (cond
    ((%array-kind? kind)
     (let ((element (second kind)))
       (ecase element
         (:uint    `(web3/easy-bridge:extract-uint-array ,decoded-coalton-var))
         (:int     `(web3/easy-bridge:extract-int-array ,decoded-coalton-var))
         (:bool    `(web3/easy-bridge:extract-bool-array ,decoded-coalton-var))
         (:string  `(web3/easy-bridge:extract-string-array ,decoded-coalton-var))
         (:address `(web3/easy-bridge:extract-address-array ,decoded-coalton-var))
         ((:bytes :bytes-fixed)
          `(web3/easy-bridge:extract-bytes-array ,decoded-coalton-var)))))
    (t
     (ecase kind
       (:uint    `(web3/easy-bridge:extract-uint ,decoded-coalton-var))
       (:int     `(web3/easy-bridge:extract-int ,decoded-coalton-var))
       (:bool    `(web3/easy-bridge:extract-bool ,decoded-coalton-var))
       (:string  `(web3/easy-bridge:extract-string ,decoded-coalton-var))
       (:address `(web3/easy-bridge:extract-address ,decoded-coalton-var))
       (:bytes   `(web3/easy-bridge:extract-bytes ,decoded-coalton-var))
       (:bytes-fixed `(web3/easy-bridge:extract-bytes ,decoded-coalton-var))))))

(defun %emit-finalize (kind raw-cl-var)
  "Convert RAW-CL-VAR (the value handed back from Coalton) into the final CL
   representation. For arrays we map the per-element finalizer over the list."
  (cond
    ((%array-kind? kind)
     (let ((element (second kind)))
       (ecase element
         (:uint    `(mapcar #'%u256-to-integer ,raw-cl-var))
         (:int     raw-cl-var)
         (:bool    raw-cl-var)
         (:string  raw-cl-var)
         (:address `(mapcar #'%address-to-hex ,raw-cl-var))
         ((:bytes :bytes-fixed)
          `(mapcar #'%bytes-to-hex ,raw-cl-var)))))
    (t
     (ecase kind
       (:uint    `(%u256-to-integer ,raw-cl-var))
       (:int     raw-cl-var)
       (:bool    raw-cl-var)
       (:string  raw-cl-var)
       (:address `(%address-to-hex ,raw-cl-var))
       ((:bytes :bytes-fixed) `(%bytes-to-hex ,raw-cl-var))))))

;;; =========================================================================
;;; Per-function expanders
;;; =========================================================================

(defun %arg-symbol (param-plist index)
  "Pick a CL parameter name for an ABI input. Use the named arg if the ABI
   gives one; otherwise fall back to ARG0/ARG1/..."
  (let ((n (getf param-plist :name)))
    (if (and (stringp n) (plusp (length n)))
        (intern (string-upcase (%camel->kebab n)))
        (intern (format nil "ARG~D" index)))))

;;; The macro emits forms wrapped in (coalton:coalton ...). That macro
;;; round-trips its body through a printed string with the Coalton readtable,
;;; which destroys gensym identity (uninterned `#:...` symbols become fresh
;;; uninterned symbols on re-read). So any binding referenced inside a
;;; (coalton:lisp ... () SYM) escape must be a stable interned symbol. We
;;; use these private names from the web3/easy package; the generated
;;; defuns are flat (no nesting) so reuse is safe.

(defparameter %sym-url      'url
  "CL parameter name for the JSON-RPC endpoint, exposed to user-written calls.")
(defparameter %sym-provider 'web3/easy::%bound-provider)
(defparameter %sym-contract 'web3/easy::%bound-contract)
(defparameter %sym-decoded  'web3/easy::%bound-decoded)
(defparameter %sym-raw      'web3/easy::%bound-raw)
(defparameter %sym-calldata 'web3/easy::%bound-calldata)
(defparameter %sym-items    'web3/easy::%bound-items)

(defun %expand-read-fn (defun-name fn-name address-form abi-form
                        input-kinds input-syms output-kinds)
  "Expand a view/pure function. Issues eth_call and decodes a single output
   (or returns a list of values for multi-output)."
  (cond
    ((null output-kinds)
     ;; No-output view: rare but valid (returns t on success)
     `(defun ,defun-name (,%sym-url ,@input-syms)
        (let ((,%sym-provider (%provider ,%sym-url))
              (,%sym-contract (%get-contract ,address-form ,abi-form)))
          (%check (,%sym-decoded
                   (coalton:coalton
                     (web3/easy-bridge:contract-encode-and-call
                      (coalton:lisp web3/provider:HttpProvider () ,%sym-provider)
                      (coalton:lisp web3/contract:Contract () ,%sym-contract)
                      (coalton:lisp coalton:String () ,fn-name)
                      ,(%emit-args-form input-kinds input-syms))))
            (declare (ignore ,%sym-decoded))
            t))))
    ((= (length output-kinds) 1)
     (let ((out-kind (first output-kinds)))
       `(defun ,defun-name (,%sym-url ,@input-syms)
          (let ((,%sym-provider (%provider ,%sym-url))
                (,%sym-contract (%get-contract ,address-form ,abi-form)))
            (let ((,%sym-decoded
                    (%check (d (coalton:coalton
                                 (web3/easy-bridge:contract-encode-and-call
                                  (coalton:lisp web3/provider:HttpProvider () ,%sym-provider)
                                  (coalton:lisp web3/contract:Contract () ,%sym-contract)
                                  (coalton:lisp coalton:String () ,fn-name)
                                  ,(%emit-args-form input-kinds input-syms))))
                      d)))
              (let ((,%sym-raw
                      (%check (v (coalton:coalton
                                   ,(%emit-extract
                                     out-kind
                                     `(coalton:lisp (coalton:List web3/abi:AbiValue) ()
                                                    ,%sym-decoded))))
                        v)))
                ,(%emit-finalize out-kind %sym-raw)))))))
    (t
     ;; Multi-output: return a CL list. The decoded list is walked with
     ;; runtime extractors of the corresponding kind.
     `(defun ,defun-name (,%sym-url ,@input-syms)
        (let ((,%sym-provider (%provider ,%sym-url))
              (,%sym-contract (%get-contract ,address-form ,abi-form)))
          (let ((,%sym-decoded
                  (%check (d (coalton:coalton
                               (web3/easy-bridge:contract-encode-and-call
                                (coalton:lisp web3/provider:HttpProvider () ,%sym-provider)
                                (coalton:lisp web3/contract:Contract () ,%sym-contract)
                                (coalton:lisp coalton:String () ,fn-name)
                                ,(%emit-args-form input-kinds input-syms))))
                    d)))
            (let ((,%sym-items ,%sym-decoded))
              (declare (ignorable ,%sym-items))
              (list
               ,@(loop for k in output-kinds
                       collect `(let ((,%sym-raw
                                        (%check (v (coalton:coalton
                                                     ,(%emit-extract
                                                       k
                                                       `(coalton:lisp (coalton:List web3/abi:AbiValue) ()
                                                                      ,%sym-items))))
                                          v)))
                                  ,(%emit-finalize k %sym-raw)))))))))))

(defun %expand-data-fn (defun-name fn-name address-form abi-form
                        input-kinds input-syms)
  "Expand a calldata builder for a non-view function: returns 0x-prefixed hex."
  `(defun ,defun-name (,@input-syms)
     (let ((,%sym-contract (%get-contract ,address-form ,abi-form)))
       (let ((,%sym-calldata
               (%check (b (coalton:coalton
                            (web3/easy-bridge:contract-encode-calldata
                             (coalton:lisp web3/contract:Contract () ,%sym-contract)
                             (coalton:lisp coalton:String () ,fn-name)
                             ,(%emit-args-form input-kinds input-syms))))
                 b)))
         (%bytes-to-hex ,%sym-calldata)))))

(defparameter %sym-wallet  'wallet
  "CL parameter name for the wallet handle, exposed to user-written calls.")
(defparameter %sym-value   'value-wei
  "CL keyword parameter name for ETH value sent alongside a call.")
(defparameter %sym-inner   'web3/easy::%bound-wallet-inner)
(defparameter %sym-value-u 'web3/easy::%bound-value-u)

;;; =========================================================================
;;; Event helpers
;;; =========================================================================

(defparameter %sym-topics 'topics-hex
  "CL parameter name for the topics hex list, exposed to user-written calls.")
(defparameter %sym-data-arg 'data-hex
  "CL parameter name for the data hex string, exposed to user-written calls.")
(defparameter %sym-topics-bytes 'web3/easy::%bound-topics-bytes)
(defparameter %sym-data-bytes   'web3/easy::%bound-data-bytes)

(defun %event-arg-extractor (kind idx-form decoded-var)
  "Emit a Coalton form that extracts arg IDX-FORM from DECODED-VAR (a CL list
   of AbiValue structs) according to KIND. Returns a Web3Result of the
   appropriate primitive type."
  ;; Arrays inside events are uncommon in practice; we treat them like
  ;; bytes (extracts the raw AbiArrayVal data via the bytes extractor's
  ;; fallback path, which signals an Err — the user can drop down to
  ;; web3/events for those).
  (when (%array-kind? kind)
    (return-from %event-arg-extractor
      `(web3/easy-bridge:event-arg-bytes
        (coalton:lisp (coalton:List web3/abi:AbiValue) () ,decoded-var)
        (coalton:lisp coalton:UFix () ,idx-form))))
  (let ((bridge-fn
          (ecase kind
            (:uint    'web3/easy-bridge:event-arg-uint)
            (:int     'web3/easy-bridge:event-arg-int)
            (:bool    'web3/easy-bridge:event-arg-bool)
            (:string  'web3/easy-bridge:event-arg-string)
            (:address 'web3/easy-bridge:event-arg-address)
            ((:bytes :bytes-fixed) 'web3/easy-bridge:event-arg-bytes))))
    `(,bridge-fn
       (coalton:lisp (coalton:List web3/abi:AbiValue) () ,decoded-var)
       (coalton:lisp coalton:UFix () ,idx-form))))

(defun %expand-event-topic-fn (defun-name event-name address-form abi-form)
  "Generate `<contract>-event-<name>-topic`: zero-arg fn returning the topic
   as a 0x-prefixed 32-byte hex string."
  `(defun ,defun-name ()
     (let ((,%sym-contract (%get-contract ,address-form ,abi-form)))
       (let ((,%sym-raw
               (%check (b (coalton:coalton
                            (web3/easy-bridge:event-topic-bytes
                             (coalton:lisp web3/contract:Contract () ,%sym-contract)
                             (coalton:lisp coalton:String () ,event-name))))
                 b)))
         (%bytes-to-hex ,%sym-raw)))))

(defun %expand-event-decode-fn (defun-name event-name address-form abi-form
                                input-kinds input-keys)
  "Generate `<contract>-event-<name>` taking (TOPICS-HEX-LIST DATA-HEX) and
   returning a CL plist of named fields. INPUT-KEYS are the keyword names
   in plist order (one per input). INPUT-KINDS are aligned `:uint`/`:address`/
   etc. for the per-arg extractors.

   Bindings inside (coalton:lisp ... () SYM) escapes must be stable interned
   symbols — `coalton:coalton` round-trips its body through a printed string,
   which destroys gensym identity."
  `(defun ,defun-name (,%sym-topics ,%sym-data-arg)
     (let ((,%sym-contract     (%get-contract ,address-form ,abi-form))
           (,%sym-topics-bytes (mapcar #'%hex-to-bytes ,%sym-topics))
           (,%sym-data-bytes   (%hex-to-bytes ,%sym-data-arg)))
       (let ((,%sym-decoded
               (%check (d (coalton:coalton
                            (web3/easy-bridge:decode-event-by-name
                             (coalton:lisp web3/contract:Contract () ,%sym-contract)
                             (coalton:lisp coalton:String () ,event-name)
                             (coalton:lisp (coalton:List web3/types:Bytes) () ,%sym-topics-bytes)
                             (coalton:lisp web3/types:Bytes () ,%sym-data-bytes))))
                 d)))
         (list
          ,@(loop for kind in input-kinds
                  for key  in input-keys
                  for idx from 0
                  collect key
                  collect `(let ((,%sym-raw
                                   (%check (v (coalton:coalton
                                                ,(%event-arg-extractor kind idx %sym-decoded)))
                                     v)))
                             ,(%emit-finalize kind %sym-raw))))))))

(defun %expand-send-fn (defun-name fn-name address-form abi-form
                        input-kinds input-syms payable?)
  "Expand a send wrapper for a non-view function: signs + sends and returns
   the resulting transaction hash. WALLET is a `web3:wallet` handle.
   For payable functions, accept `:value-wei` for the ETH attached to the
   call; for non-payable functions the keyword still exists but defaults
   to 0 and may be omitted."
  (declare (ignore payable?))
  `(defun ,defun-name (,%sym-wallet ,@input-syms &key ((:value-wei ,%sym-value) 0))
     (let ((,%sym-inner   (%wallet-inner ,%sym-wallet))
           (,%sym-contract (%get-contract ,address-form ,abi-form))
           (,%sym-value-u  (%integer-to-u256 ,%sym-value)))
       (%check (hash
                (coalton:coalton
                  (web3/contract-write:send-function-call
                   (coalton:lisp web3/wallet:Wallet () ,%sym-inner)
                   (coalton:lisp web3/contract:Contract () ,%sym-contract)
                   (coalton:lisp coalton:String () ,fn-name)
                   ,(%emit-args-form input-kinds input-syms)
                   (coalton:lisp web3/types:U256 () ,%sym-value-u))))
         hash))))

;;; =========================================================================
;;; The macro
;;; =========================================================================

(defmacro defcontract (name &key address abi abi-file)
  "Generate plain CL wrappers for the functions in a Solidity ABI.

   Required:
     :address  — 0x-prefixed contract address (string).
     :abi or :abi-file — the contract ABI as a JSON string, or a path to
                          a JSON file. Read at macroexpand time.

   For each ABI function, the macro generates one of:

     (NAME-FUNCTION-NAME url arg ...)         ; for view/pure: returns CL value
     (NAME-FUNCTION-NAME-data arg ...)        ; for nonpayable/payable: returns calldata hex

   Supported argument and return types: uint*, int*, address, bool, string,
   bytes, bytesN, plus arrays of any of the above (T[] and T[N]). Tuples
   (Solidity structs) are not yet wrapped; functions whose I/O contains a
   tuple are skipped with a compile-time warning — fall back to the typed
   `web3/contract` API for those.

   Example:
     (web3:defcontract usdc
       :address \"0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48\"
       :abi-file \"USDC.json\")
     (usdc-balance-of url holder-hex)         ; -> integer
     (usdc-transfer-data recipient-hex 1000)  ; -> 0x... calldata"
  (let* ((abi-string (cond
                       ;; Allow :abi to be either a literal string at macro
                       ;; expansion time or a symbol/form bound at compile
                       ;; time (e.g. a `defparameter`). The latter case is
                       ;; common in tests and shared fixtures.
                       (abi (cond
                              ((stringp abi) abi)
                              ((and (symbolp abi) (boundp abi)) (symbol-value abi))
                              (t (eval abi))))
                       (abi-file (uiop:read-file-string abi-file))
                       (t (error "defcontract: must supply :abi or :abi-file"))))
         (parsed-fns (%abi-functions abi-string))
         (parsed-events (%abi-events abi-string))
         (pkg      *package*)
         (defs     '()))
    (dolist (fn parsed-fns)
      (let* ((sol-name      (getf fn :name))
             (mutability    (getf fn :mutability))
             (inputs        (getf fn :inputs))
             (outputs       (getf fn :outputs))
             (input-kinds   (mapcar (lambda (p)
                                      (%normalize-solidity-type (getf p :type)))
                                    inputs))
             (output-kinds  (mapcar (lambda (p)
                                      (%normalize-solidity-type (getf p :type)))
                                    outputs))
             (read-only?    (member mutability '(:view :pure))))
        (cond
          ((or (null sol-name) (zerop (length sol-name)))
           nil)  ; unnamed / fallback / receive — skip silently
          ((or (find :unsupported input-kinds)
               (find :unsupported output-kinds))
           (warn "defcontract ~A: skipping ~A (uses unsupported type — arrays/tuples not yet wrapped)"
                 name sol-name))
          (t
           (let* ((kebab     (%camel->kebab sol-name))
                  (read-name (%sym pkg name kebab))
                  (data-name (%sym pkg name kebab "data"))
                  (send-name (%sym pkg name kebab))
                  (input-syms (loop for p in inputs
                                    for i from 0
                                    collect (%arg-symbol p i)))
                  (payable?  (eq mutability :payable)))
             (cond
               (read-only?
                (push (%expand-read-fn read-name sol-name
                                       (list 'quote address)
                                       (list 'quote abi-string)
                                       input-kinds input-syms output-kinds)
                      defs))
               (t
                ;; Non-view: emit BOTH a calldata builder (`-data`) for users
                ;; who want to embed the call in something custom, and a send
                ;; wrapper (no suffix) that signs + broadcasts via the
                ;; existing wallet/contract-write pipeline.
                (push (%expand-data-fn data-name sol-name
                                       (list 'quote address)
                                       (list 'quote abi-string)
                                       input-kinds input-syms)
                      defs)
                (push (%expand-send-fn send-name sol-name
                                       (list 'quote address)
                                       (list 'quote abi-string)
                                       input-kinds input-syms payable?)
                      defs))))))))
    ;; Now process events. Per-event we emit:
    ;;   <name>-event-<event>-topic         — the topic0 hex (zero-arg)
    ;;   <name>-event-<event>               — decoder taking (topics-hex data-hex)
    ;;
    ;; The "event-" infix keeps these from clashing with same-named function
    ;; wrappers (e.g. ERC-20's `Transfer` event vs `transfer` function both
    ;; map to `<name>-transfer` without the infix).
    (dolist (ev parsed-events)
      (let* ((sol-name      (getf ev :name))
             (inputs        (getf ev :inputs))
             (input-kinds   (mapcar (lambda (p)
                                      (%normalize-solidity-type (getf p :type)))
                                    inputs)))
        (cond
          ((or (null sol-name) (zerop (length sol-name))) nil)
          ((find :unsupported input-kinds)
           (warn "defcontract ~A: skipping event ~A (uses unsupported type)"
                 name sol-name))
          (t
           (let* ((kebab     (%camel->kebab sol-name))
                  (decode-name (%sym pkg name "event" kebab))
                  (topic-name  (%sym pkg name "event" kebab "topic"))
                  (input-keys  (loop for p in inputs
                                     for i from 0
                                     for n = (getf p :name)
                                     collect (intern
                                              (string-upcase
                                               (if (and (stringp n) (plusp (length n)))
                                                   (%camel->kebab n)
                                                   (format nil "ARG~D" i)))
                                              :keyword))))
             (push (%expand-event-topic-fn topic-name sol-name
                                           (list 'quote address)
                                           (list 'quote abi-string))
                   defs)
             (push (%expand-event-decode-fn decode-name sol-name
                                            (list 'quote address)
                                            (list 'quote abi-string)
                                            input-kinds input-keys)
                   defs))))))
    `(progn ,@(nreverse defs)
            ',name)))
