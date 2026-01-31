;;;; ABI Parser implementation
;;;; Parse Solidity ABI JSON and type strings

(in-package #:web3/abi-parser)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; ABI Parameter
  ;;; =========================================================================

  (define-struct AbiParam
    "A parameter in an ABI function or event"
    (param-name String)
    (param-type abi:AbiType)
    (param-indexed Boolean))  ; For events

  (declare make-abi-param (String -> abi:AbiType -> Boolean -> AbiParam))
  (define (make-abi-param name typ indexed)
    (AbiParam name typ indexed))

  ;;; =========================================================================
  ;;; Parsed Function
  ;;; =========================================================================

  (define-struct ParsedFunction
    "A parsed function from ABI JSON"
    (fn-name String)
    (fn-inputs (List AbiParam))
    (fn-outputs (List AbiParam))
    (fn-selector types:Bytes)
    (fn-state-mutability String))  ; "pure", "view", "nonpayable", "payable"

  ;;; =========================================================================
  ;;; Parsed Event
  ;;; =========================================================================

  (define-struct ParsedEvent
    "A parsed event from ABI JSON"
    (event-name String)
    (event-inputs (List AbiParam))
    (event-topic types:Bytes)      ; keccak256 of signature (topic0)
    (event-anonymous Boolean))

  ;;; =========================================================================
  ;;; ABI Item Types
  ;;; =========================================================================

  (define-type AbiItem
    "An item from an ABI JSON array"
    (AbiFunction ParsedFunction)
    (AbiEvent ParsedEvent)
    (AbiConstructor (List AbiParam))
    (AbiFallback)
    (AbiReceive)
    (AbiError String (List AbiParam))))  ; Custom error with name and params


;;; =========================================================================
;;; Type String Parsing
;;; =========================================================================
;;; Parse strings like "uint256", "address[]", "bytes32", "(uint256,address)"

(coalton-toplevel

  (declare parse-abi-type (String -> (types:Web3Result abi:AbiType)))
  (define (parse-abi-type type-str)
    "Parse a Solidity type string into an AbiType"
    (type-string-to-abi-type type-str))

  (declare type-string-to-abi-type (String -> (types:Web3Result abi:AbiType)))
  (define (type-string-to-abi-type s)
    "Convert a Solidity type string to an AbiType - delegates to CL"
    (lisp (types:Web3Result abi:AbiType) (s)
      (%parse-type-string-cl (cl:string-trim " " s))))

  ;; Helper to unwrap Ok - used from CL level to extract values
  (declare %unwrap-abi-type-ok ((types:Web3Result abi:AbiType) -> abi:AbiType))
  (define (%unwrap-abi-type-ok result)
    "Unwrap Ok value (only call when you know it's Ok!)"
    (match result
      ((Ok v) v)
      ((Err _) abi:AbiAddress)))  ; Fallback, shouldn't be reached

  ;; Check if a type result is Ok
  (declare %type-result-ok? ((types:Web3Result abi:AbiType) -> Boolean))
  (define (%type-result-ok? result)
    (match result
      ((Ok _) True)
      ((Err _) False)))

)

;;; CL-level type parser (cleaner than mixing Coalton string ops)

(cl:defun %parse-type-string-cl (s)
  "Parse a Solidity type string, returning Coalton Result"
  (cl:cond
    ;; Empty string
    ((cl:zerop (cl:length s))
     (Err (web3/types:AbiError "Empty type string")))

    ;; Tuple: starts with (
    ((cl:char= (cl:char s 0) #\()
     (%parse-tuple-type-cl s))

    ;; Array: ends with ]
    ((cl:and (cl:> (cl:length s) 2)
             (cl:char= (cl:char s (cl:1- (cl:length s))) #\]))
     (%parse-array-type-cl s))

    ;; uint<N>
    ((cl:and (cl:>= (cl:length s) 4)
             (cl:string= "uint" (cl:subseq s 0 4)))
     (%parse-uint-type-cl s))

    ;; int<N> (but not uint)
    ((cl:and (cl:>= (cl:length s) 3)
             (cl:string= "int" (cl:subseq s 0 3))
             (cl:not (cl:string= "uint" (cl:subseq s 0 (cl:min 4 (cl:length s))))))
     (%parse-int-type-cl s))

    ;; address
    ((cl:string= s "address")
     (Ok web3/abi:AbiAddress))

    ;; bool
    ((cl:string= s "bool")
     (Ok web3/abi:AbiBool))

    ;; string
    ((cl:string= s "string")
     (Ok web3/abi:AbiString))

    ;; bytes (dynamic)
    ((cl:string= s "bytes")
     (Ok web3/abi:AbiBytes))

    ;; bytes<N>
    ((cl:and (cl:>= (cl:length s) 6)
             (cl:string= "bytes" (cl:subseq s 0 5)))
     (%parse-bytes-fixed-type-cl s))

    ;; Unknown type
    (cl:t
     (Err (web3/types:AbiError (cl:format cl:nil "Unknown type: ~A" s))))))

(cl:defun %parse-uint-type-cl (s)
  "Parse uint<N> type"
  (cl:if (cl:string= s "uint")
         (Ok (web3/abi:AbiUint 256))
         (cl:let ((bits (cl:parse-integer (cl:subseq s 4) :junk-allowed cl:t)))
           (cl:if (cl:and bits (cl:> bits 0) (cl:<= bits 256)
                          (cl:zerop (cl:mod bits 8)))
                  (Ok (web3/abi:AbiUint bits))
                  (Err (web3/types:AbiError (cl:format cl:nil "Invalid uint size: ~A" s)))))))

(cl:defun %parse-int-type-cl (s)
  "Parse int<N> type"
  (cl:if (cl:string= s "int")
         (Ok (web3/abi:AbiInt 256))
         (cl:let ((bits (cl:parse-integer (cl:subseq s 3) :junk-allowed cl:t)))
           (cl:if (cl:and bits (cl:> bits 0) (cl:<= bits 256)
                          (cl:zerop (cl:mod bits 8)))
                  (Ok (web3/abi:AbiInt bits))
                  (Err (web3/types:AbiError (cl:format cl:nil "Invalid int size: ~A" s)))))))

(cl:defun %parse-bytes-fixed-type-cl (s)
  "Parse bytes<N> type (1-32)"
  (cl:let ((size (cl:parse-integer (cl:subseq s 5) :junk-allowed cl:t)))
    (cl:if (cl:and size (cl:>= size 1) (cl:<= size 32))
           (Ok (web3/abi:AbiBytesFixed size))
           (Err (web3/types:AbiError (cl:format cl:nil "Invalid bytes size: ~A" s))))))

(cl:defun %parse-array-type-cl (s)
  "Parse array type: T[] or T[N]"
  (cl:let* ((bracket-pos (cl:position #\[ s :from-end cl:t))
           (base-type (cl:subseq s 0 bracket-pos))
           (size-str (cl:subseq s (cl:1+ bracket-pos) (cl:1- (cl:length s)))))
    (cl:let ((base-result (%parse-type-string-cl base-type)))
      (cl:if (cl:not (%cl-result-ok? base-result))
             base-result
             (cl:let ((base-abi-type (%cl-unwrap-ok base-result)))
               (cl:if (cl:zerop (cl:length size-str))
                      ;; Dynamic array: T[]
                      (Ok (web3/abi:AbiArray base-abi-type))
                      ;; Fixed array: T[N]
                      (cl:let ((size (cl:parse-integer size-str :junk-allowed cl:t)))
                        (cl:if size
                               (Ok (web3/abi:AbiFixedArray base-abi-type size))
                               (Err (web3/types:AbiError
                                     (cl:format cl:nil "Invalid array size: ~A" size-str)))))))))))

(cl:defun %parse-tuple-type-cl (s)
  "Parse tuple type: (T1,T2,...)"
  (cl:if (cl:or (cl:< (cl:length s) 2)
                (cl:not (cl:char= (cl:char s 0) #\())
                (cl:not (cl:char= (cl:char s (cl:1- (cl:length s))) #\))))
         (Err (web3/types:AbiError "Invalid tuple syntax"))
         (cl:let* ((inner (cl:subseq s 1 (cl:1- (cl:length s))))
                  (components (%split-tuple-components-cl inner)))
           (cl:if (cl:null components)
                  (Ok (web3/abi:AbiTuple cl:nil))
                  (%parse-tuple-components-cl components cl:nil)))))

(cl:defun %split-tuple-components-cl (s)
  "Split tuple components by comma, respecting nested parens"
  (cl:if (cl:zerop (cl:length s))
         cl:nil
         (cl:let ((result cl:nil)
                  (current "")
                  (depth 0))
           (cl:loop :for c :across s
                    :do (cl:cond
                          ((cl:char= c #\()
                           (cl:incf depth)
                           (cl:setf current (cl:concatenate 'cl:string current (cl:string c))))
                          ((cl:char= c #\))
                           (cl:decf depth)
                           (cl:setf current (cl:concatenate 'cl:string current (cl:string c))))
                          ((cl:and (cl:char= c #\,) (cl:zerop depth))
                           (cl:push (cl:string-trim " " current) result)
                           (cl:setf current ""))
                          (cl:t
                           (cl:setf current (cl:concatenate 'cl:string current (cl:string c))))))
           (cl:when (cl:> (cl:length current) 0)
             (cl:push (cl:string-trim " " current) result))
           (cl:nreverse result))))

(cl:defun %parse-tuple-components-cl (components acc)
  "Parse each tuple component type"
  (cl:if (cl:null components)
         (Ok (web3/abi:AbiTuple (cl:nreverse acc)))
         (cl:let ((result (%parse-type-string-cl (cl:first components))))
           (cl:if (cl:not (%cl-result-ok? result))
                  result
                  (%parse-tuple-components-cl
                   (cl:rest components)
                   (cl:cons (%cl-unwrap-ok result) acc))))))

(cl:defun %cl-result-ok? (result)
  "Check if a Coalton Result is Ok (works at CL level)"
  ;; Check the type name directly since Coalton Ok is RESULT/OK
  (cl:and result
          (cl:typep result 'coalton-library/classes::result/ok)))

(cl:defun %cl-unwrap-ok (result)
  "Unwrap an Ok result at CL level (use only when you know it's Ok)"
  ;; Access the value slot of the Ok struct directly
  (cl:slot-value result 'coalton-library/classes::|_0|))


;;; =========================================================================
;;; Signature Building
;;; =========================================================================

(coalton-toplevel

  (declare build-function-signature (String -> (List AbiParam) -> String))
  (define (build-function-signature name inputs)
    "Build a function signature string like 'transfer(address,uint256)'"
    (lisp String (name inputs)
      (%build-signature name inputs)))

  (declare build-event-signature (String -> (List AbiParam) -> String))
  (define (build-event-signature name inputs)
    "Build an event signature string"
    (lisp String (name inputs)
      (%build-signature name inputs))))

(cl:defun %build-signature (name inputs)
  "Build signature: name(type1,type2,...)"
  (cl:format cl:nil "~A(~{~A~^,~})"
             name
             (cl:mapcar (cl:lambda (param)
                          (%abi-type-to-string
                           (coalton:coalton
                            (.param-type
                             (coalton:lisp AbiParam () param)))))
                        inputs)))

(cl:defun %abi-type-to-string (typ)
  "Convert AbiType back to Solidity type string"
  (coalton:coalton
   (match (coalton:lisp abi:AbiType () typ)
     ((abi:AbiUint bits)
      (coalton:lisp String (bits)
        (cl:format cl:nil "uint~A" bits)))
     ((abi:AbiInt bits)
      (coalton:lisp String (bits)
        (cl:format cl:nil "int~A" bits)))
     ((abi:AbiAddress)
      "address")
     ((abi:AbiBool)
      "bool")
     ((abi:AbiBytes)
      "bytes")
     ((abi:AbiBytesFixed n)
      (coalton:lisp String (n)
        (cl:format cl:nil "bytes~A" n)))
     ((abi:AbiString)
      "string")
     ((abi:AbiArray elem)
      (coalton:lisp String (elem)
        (cl:format cl:nil "~A[]" (%abi-type-to-string elem))))
     ((abi:AbiFixedArray elem size)
      (coalton:lisp String (elem size)
        (cl:format cl:nil "~A[~A]" (%abi-type-to-string elem) size)))
     ((abi:AbiTuple components)
      (coalton:lisp String (components)
        (cl:format cl:nil "(~{~A~^,~})"
                   (cl:mapcar #'%abi-type-to-string components)))))))

;;; =========================================================================
;;; JSON Parsing
;;; =========================================================================

(coalton-toplevel

  (declare parse-abi-json (String -> (types:Web3Result (List AbiItem))))
  (define (parse-abi-json json-str)
    "Parse a Solidity ABI JSON string into a list of ABI items"
    (lisp (types:Web3Result (List AbiItem)) (json-str)
      (cl:handler-case
          (%parse-abi-json-impl json-str)
        (cl:error (e)
          (Err (web3/types:AbiError
                (cl:format cl:nil "Failed to parse ABI JSON: ~A" e)))))))

  (declare parse-function-json (String -> (types:Web3Result ParsedFunction)))
  (define (parse-function-json json-str)
    "Parse a single function ABI JSON"
    (lisp (types:Web3Result ParsedFunction) (json-str)
      (cl:handler-case
          (cl:let ((parsed (cl-json:decode-json-from-string json-str)))
            (%parse-function-item parsed))
        (cl:error (e)
          (Err (web3/types:AbiError
                (cl:format cl:nil "Failed to parse function JSON: ~A" e)))))))

  (declare parse-event-json (String -> (types:Web3Result ParsedEvent)))
  (define (parse-event-json json-str)
    "Parse a single event ABI JSON"
    (lisp (types:Web3Result ParsedEvent) (json-str)
      (cl:handler-case
          (cl:let ((parsed (cl-json:decode-json-from-string json-str)))
            (%parse-event-item parsed))
        (cl:error (e)
          (Err (web3/types:AbiError
                (cl:format cl:nil "Failed to parse event JSON: ~A" e)))))))
)

;;; CL JSON parsing helpers - use tagged tuples for error handling

(cl:defun %parse-abi-json-impl (json-str)
  "Parse ABI JSON and return Coalton Result"
  (cl:let* ((parsed (cl-json:decode-json-from-string json-str))
            (items-or-err (%parse-abi-items-tagged parsed)))
    (cl:if (cl:eq (cl:car items-or-err) :ok)
           (Ok (cl:cdr items-or-err))
           (Err (web3/types:AbiError (cl:cdr items-or-err))))))

(cl:defun %parse-abi-items-tagged (items)
  "Parse list of ABI JSON items, returning (:ok . items) or (:err . message)"
  (cl:if (cl:null items)
         (cl:cons :ok cl:nil)
         (cl:let ((first-result (%parse-abi-item-tagged (cl:first items))))
           (cl:if (cl:eq (cl:car first-result) :err)
                  first-result
                  (cl:let ((rest-result (%parse-abi-items-tagged (cl:rest items))))
                    (cl:if (cl:eq (cl:car rest-result) :err)
                           rest-result
                           (cl:cons :ok (cl:cons (cl:cdr first-result)
                                                 (cl:cdr rest-result)))))))))

(cl:defun %parse-abi-item-tagged (item)
  "Parse a single ABI JSON item, returning (:ok . AbiItem) or (:err . message)"
  (cl:let ((item-type (cl:cdr (cl:assoc :type item))))
    (cl:cond
      ((cl:string-equal item-type "function")
       (cl:let ((fn-result (%parse-function-item-tagged item)))
         (cl:if (cl:eq (cl:car fn-result) :err)
                fn-result
                (cl:cons :ok (AbiFunction (cl:cdr fn-result))))))

      ((cl:string-equal item-type "event")
       (cl:let ((ev-result (%parse-event-item-tagged item)))
         (cl:if (cl:eq (cl:car ev-result) :err)
                ev-result
                (cl:cons :ok (AbiEvent (cl:cdr ev-result))))))

      ((cl:string-equal item-type "constructor")
       (cl:let ((inputs-result (%parse-params-tagged (cl:cdr (cl:assoc :inputs item)) cl:nil)))
         (cl:if (cl:eq (cl:car inputs-result) :err)
                inputs-result
                (cl:cons :ok (AbiConstructor (cl:cdr inputs-result))))))

      ((cl:string-equal item-type "fallback")
       (cl:cons :ok AbiFallback))

      ((cl:string-equal item-type "receive")
       (cl:cons :ok AbiReceive))

      ((cl:string-equal item-type "error")
       (cl:let* ((name (cl:or (cl:cdr (cl:assoc :name item)) ""))
                (inputs-result (%parse-params-tagged (cl:cdr (cl:assoc :inputs item)) cl:nil)))
         (cl:if (cl:eq (cl:car inputs-result) :err)
                inputs-result
                (cl:cons :ok (AbiError name (cl:cdr inputs-result))))))

      (cl:t
       (cl:cons :ok AbiFallback)))))

(cl:defun %parse-function-item-tagged (item)
  "Parse a function ABI item, returning (:ok . ParsedFunction) or (:err . message)"
  (cl:let* ((name (cl:or (cl:cdr (cl:assoc :name item)) ""))
           (inputs-result (%parse-params-tagged (cl:cdr (cl:assoc :inputs item)) cl:nil))
           (outputs-result (%parse-params-tagged (cl:cdr (cl:assoc :outputs item)) cl:nil))
           (state-mutability (cl:or (cl:cdr (cl:assoc :state-mutability item)) "nonpayable")))
    (cl:if (cl:eq (cl:car inputs-result) :err)
           inputs-result
           (cl:if (cl:eq (cl:car outputs-result) :err)
                  outputs-result
                  (cl:let* ((inputs (cl:cdr inputs-result))
                           (outputs (cl:cdr outputs-result))
                           (sig (%build-signature name inputs))
                           (selector (coalton:coalton
                                      (abi:function-selector
                                       (coalton:lisp String () sig)))))
                    (cl:cons :ok (ParsedFunction name inputs outputs selector state-mutability)))))))

(cl:defun %parse-function-item (item)
  "Parse a function ABI item, returning Coalton Result"
  (cl:let ((result (%parse-function-item-tagged item)))
    (cl:if (cl:eq (cl:car result) :err)
           (Err (web3/types:AbiError (cl:cdr result)))
           (Ok (cl:cdr result)))))

(cl:defun %parse-event-item-tagged (item)
  "Parse an event ABI item, returning (:ok . ParsedEvent) or (:err . message)"
  (cl:let* ((name (cl:or (cl:cdr (cl:assoc :name item)) ""))
           (inputs-result (%parse-params-tagged (cl:cdr (cl:assoc :inputs item)) cl:t))
           (anonymous (cl:eq (cl:cdr (cl:assoc :anonymous item)) cl:t)))
    (cl:if (cl:eq (cl:car inputs-result) :err)
           inputs-result
           (cl:let* ((inputs (cl:cdr inputs-result))
                    (sig (%build-signature name inputs))
                    (topic (coalton:coalton
                            (abi:event-topic
                             (coalton:lisp String () sig)))))
             (cl:cons :ok (ParsedEvent name inputs topic
                                       (cl:if anonymous coalton:True coalton:False)))))))

(cl:defun %parse-event-item (item)
  "Parse an event ABI item, returning Coalton Result"
  (cl:let ((result (%parse-event-item-tagged item)))
    (cl:if (cl:eq (cl:car result) :err)
           (Err (web3/types:AbiError (cl:cdr result)))
           (Ok (cl:cdr result)))))

(cl:defun %parse-params-tagged (params-list is-event)
  "Parse a list of ABI parameters, returning (:ok . list) or (:err . message)"
  (cl:if (cl:null params-list)
         (cl:cons :ok cl:nil)
         (cl:let* ((param (cl:first params-list))
                  (name (cl:or (cl:cdr (cl:assoc :name param)) ""))
                  (type-str (cl:cdr (cl:assoc :type param)))
                  (indexed (cl:and is-event
                                   (cl:eq (cl:cdr (cl:assoc :indexed param)) cl:t)))
                  (type-result (%parse-type-coalton type-str)))
           (cl:if (cl:eq (cl:car type-result) :err)
                  type-result
                  (cl:let ((rest-result (%parse-params-tagged (cl:rest params-list) is-event)))
                    (cl:if (cl:eq (cl:car rest-result) :err)
                           rest-result
                           (cl:cons :ok
                                    (cl:cons (AbiParam name
                                                       (cl:cdr type-result)
                                                       (cl:if indexed coalton:True coalton:False))
                                             (cl:cdr rest-result)))))))))

(cl:defun %parse-type-coalton (type-str)
  "Parse a type string using CL parser, returning (:ok . AbiType) or (:err . message)"
  ;; Directly call the CL parser function instead of going through Coalton
  (cl:let ((result (%parse-type-string-cl (cl:string-trim " " type-str))))
    (cl:if (%cl-result-ok? result)
           (cl:cons :ok (%cl-unwrap-ok result))
           (cl:cons :err (cl:format cl:nil "Failed to parse type: ~A" type-str)))))


;;; =========================================================================
;;; Calldata Encoding/Decoding
;;; =========================================================================

(coalton-toplevel

  ;; Helper functions first (needed by decode-event-log)
  (declare %decode-indexed-params ((List AbiParam) -> (List types:Bytes) ->
                                   (types:Web3Result (List abi:AbiValue))))
  (define (%decode-indexed-params params topics)
    "Decode indexed parameters from topics"
    (match params
      ((Nil) (Ok Nil))
      ((Cons param rest-params)
       (match topics
         ((Nil) (Err (types:AbiError "Not enough topics for indexed params")))
         ((Cons topic rest-topics)
          ;; Indexed params are always 32 bytes (or hashes for dynamic types)
          (match (abi:abi-decode (make-list (.param-type param)) topic)
            ((Err e) (Err e))
            ((Ok (Cons val _))
             (match (%decode-indexed-params rest-params rest-topics)
               ((Err e) (Err e))
               ((Ok rest-vals)
                (Ok (Cons val rest-vals)))))
            (_ (Err (types:AbiError "Unexpected empty decode result")))))))))

  (declare %interleave-params ((List AbiParam) -> (List abi:AbiValue) -> (List abi:AbiValue) -> (List abi:AbiValue)))
  (define (%interleave-params params indexed non-indexed)
    "Put decoded values back in original parameter order"
    (match params
      ((Nil) Nil)
      ((Cons param rest-params)
       (if (.param-indexed param)
           ;; Take from indexed
           (match indexed
             ((Nil) Nil)  ; Shouldn't happen
             ((Cons val rest-indexed)
              (Cons val (%interleave-params rest-params rest-indexed non-indexed))))
           ;; Take from non-indexed
           (match non-indexed
             ((Nil) Nil)  ; Shouldn't happen
             ((Cons val rest-non-indexed)
              (Cons val (%interleave-params rest-params indexed rest-non-indexed))))))))

  ;; Main encoding/decoding functions
  (declare encode-function-call (ParsedFunction -> (List abi:AbiValue) -> types:Bytes))
  (define (encode-function-call fn args)
    "Encode a function call with the given arguments"
    (abi:abi-encode-with-selector (.fn-selector fn) args))

  (declare decode-function-output (ParsedFunction -> types:Bytes -> (types:Web3Result (List abi:AbiValue))))
  (define (decode-function-output fn data)
    "Decode function return values"
    (let ((output-types (map (fn (p) (.param-type p)) (.fn-outputs fn))))
      (abi:abi-decode output-types data)))

  (declare decode-event-log (ParsedEvent -> (List types:Bytes) -> types:Bytes ->
                             (types:Web3Result (List abi:AbiValue))))
  (define (decode-event-log event topics data)
    "Decode an event log. Topics contains indexed params, data contains non-indexed."
    ;; Separate indexed and non-indexed inputs
    (let ((indexed-inputs (list:filter (fn (p) (.param-indexed p)) (.event-inputs event)))
          (non-indexed-inputs (list:filter (fn (p) (not (.param-indexed p))) (.event-inputs event))))
      ;; Decode non-indexed from data
      (let ((non-indexed-types (map (fn (p) (.param-type p)) non-indexed-inputs)))
        (match (abi:abi-decode non-indexed-types data)
          ((Err e) (Err e))
          ((Ok decoded-data)
           ;; Decode indexed from topics (skip topic0 which is signature)
           (match (%decode-indexed-params indexed-inputs (list:drop 1 topics))
             ((Err e) (Err e))
             ((Ok decoded-indexed)
              ;; Interleave back in original order
              (Ok (%interleave-params (.event-inputs event)
                                       decoded-indexed
                                       decoded-data))))))))))

;;; =========================================================================
;;; Explicit Exports (ensure all Coalton symbols are exported)
;;; =========================================================================

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:export '(AbiItem
               AbiFunction
               AbiEvent
               AbiConstructor
               AbiFallback
               AbiReceive
               AbiError
               AbiParam
               .param-name
               .param-type
               .param-indexed
               make-abi-param
               ParsedFunction
               .fn-name
               .fn-inputs
               .fn-outputs
               .fn-selector
               .fn-state-mutability
               ParsedEvent
               .event-name
               .event-inputs
               .event-topic
               .event-anonymous
               parse-abi-type
               type-string-to-abi-type
               parse-abi-json
               parse-function-json
               parse-event-json
               encode-function-call
               decode-function-output
               decode-event-log
               build-function-signature
               build-event-signature)
             (cl:find-package '#:web3/abi-parser)))
