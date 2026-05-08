;;;; web3/easy - implementation
;;;;
;;;; This file is intentionally pure CL. Coalton expressions are wrapped in
;;;; `coalton:coalton` and CL values are forwarded into them via
;;;; `coalton:lisp <type> () expr`. Anything that needs Coalton control flow
;;;; (Some/None, struct construction) is delegated to `web3/easy-bridge`.

(cl:in-package #:web3/easy)

;;; =========================================================================
;;; Conditions
;;; =========================================================================

(define-condition web3-error (simple-error)
  ((message :initarg :message :reader web3-error-message))
  (:documentation "Signaled when an underlying Coalton operation returns Err.")
  (:report (lambda (c stream)
             (format stream "~A" (web3-error-message c)))))

(defun %fail (fmt &rest args)
  (error 'web3-error :message (apply #'format nil fmt args)))

;;; =========================================================================
;;; Result + Web3Error unwrapping
;;; =========================================================================

(defun %unwrap-err (result)
  "Extract the Web3Error inside an Err result."
  (web3/types:%unwrap-err result))

(defun %web3-error->string (err)
  "Pull the message string out of a Web3Error variant.
   The variants are defined in `web3/types`, so their payload slot is
   `web3/types::|_0|`, not the `coalton-library/classes::|_0|` used for
   Ok/Err. Fall back to printing the object if neither slot is present."
  (cond
    ((slot-exists-p err 'web3/types::|_0|)
     (slot-value err 'web3/types::|_0|))
    ((slot-exists-p err 'coalton-library/classes::|_0|)
     (slot-value err 'coalton-library/classes::|_0|))
    (t (princ-to-string err))))

(defmacro %check ((var coalton-form) &body body)
  "Evaluate COALTON-FORM (a (coalton:coalton ...) returning Web3Result T),
   bind VAR to its Ok payload, and execute BODY. Signal `web3-error` on Err."
  (let ((res (gensym "RES")))
    `(let ((,res ,coalton-form))
       (cond
         ((web3/types:%result-ok-p ,res)
          (let ((,var (web3/types:%unwrap-ok ,res)))
            ,@body))
         (t
          (%fail "~A" (%web3-error->string (%unwrap-err ,res))))))))

;;; =========================================================================
;;; Internal builders (CL <-> Coalton conversion)
;;; =========================================================================

(defun %provider (url)
  "Build a Coalton HttpProvider from a CL URL string."
  (coalton:coalton
    (web3/provider:make-http-provider
     (coalton:lisp coalton:String () url))))

(defun %parse-address (hex)
  "Parse a CL hex string into a Coalton Address, signaling on bad input."
  (%check (addr (coalton:coalton
                  (web3/address:address-from-hex
                   (coalton:lisp coalton:String () hex))))
    addr))

(defun %u256-to-integer (u256)
  "Coalton U256 -> CL integer."
  (coalton:coalton
    (web3/types:u256-to-integer
     (coalton:lisp web3/types:U256 () u256))))

(defun %integer-to-u256 (n)
  "CL non-negative integer -> Coalton U256."
  (when (minusp n)
    (%fail "web3:integer-to-u256: value must be non-negative, got ~A" n))
  (coalton:coalton
    (web3/types:u256-from-integer
     (coalton:lisp coalton:Integer () n))))

(defun %address-to-hex (address)
  "Coalton Address -> EIP-55 checksum hex string."
  (coalton:coalton
    (web3/address:address-to-checksum-hex
     (coalton:lisp web3/address:Address () address))))

(defun %bytes-to-hex (bytes)
  "Coalton Bytes -> 0x-prefixed hex string."
  (coalton:coalton
    (web3/types:hex-encode-prefixed
     (coalton:lisp web3/types:Bytes () bytes))))

(defun %hex-to-bytes (hex)
  "0x-prefixed hex string -> Coalton Bytes (signals on bad hex)."
  (%check (b (coalton:coalton
               (web3/types:hex-decode
                (coalton:lisp coalton:String () hex))))
    b))

(defun %from-option (from-hex)
  "Build an (Optional Address) value from an optional CL hex string."
  (if from-hex
      (let ((a (%parse-address from-hex)))
        (coalton:coalton
          (coalton-prelude:Some
           (coalton:lisp web3/address:Address () a))))
      (coalton:coalton coalton-prelude:None)))

;;; =========================================================================
;;; Retry + fallback
;;; =========================================================================
;;;
;;; A small utility that wraps any of the URL-based reads above with:
;;;   - per-URL retries with exponential backoff for transient errors
;;;   - a fallback chain across multiple URLs (try primary; on exhaustion,
;;;     try the next; ...)
;;;
;;; Usage:
;;;   (web3:with-fallback '("https://primary" "https://backup")
;;;     (lambda (url) (web3:get-balance url addr-hex)))
;;;
;;; Designed as a wrapper rather than baked into every eth-* function so we
;;; don't duplicate the 20+ method surface and so users can compose retry
;;; with anything (defcontract reads, custom flows, multi-call sequences).

(defparameter *default-max-retries-per-url* 3
  "Default number of attempts per URL inside `with-fallback`.")

(defparameter *default-backoff-ms* 250
  "Default initial backoff between retries (doubles up to ~5s).")

(defun %transient-web3-error? (condition)
  "Heuristic: is this a transient error worth retrying? Defaults to true
   for HTTP errors, timeouts, connection refusals and rate-limit hints —
   anything that looks like \"the server may answer if we ask again.\"
   Permanent errors like \"Invalid hex string\" will NOT match."
  (let ((msg (web3-error-message condition)))
    (or (search "HTTP" msg)
        (search "timeout" msg :test #'char-equal)
        (search "connection" msg :test #'char-equal)
        (search "refused"   msg :test #'char-equal)
        (search "rate"      msg :test #'char-equal)
        (search "429"       msg)
        (search "503"       msg)
        (search "502"       msg)
        (search "504"       msg))))

(defun with-fallback (urls thunk
                      &key
                        (max-retries-per-url *default-max-retries-per-url*)
                        (initial-backoff-ms *default-backoff-ms*)
                        (max-backoff-ms 5000)
                        (retryable-p #'%transient-web3-error?))
  "Call THUNK with each URL in URLS, retrying transient errors per-URL.

   Returns the first successful result. Signals `web3-error` only when every
   URL has been exhausted; the message names the last underlying error so
   you can tell mass failure from per-call mistakes.

   :retryable-p predicate decides which errors to retry vs surface
   immediately. The default treats HTTP errors / timeouts / 429/5xx hints
   as transient and other errors (e.g. malformed input) as permanent."
  (when (null urls)
    (%fail "with-fallback: empty URL list"))
  (let ((last-error nil))
    (dolist (url urls)
      (let ((backoff (/ initial-backoff-ms 1000.0))
            (max-backoff (/ max-backoff-ms 1000.0))
            (attempt 0))
        (loop
          (handler-case (return-from with-fallback (funcall thunk url))
            (web3-error (c)
              (setf last-error c)
              (incf attempt)
              (cond
                ((not (funcall retryable-p c))
                 (error c))
                ((>= attempt max-retries-per-url)
                 (return))                           ; try next URL
                (t (sleep backoff)
                   (setf backoff (min (* backoff 2) max-backoff)))))))))
    (%fail "with-fallback: all URLs exhausted (~D); last error: ~A"
           (length urls)
           (if last-error (web3-error-message last-error) "<none>"))))

;;; =========================================================================
;;; Provider reads
;;; =========================================================================

(defun get-block-number (url)
  "Latest block number on URL, as an integer."
  (let ((p (%provider url)))
    (%check (n (coalton:coalton
                 (web3/provider:eth-block-number
                  (coalton:lisp web3/provider:HttpProvider () p))))
      n)))

(defun chain-id (url)
  "Chain ID on URL, as an integer."
  (let ((p (%provider url)))
    (%check (n (coalton:coalton
                 (web3/provider:eth-chain-id
                  (coalton:lisp web3/provider:HttpProvider () p))))
      n)))

(defun get-balance (url address-hex)
  "Balance of ADDRESS-HEX on URL, in wei (integer)."
  (let ((p (%provider url))
        (a (%parse-address address-hex)))
    (%check (u (coalton:coalton
                 (web3/provider:eth-get-balance
                  (coalton:lisp web3/provider:HttpProvider () p)
                  (coalton:lisp web3/address:Address () a))))
      (%u256-to-integer u))))

(defun get-transaction-count (url address-hex)
  "Transaction count (nonce) of ADDRESS-HEX on URL, as an integer."
  (let ((p (%provider url))
        (a (%parse-address address-hex)))
    (%check (n (coalton:coalton
                 (web3/provider:eth-get-transaction-count
                  (coalton:lisp web3/provider:HttpProvider () p)
                  (coalton:lisp web3/address:Address () a))))
      n)))

(defun gas-price (url)
  "Current gas price on URL, in wei (integer)."
  (let ((p (%provider url)))
    (%check (u (coalton:coalton
                 (web3/provider:eth-gas-price
                  (coalton:lisp web3/provider:HttpProvider () p))))
      (%u256-to-integer u))))

(defun max-priority-fee (url)
  "Suggested max priority fee per gas on URL, in wei."
  (let ((p (%provider url)))
    (%check (u (coalton:coalton
                 (web3/provider:eth-max-priority-fee-per-gas
                  (coalton:lisp web3/provider:HttpProvider () p))))
      (%u256-to-integer u))))

(defun get-code (url address-hex)
  "Bytecode at ADDRESS-HEX on URL, as a 0x-prefixed hex string."
  (let ((p (%provider url))
        (a (%parse-address address-hex)))
    (%check (b (coalton:coalton
                 (web3/provider:eth-get-code
                  (coalton:lisp web3/provider:HttpProvider () p)
                  (coalton:lisp web3/address:Address () a))))
      (%bytes-to-hex b))))

(defun get-storage-at (url address-hex slot-integer)
  "Read storage SLOT-INTEGER at ADDRESS-HEX, returning 0x-prefixed hex bytes."
  (let ((p (%provider url))
        (a (%parse-address address-hex))
        (s (%integer-to-u256 slot-integer)))
    (%check (b (coalton:coalton
                 (web3/provider:eth-get-storage-at
                  (coalton:lisp web3/provider:HttpProvider () p)
                  (coalton:lisp web3/address:Address () a)
                  (coalton:lisp web3/types:U256 () s))))
      (%bytes-to-hex b))))

(defun eth-call (url to-hex data-hex &key from-hex)
  "Read-only call to TO-HEX with DATA-HEX (0x-prefixed). FROM-HEX is optional.
   Returns the response as a 0x-prefixed hex string."
  (let* ((p    (%provider url))
         (to   (%parse-address to-hex))
         (data (%hex-to-bytes data-hex))
         (from (%from-option from-hex)))
    (%check (b (coalton:coalton
                 (web3/easy-bridge:eth-call
                  (coalton:lisp web3/provider:HttpProvider () p)
                  (coalton:lisp (coalton-prelude:Optional web3/address:Address) () from)
                  (coalton:lisp web3/address:Address () to)
                  (coalton:lisp web3/types:Bytes () data))))
      (%bytes-to-hex b))))

(defun get-receipt (url tx-hash-hex)
  "Receipt for TX-HASH-HEX on URL, as a raw JSON string. Returns NIL when
   the receipt is not yet available."
  (let ((p (%provider url)))
    (%check (opt (coalton:coalton
                   (web3/provider:eth-get-transaction-receipt
                    (coalton:lisp web3/provider:HttpProvider () p)
                    (coalton:lisp coalton:String () tx-hash-hex))))
      (when (web3/types:%is-some-p opt)
        (web3/types:%unwrap-some opt)))))

(defun wait-for-receipt (url tx-hash-hex &key (max-attempts 60) (poll-interval-ms 2000))
  "Block until a receipt for TX-HASH-HEX is available or MAX-ATTEMPTS elapse.
   Returns the receipt JSON string."
  (let ((p (%provider url)))
    (%check (raw (coalton:coalton
                   (web3/provider:wait-for-transaction-receipt
                    (coalton:lisp web3/provider:HttpProvider () p)
                    (coalton:lisp coalton:String () tx-hash-hex)
                    (coalton:lisp coalton:UFix () max-attempts)
                    (coalton:lisp coalton:UFix () poll-interval-ms))))
      raw)))

;;; =========================================================================
;;; Units
;;; =========================================================================

(defun parse-ether (str)
  "Parse a decimal ether string (e.g. \"1.5\") to wei (integer)."
  (%check (u (coalton:coalton
               (web3/units:parse-ether
                (coalton:lisp coalton:String () str))))
    (%u256-to-integer u)))

(defun format-ether (wei-integer)
  "Format an integer wei value as a decimal ether string."
  (let ((u (%integer-to-u256 wei-integer)))
    (coalton:coalton
      (web3/units:format-ether
       (coalton:lisp web3/types:U256 () u)))))

(defun parse-gwei (str)
  "Parse a decimal gwei string to wei (integer)."
  (%check (u (coalton:coalton
               (web3/units:parse-gwei
                (coalton:lisp coalton:String () str))))
    (%u256-to-integer u)))

(defun format-gwei (wei-integer)
  "Format an integer wei value as a decimal gwei string."
  (let ((u (%integer-to-u256 wei-integer)))
    (coalton:coalton
      (web3/units:format-gwei
       (coalton:lisp web3/types:U256 () u)))))

(defun parse-units (str decimals)
  "Parse STR into the smallest unit using DECIMALS (e.g. 6 for USDC)."
  (%check (u (coalton:coalton
               (web3/units:parse-units
                (coalton:lisp coalton:String () str)
                (coalton:lisp coalton:UFix () decimals))))
    (%u256-to-integer u)))

(defun format-units (smallest-integer decimals)
  "Format SMALLEST-INTEGER as a decimal string with DECIMALS places."
  (let ((u (%integer-to-u256 smallest-integer)))
    (coalton:coalton
      (web3/units:format-units
       (coalton:lisp web3/types:U256 () u)
       (coalton:lisp coalton:UFix () decimals)))))

;;; =========================================================================
;;; Address + hashing
;;; =========================================================================

(defun checksum-address (hex)
  "Return the EIP-55 checksum form of HEX. Validates the address shape."
  (let ((a (%parse-address hex)))
    (%address-to-hex a)))

(defun address-zero ()
  "The zero address as an EIP-55-formatted hex string."
  (coalton:coalton
    (web3/address:address-to-checksum-hex
     web3/address:address-zero)))

(defun keccak256 (hex)
  "keccak256 of the bytes given as 0x-prefixed hex. Returns 0x-prefixed hex."
  (let ((b (%hex-to-bytes hex)))
    (%bytes-to-hex
     (coalton:coalton
       (web3/crypto:keccak256
        (coalton:lisp web3/types:Bytes () b))))))

;;; =========================================================================
;;; ERC-20 reads
;;; =========================================================================

(defun erc20-name (url contract-hex)
  (let ((p (%provider url))
        (c (%parse-address contract-hex)))
    (%check (s (coalton:coalton
                 (web3/erc20:erc20-name
                  (coalton:lisp web3/provider:HttpProvider () p)
                  (coalton:lisp web3/address:Address () c))))
      s)))

(defun erc20-symbol (url contract-hex)
  (let ((p (%provider url))
        (c (%parse-address contract-hex)))
    (%check (s (coalton:coalton
                 (web3/erc20:erc20-symbol
                  (coalton:lisp web3/provider:HttpProvider () p)
                  (coalton:lisp web3/address:Address () c))))
      s)))

(defun erc20-decimals (url contract-hex)
  (let ((p (%provider url))
        (c (%parse-address contract-hex)))
    (%check (d (coalton:coalton
                 (web3/erc20:erc20-decimals
                  (coalton:lisp web3/provider:HttpProvider () p)
                  (coalton:lisp web3/address:Address () c))))
      d)))

(defun erc20-total-supply (url contract-hex)
  (let ((p (%provider url))
        (c (%parse-address contract-hex)))
    (%check (u (coalton:coalton
                 (web3/erc20:erc20-total-supply
                  (coalton:lisp web3/provider:HttpProvider () p)
                  (coalton:lisp web3/address:Address () c))))
      (%u256-to-integer u))))

(defun erc20-balance (url contract-hex holder-hex)
  (let ((p (%provider url))
        (c (%parse-address contract-hex))
        (h (%parse-address holder-hex)))
    (%check (u (coalton:coalton
                 (web3/erc20:erc20-balance-of
                  (coalton:lisp web3/provider:HttpProvider () p)
                  (coalton:lisp web3/address:Address () c)
                  (coalton:lisp web3/address:Address () h))))
      (%u256-to-integer u))))

(defun erc20-allowance (url contract-hex owner-hex spender-hex)
  (let ((p (%provider url))
        (c (%parse-address contract-hex))
        (o (%parse-address owner-hex))
        (s (%parse-address spender-hex)))
    (%check (u (coalton:coalton
                 (web3/erc20:erc20-allowance
                  (coalton:lisp web3/provider:HttpProvider () p)
                  (coalton:lisp web3/address:Address () c)
                  (coalton:lisp web3/address:Address () o)
                  (coalton:lisp web3/address:Address () s))))
      (%u256-to-integer u))))

;;; =========================================================================
;;; Block reads (parsed)
;;; =========================================================================

(defun %block-tag-to-string (tag)
  "Normalize a block tag to its JSON-RPC string form.
   Accepts: integers (turned into 0x... hex), keywords (:latest :earliest
   :pending :finalized :safe), or already-formatted strings."
  (etypecase tag
    (integer
     (when (minusp tag)
       (%fail "block tag must be non-negative, got ~A" tag))
     (format nil "0x~X" tag))
    (keyword
     (ecase tag
       (:latest    "latest")
       (:earliest  "earliest")
       (:pending   "pending")
       (:finalized "finalized")
       (:safe      "safe")))
    (string tag)))

(defun get-block (url tag &key (full-transactions nil))
  "Fetch a block and return a CL plist:
     (:number :hash :parent-hash :timestamp :miner :gas-limit :gas-used
      :base-fee :transactions-count :size)
   Returns NIL if the block does not exist.

   TAG can be an integer block number, one of (:latest :earliest :pending
   :finalized :safe), or a 0x-prefixed hex string."
  (let* ((tag-str (%block-tag-to-string tag))
         (provider (%provider url))
         (full-bool (and full-transactions cl:t)))
    (let ((opt-json
            (%check (o (coalton:coalton
                         (web3/easy-bridge:fetch-block-json
                          (coalton:lisp web3/provider:HttpProvider () provider)
                          (coalton:lisp coalton:String () tag-str)
                          (coalton:lisp coalton:Boolean () full-bool))))
              o)))
      (when (web3/types:%is-some-p opt-json)
        (let* ((json-str (web3/types:%unwrap-some opt-json))
               (opt-block
                 (%check (o (coalton:coalton
                              (web3/easy-bridge:parse-block-from-json
                               (coalton:lisp coalton:String () json-str)
                               (coalton:lisp coalton:Boolean () full-bool))))
                   o)))
          (when (web3/types:%is-some-p opt-block)
            (let ((blk (web3/types:%unwrap-some opt-block)))
              (list
               :number       (coalton:coalton
                               (web3/easy-bridge:block-number-of
                                (coalton:lisp web3/block:Block () blk)))
               :hash         (coalton:coalton
                               (web3/easy-bridge:block-hash-hex
                                (coalton:lisp web3/block:Block () blk)))
               :parent-hash  (coalton:coalton
                               (web3/easy-bridge:block-parent-hash-hex
                                (coalton:lisp web3/block:Block () blk)))
               :timestamp    (coalton:coalton
                               (web3/easy-bridge:block-timestamp-of
                                (coalton:lisp web3/block:Block () blk)))
               :miner        (coalton:coalton
                               (web3/easy-bridge:block-miner-hex
                                (coalton:lisp web3/block:Block () blk)))
               :gas-limit    (coalton:coalton
                               (web3/easy-bridge:block-gas-limit-of
                                (coalton:lisp web3/block:Block () blk)))
               :gas-used     (coalton:coalton
                               (web3/easy-bridge:block-gas-used-of
                                (coalton:lisp web3/block:Block () blk)))
               :base-fee     (let ((opt (coalton:coalton
                                          (web3/easy-bridge:block-base-fee-of
                                           (coalton:lisp web3/block:Block () blk)))))
                               (when (web3/types:%is-some-p opt)
                                 (web3/types:%unwrap-some opt)))
               :transactions-count
                             (coalton:coalton
                               (web3/easy-bridge:block-tx-count-of
                                (coalton:lisp web3/block:Block () blk)))
               :size         (coalton:coalton
                               (web3/easy-bridge:block-size-of
                                (coalton:lisp web3/block:Block () blk)))))))))))

;;; =========================================================================
;;; Multicall (Multicall3 aggregate3)
;;; =========================================================================

(defparameter *multicall3-address*
  "0xcA11bde05977b3631167028862bE2a173976CA11"
  "Canonical Multicall3 address. Same on most EVM chains (deployed via CREATE2).
   Override per-call with the :address keyword if your chain uses a different one.")

(defun multicall (url calls
                  &key (allow-failure t)
                       (address *multicall3-address*))
  "Batch read calls through Multicall3's aggregate3.

   CALLS is a list of plists, each shaped like:
     (:to ADDRESS-HEX :data CALLDATA-HEX [:allow-failure BOOL])

   ALLOW-FAILURE is the default for any call that doesn't override it.
   When true, an individual call's revert returns (:success nil :data ...)
   without aborting the batch.

   Returns a list of plists in the same order as CALLS, each shaped:
     (:success BOOL :data RETURN-DATA-HEX)

   Each call still goes out as one HTTP request — the batching is on-chain."
  (let* ((to    (%parse-address address))
         (call3-list
           ;; Build the Coalton Call3 list element-by-element. Coalton's List
           ;; is :repr :native cl:list, so a CL list of Coalton Call3 structs
           ;; is directly usable as `(List Call3)`.
           (mapcar
            (lambda (c)
              (let* ((target (%parse-address (getf c :to)))
                     (data   (%hex-to-bytes (getf c :data)))
                     (af     (if (member :allow-failure c)
                                 (and (getf c :allow-failure) t)
                                 allow-failure)))
                ;; Coalton Boolean is :repr :native cl:t — its True is just
                ;; CL's t and False is CL's nil. Pass them through directly
                ;; instead of routing through the quoted symbols.
                (coalton:coalton
                  (web3/easy-bridge:make-call3
                   (coalton:lisp web3/address:Address () target)
                   (coalton:lisp coalton:Boolean ()
                     (if af cl:t cl:nil))
                   (coalton:lisp web3/types:Bytes () data)))))
            calls))
         (calldata
           (coalton:coalton
             (web3/easy-bridge:encode-aggregate3
              (coalton:lisp (coalton:List web3/multicall:Call3) () call3-list))))
         (raw-hex
           (eth-call url
                     (%address-to-hex to)
                     (%bytes-to-hex calldata)))
         (raw-bytes (%hex-to-bytes raw-hex))
         (decoded
           (%check (d (coalton:coalton
                        (web3/easy-bridge:decode-aggregate3-response
                         (coalton:lisp web3/types:Bytes () raw-bytes))))
             d)))
    ;; decoded is a CL list of (Tuple Boolean Bytes).
    (mapcar (lambda (tup)
              (let ((success (web3/types:%tuple-0 tup))
                    (data    (web3/types:%tuple-1 tup)))
                (list :success (and success t)
                      :data    (%bytes-to-hex data))))
            decoded)))

;;; =========================================================================
;;; Wallet (CL-side handle)
;;; =========================================================================

(defstruct wallet
  "A CL-side wallet handle storing the URL and the underlying Coalton wallet."
  url
  inner)  ; web3/wallet:Wallet

(defun make-wallet-from-hex (private-key-hex url)
  "Build a wallet from a 0x-prefixed PRIVATE-KEY-HEX bound to a JSON-RPC URL."
  (let ((pk (%hex-to-bytes private-key-hex))
        (p  (%provider url)))
    (let ((w (coalton:coalton
               (web3/wallet:wallet-with-provider
                (coalton:lisp web3/types:Bytes () pk)
                (coalton:lisp web3/provider:HttpProvider () p)))))
      (make-wallet :url url :inner w))))

(defun %wallet-inner (w)
  (etypecase w
    (wallet (wallet-inner w))))

(defun wallet-address (w)
  "EIP-55 checksum address of wallet W."
  (let ((inner (%wallet-inner w)))
    (%check (a (coalton:coalton
                 (web3/wallet:wallet-address
                  (coalton:lisp web3/wallet:Wallet () inner))))
      (%address-to-hex a))))

(defun wallet-balance (w)
  "Balance of wallet W's address in wei (integer)."
  (let ((inner (%wallet-inner w)))
    (%check (u (coalton:coalton
                 (web3/wallet:wallet-get-balance
                  (coalton:lisp web3/wallet:Wallet () inner))))
      (%u256-to-integer u))))

(defun wallet-nonce (w)
  "Current nonce (transaction count) of wallet W."
  (let ((inner (%wallet-inner w)))
    (%check (n (coalton:coalton
                 (web3/wallet:wallet-get-nonce
                  (coalton:lisp web3/wallet:Wallet () inner))))
      n)))

(defun wallet-send-eth (w to-hex value-wei
                        &key chain-id
                             (gas-limit 21000)
                             max-fee-per-gas
                             max-priority-fee-per-gas
                             nonce)
  "Send VALUE-WEI from W to TO-HEX as a plain ETH transfer.

   Defaults are populated from the wallet's RPC endpoint when omitted:
     :chain-id                 -> eth_chainId
     :nonce                    -> eth_getTransactionCount(latest)
     :max-priority-fee-per-gas -> eth_maxPriorityFeePerGas
     :max-fee-per-gas          -> 2*gasPrice + maxPriorityFee
     :gas-limit                -> 21000 (plain transfer)

   Returns the resulting transaction hash."
  (let* ((inner (%wallet-inner w))
         (url   (wallet-url w))
         (to    (%parse-address to-hex))
         (value (%integer-to-u256 value-wei))
         (resolved-chain
           (or chain-id (web3/easy:chain-id url)))
         (resolved-nonce
           (or nonce (wallet-nonce w)))
         (resolved-priority
           (or max-priority-fee-per-gas (max-priority-fee url)))
         (resolved-max-fee
           (or max-fee-per-gas
               (+ (* 2 (gas-price url)) resolved-priority)))
         (priority-u (%integer-to-u256 resolved-priority))
         (max-fee-u  (%integer-to-u256 resolved-max-fee)))
    (let ((tx (coalton:coalton
                (web3/easy-bridge:make-eip1559-eth-transfer
                 (coalton:lisp coalton:U64 () resolved-chain)
                 (coalton:lisp coalton:U64 () resolved-nonce)
                 (coalton:lisp web3/types:U256 () priority-u)
                 (coalton:lisp web3/types:U256 () max-fee-u)
                 (coalton:lisp coalton:U64 () gas-limit)
                 (coalton:lisp web3/address:Address () to)
                 (coalton:lisp web3/types:U256 () value)))))
      (%check (hash (coalton:coalton
                      (web3/wallet:wallet-send-transaction
                       (coalton:lisp web3/wallet:Wallet () inner)
                       (coalton:lisp web3/transaction:Transaction () tx))))
        hash))))
