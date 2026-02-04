;;;; Receipt Parsing implementation
;;;; Parse Ethereum transaction receipts from JSON-RPC responses

(in-package #:web3/receipt)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Receipt Status Type
  ;;; =========================================================================

  (define-type ReceiptStatus
    "Transaction execution status"
    StatusSuccess    ; Transaction succeeded (status = 1)
    StatusFailed     ; Transaction failed/reverted (status = 0)
    StatusUnknown)   ; Pre-Byzantium receipt (no status field)

  ;;; =========================================================================
  ;;; Transaction Type
  ;;; =========================================================================

  (define-type TxType
    "EIP-2718 transaction type"
    TxTypeLegacy      ; Type 0: Legacy transaction
    TxTypeAccessList  ; Type 1: EIP-2930 access list
    TxTypeEip1559     ; Type 2: EIP-1559 dynamic fee
    TxTypeBlob)       ; Type 3: EIP-4844 blob transaction

  ;;; =========================================================================
  ;;; Log Entry Type
  ;;; =========================================================================

  (define-struct LogEntry
    "A log entry from a transaction receipt"
    (log-address addr:Address)              ; Contract that emitted the log
    (log-topics (List types:Bytes))       ; Indexed event parameters
    (log-data types:Bytes)                ; Non-indexed event data
    (log-block-number UFix)                 ; Block containing the log
    (log-transaction-hash types:Bytes)    ; Transaction that created log
    (log-transaction-index UFix)            ; Tx position in block
    (log-block-hash types:Bytes)          ; Block hash
    (log-log-index UFix)                    ; Log position in block
    (log-removed Boolean))                  ; True if removed due to reorg

  (declare make-log-entry (addr:Address -> (List types:Bytes) -> types:Bytes ->
                           UFix -> types:Bytes -> UFix -> types:Bytes ->
                           UFix -> Boolean -> LogEntry))
  (define (make-log-entry address topics data block-num tx-hash tx-idx block-hash log-idx removed)
    (LogEntry address topics data block-num tx-hash tx-idx block-hash log-idx removed))

  ;;; =========================================================================
  ;;; Receipt Type
  ;;; =========================================================================

  (define-struct Receipt
    "Transaction receipt containing execution results"
    (receipt-transaction-hash types:Bytes)
    (receipt-transaction-index UFix)
    (receipt-block-hash types:Bytes)
    (receipt-block-number UFix)
    (receipt-from addr:Address)
    (receipt-to (Optional addr:Address))      ; None for contract creation
    (receipt-cumulative-gas-used UFix)        ; Total gas used in block up to this tx
    (receipt-gas-used UFix)                   ; Gas used by this tx
    (receipt-effective-gas-price types:U256)  ; Actual gas price paid
    (receipt-contract-address (Optional addr:Address))  ; Created contract address
    (receipt-logs (List LogEntry))
    (receipt-logs-bloom types:Bytes)          ; 256-byte bloom filter
    (receipt-status ReceiptStatus)
    (receipt-type TxType))

  (declare make-receipt (types:Bytes -> UFix -> types:Bytes -> UFix ->
                         addr:Address -> (Optional addr:Address) -> UFix -> UFix ->
                         types:U256 -> (Optional addr:Address) -> (List LogEntry) ->
                         types:Bytes -> ReceiptStatus -> TxType -> Receipt))
  (define (make-receipt tx-hash tx-idx block-hash block-num from to
                        cumulative-gas gas-used effective-price contract-addr
                        logs bloom status tx-type)
    (Receipt tx-hash tx-idx block-hash block-num from to
             cumulative-gas gas-used effective-price contract-addr
             logs bloom status tx-type))

  ;;; =========================================================================
  ;;; Query Helpers
  ;;; =========================================================================

  (declare receipt-success? (Receipt -> Boolean))
  (define (receipt-success? receipt)
    "Check if the transaction succeeded"
    (match (.receipt-status receipt)
      ((StatusSuccess) True)
      ((StatusFailed) False)
      ((StatusUnknown) True)))  ; Pre-Byzantium assumes success if included

  (declare receipt-failed? (Receipt -> Boolean))
  (define (receipt-failed? receipt)
    "Check if the transaction failed/reverted"
    (match (.receipt-status receipt)
      ((StatusSuccess) False)
      ((StatusFailed) True)
      ((StatusUnknown) False)))

  (declare receipt-contract-created? (Receipt -> Boolean))
  (define (receipt-contract-created? receipt)
    "Check if this transaction created a contract"
    (match (.receipt-contract-address receipt)
      ((Some _) True)
      ((None) False)))

  (declare receipt-total-cost (Receipt -> types:U256))
  (define (receipt-total-cost receipt)
    "Calculate total transaction cost (gas-used * effective-gas-price)"
    (let ((gas-used (.receipt-gas-used receipt))
          (gas-price (.receipt-effective-gas-price receipt)))
      (lisp types:U256 (gas-used gas-price)
        (cl:let ((price-int (web3/types::u256-to-integer gas-price)))
          (web3/types::u256-from-integer (cl:* gas-used price-int))))))

  (declare filter-logs-by-address ((List LogEntry) -> addr:Address -> (List LogEntry)))
  (define (filter-logs-by-address logs target-address)
    "Filter logs emitted by a specific contract address"
    (list:filter
     (fn (log)
       (types:bytes-equal? (addr:address-bytes (.log-address log))
                           (addr:address-bytes target-address)))
     logs))

  (declare filter-logs-by-topic ((List LogEntry) -> types:Bytes -> (List LogEntry)))
  (define (filter-logs-by-topic logs topic)
    "Filter logs by first topic (event signature)"
    (list:filter
     (fn (log)
       (match (.log-topics log)
         ((Cons first-topic _) (types:bytes-equal? first-topic topic))
         ((Nil) False)))
     logs))

  ;;; =========================================================================
  ;;; Parsing Functions (Coalton wrappers)
  ;;; =========================================================================

  (declare parse-receipt (String -> (types:Web3Result Receipt)))
  (define (parse-receipt json-str)
    "Parse a receipt from JSON string"
    (lisp (types:Web3Result Receipt) (json-str)
      (%parse-receipt-json json-str)))

  (declare parse-receipt-json (String -> (types:Web3Result Receipt)))
  (define (parse-receipt-json json-str)
    "Alias for parse-receipt"
    (parse-receipt json-str))

  (declare parse-log-entry (String -> (types:Web3Result LogEntry)))
  (define (parse-log-entry json-str)
    "Parse a single log entry from JSON string"
    (lisp (types:Web3Result LogEntry) (json-str)
      (%parse-log-entry-json json-str)))

  (declare parse-log-entry-json (String -> (types:Web3Result LogEntry)))
  (define (parse-log-entry-json json-str)
    "Alias for parse-log-entry"
    (parse-log-entry json-str))

  ;;; =========================================================================
  ;;; JSON-RPC Request/Response
  ;;; =========================================================================

  (declare encode-get-receipt-request (types:Bytes -> String))
  (define (encode-get-receipt-request tx-hash)
    "Encode eth_getTransactionReceipt request"
    (lisp String (tx-hash)
      (%encode-get-receipt-request tx-hash)))

  (declare parse-get-receipt-response (String -> (types:Web3Result (Optional Receipt))))
  (define (parse-get-receipt-response json-str)
    "Parse eth_getTransactionReceipt response (may be null if pending)"
    (lisp (types:Web3Result (Optional Receipt)) (json-str)
      (%parse-get-receipt-response json-str))))


;;; =========================================================================
;;; CL-Level Parsing Functions
;;; =========================================================================

(cl:defun %parse-hex-ufix (hex-str)
  "Parse hex string to UFix"
  (cl:if (cl:and hex-str (cl:> (cl:length hex-str) 2))
         (cl:parse-integer (cl:subseq hex-str 2) :radix 16)
         0))

(cl:defun %parse-hex-u256 (hex-str)
  "Parse hex string to U256"
  (cl:if (cl:and hex-str (cl:> (cl:length hex-str) 2))
         (cl:let ((int-val (cl:parse-integer (cl:subseq hex-str 2) :radix 16)))
           (web3/types::u256-from-integer int-val))
         (web3/types::u256-from-integer 0)))

(cl:defun %parse-hex-bytes (hex-str)
  "Parse hex string to Bytes (adjustable vector for Coalton)"
  (cl:if (cl:and hex-str (cl:> (cl:length hex-str) 2))
         (cl:let* ((hex-part (cl:subseq hex-str 2))
                   (len (cl:floor (cl:length hex-part) 2))
                   (bytes (cl:make-array len :element-type 'cl:t
                                         :fill-pointer len
                                         :adjustable cl:t)))
           (cl:dotimes (i len bytes)
             (cl:setf (cl:aref bytes i)
                      (cl:parse-integer hex-part :start (cl:* i 2) :end (cl:+ (cl:* i 2) 2) :radix 16))))
         (cl:make-array 0 :element-type 'cl:t :fill-pointer 0 :adjustable cl:t)))

(cl:defun %parse-hex-bytes32 (hex-str)
  "Parse hex string to Bytes (32 bytes, adjustable vector for Coalton)"
  (cl:let ((bytes (%parse-hex-bytes hex-str)))
    ;; Ensure exactly 32 bytes
    (cl:if (cl:= (cl:length bytes) 32)
           bytes
           (cl:let ((result (cl:make-array 32 :element-type 'cl:t
                                           :fill-pointer 32
                                           :adjustable cl:t
                                           :initial-element 0)))
             (cl:dotimes (i (cl:min 32 (cl:length bytes)) result)
               (cl:setf (cl:aref result i) (cl:aref bytes i)))))))

;; Helper to check if a Result is Ok (by checking type name string)
(cl:defun %result-ok-p (result)
  "Check if Coalton Result is Ok"
  (cl:let ((type-name (cl:symbol-name (cl:type-of result))))
    (cl:search "OK" type-name)))

;; Helper to extract value from Ok (slot _0)
(cl:defun %unwrap-ok (result)
  "Extract the inner value from Ok"
  (cl:slot-value result 'coalton-library/classes::|_0|))

(cl:defun %parse-address (hex-str)
  "Parse hex string to Address"
  (cl:if hex-str
         (cl:let ((result (coalton:coalton
                           (web3/address:address-from-hex
                            (coalton:lisp coalton:String () hex-str)))))
           (cl:if (%result-ok-p result)
                  (%unwrap-ok result)
                  cl:nil))
         cl:nil))

(cl:defun %parse-optional-address (hex-str)
  "Parse hex string to Optional Address"
  (cl:let ((addr (%parse-address hex-str)))
    (cl:if addr
           (coalton:coalton (Some (coalton:lisp addr:Address () addr)))
           (coalton:coalton (the (Optional addr:Address) None)))))

(cl:defun %parse-status (status-hex)
  "Parse status field to ReceiptStatus"
  (cl:cond
    ((cl:null status-hex) (coalton:coalton StatusUnknown))
    ((cl:equal status-hex "0x1") (coalton:coalton StatusSuccess))
    ((cl:equal status-hex "0x0") (coalton:coalton StatusFailed))
    (cl:t (cl:let ((val (%parse-hex-ufix status-hex)))
            (cl:if (cl:> val 0)
                   (coalton:coalton StatusSuccess)
                   (coalton:coalton StatusFailed))))))

(cl:defun %parse-tx-type (type-hex)
  "Parse transaction type"
  (cl:if (cl:null type-hex)
         (coalton:coalton TxTypeLegacy)
         (cl:let ((type-val (%parse-hex-ufix type-hex)))
           (cl:case type-val
             (0 (coalton:coalton TxTypeLegacy))
             (1 (coalton:coalton TxTypeAccessList))
             (2 (coalton:coalton TxTypeEip1559))
             (3 (coalton:coalton TxTypeBlob))
             (cl:otherwise (coalton:coalton TxTypeLegacy))))))

(cl:defun %parse-topics (topics-list)
  "Parse list of topic hex strings to Coalton list of Bytes"
  (cl:if (cl:null topics-list)
         coalton:Nil
         (coalton:Cons (%parse-hex-bytes32 (cl:first topics-list))
                       (%parse-topics (cl:rest topics-list)))))

(cl:defun %parse-single-log (log-obj)
  "Parse a single log entry from alist"
  (cl:let* ((address-hex (cl:cdr (cl:assoc :address log-obj)))
            (topics-list (cl:cdr (cl:assoc :topics log-obj)))
            (data-hex (cl:cdr (cl:assoc :data log-obj)))
            (block-num-hex (cl:cdr (cl:assoc :block-number log-obj)))
            (tx-hash-hex (cl:cdr (cl:assoc :transaction-hash log-obj)))
            (tx-idx-hex (cl:cdr (cl:assoc :transaction-index log-obj)))
            (block-hash-hex (cl:cdr (cl:assoc :block-hash log-obj)))
            (log-idx-hex (cl:cdr (cl:assoc :log-index log-obj)))
            (removed (cl:cdr (cl:assoc :removed log-obj))))
    (web3/receipt::make-log-entry
     (%parse-address address-hex)
     (%parse-topics topics-list)
     (%parse-hex-bytes data-hex)
     (%parse-hex-ufix block-num-hex)
     (%parse-hex-bytes32 tx-hash-hex)
     (%parse-hex-ufix tx-idx-hex)
     (%parse-hex-bytes32 block-hash-hex)
     (%parse-hex-ufix log-idx-hex)
     (cl:if removed coalton:True coalton:False))))

(cl:defun %parse-logs (logs-list)
  "Parse list of log objects to Coalton list"
  (cl:if (cl:null logs-list)
         coalton:Nil
         (coalton:Cons (%parse-single-log (cl:first logs-list))
                       (%parse-logs (cl:rest logs-list)))))

(cl:defun %parse-receipt-from-alist (receipt-obj)
  "Parse receipt from alist"
  (cl:let* ((tx-hash-hex (cl:cdr (cl:assoc :transaction-hash receipt-obj)))
            (tx-idx-hex (cl:cdr (cl:assoc :transaction-index receipt-obj)))
            (block-hash-hex (cl:cdr (cl:assoc :block-hash receipt-obj)))
            (block-num-hex (cl:cdr (cl:assoc :block-number receipt-obj)))
            (from-hex (cl:cdr (cl:assoc :from receipt-obj)))
            (to-hex (cl:cdr (cl:assoc :to receipt-obj)))
            (cumulative-gas-hex (cl:cdr (cl:assoc :cumulative-gas-used receipt-obj)))
            (gas-used-hex (cl:cdr (cl:assoc :gas-used receipt-obj)))
            (effective-price-hex (cl:cdr (cl:assoc :effective-gas-price receipt-obj)))
            (contract-addr-hex (cl:cdr (cl:assoc :contract-address receipt-obj)))
            (logs-list (cl:cdr (cl:assoc :logs receipt-obj)))
            (logs-bloom-hex (cl:cdr (cl:assoc :logs-bloom receipt-obj)))
            (status-hex (cl:cdr (cl:assoc :status receipt-obj)))
            (type-hex (cl:cdr (cl:assoc :type receipt-obj))))
    (web3/receipt::make-receipt
     (%parse-hex-bytes32 tx-hash-hex)
     (%parse-hex-ufix tx-idx-hex)
     (%parse-hex-bytes32 block-hash-hex)
     (%parse-hex-ufix block-num-hex)
     (%parse-address from-hex)
     (%parse-optional-address to-hex)
     (%parse-hex-ufix cumulative-gas-hex)
     (%parse-hex-ufix gas-used-hex)
     (%parse-hex-u256 (cl:or effective-price-hex "0x0"))
     (%parse-optional-address contract-addr-hex)
     (%parse-logs logs-list)
     (%parse-hex-bytes (cl:or logs-bloom-hex "0x"))
     (%parse-status status-hex)
     (%parse-tx-type type-hex))))

(cl:defun %parse-receipt-json (json-str)
  "Parse receipt JSON string"
  (cl:handler-case
      (cl:let* ((parsed (cl-json:decode-json-from-string json-str)))
        (Ok (%parse-receipt-from-alist parsed)))
    (cl:error (e)
      (Err (web3/types:ProviderError
            (cl:format cl:nil "Failed to parse receipt: ~A" e))))))

(cl:defun %parse-log-entry-json (json-str)
  "Parse log entry JSON string"
  (cl:handler-case
      (cl:let* ((parsed (cl-json:decode-json-from-string json-str)))
        (Ok (%parse-single-log parsed)))
    (cl:error (e)
      (Err (web3/types:ProviderError
            (cl:format cl:nil "Failed to parse log entry: ~A" e))))))

(cl:defun %encode-get-receipt-request (tx-hash)
  "Encode eth_getTransactionReceipt params"
  (cl:let ((hash-hex (web3/types::hex-encode-prefixed tx-hash)))
    (cl:format cl:nil "[\"~A\"]" hash-hex)))

(cl:defun %parse-get-receipt-response (json-str)
  "Parse eth_getTransactionReceipt response"
  (cl:handler-case
      (cl:let* ((parsed (cl-json:decode-json-from-string json-str))
                (result (cl:cdr (cl:assoc :result parsed))))
        (cl:if (cl:null result)
               (Ok (coalton:coalton (the (Optional Receipt) None)))  ; Transaction pending or not found
               (cl:let ((receipt (%parse-receipt-from-alist result)))
                 (Ok (coalton:coalton (Some (coalton:lisp Receipt () receipt)))))))
    (cl:error (e)
      (Err (web3/types:ProviderError
            (cl:format cl:nil "Failed to parse receipt response: ~A" e))))))


;;; =========================================================================
;;; Exports
;;; =========================================================================

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:export '(LogEntry
               make-log-entry
               .log-address
               .log-topics
               .log-data
               .log-block-number
               .log-transaction-hash
               .log-transaction-index
               .log-block-hash
               .log-log-index
               .log-removed
               Receipt
               make-receipt
               .receipt-transaction-hash
               .receipt-transaction-index
               .receipt-block-hash
               .receipt-block-number
               .receipt-from
               .receipt-to
               .receipt-cumulative-gas-used
               .receipt-gas-used
               .receipt-effective-gas-price
               .receipt-contract-address
               .receipt-logs
               .receipt-logs-bloom
               .receipt-status
               .receipt-type
               ReceiptStatus
               StatusSuccess
               StatusFailed
               StatusUnknown
               TxType
               TxTypeLegacy
               TxTypeAccessList
               TxTypeEip1559
               TxTypeBlob
               parse-receipt
               parse-receipt-json
               parse-log-entry
               parse-log-entry-json
               receipt-success?
               receipt-failed?
               receipt-contract-created?
               receipt-total-cost
               filter-logs-by-address
               filter-logs-by-topic
               encode-get-receipt-request
               parse-get-receipt-response)
             (cl:find-package '#:web3/receipt)))
