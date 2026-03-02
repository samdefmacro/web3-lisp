;;;; Event Log Querying implementation
;;;; Query historical event logs via eth_getLogs

(in-package #:web3/logs)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Log Filter Type
  ;;; =========================================================================

  (define-struct LogFilter
    "Filter parameters for eth_getLogs queries"
    (filter-from-block  (Optional block:BlockTag))
    (filter-to-block    (Optional block:BlockTag))
    (filter-address     (Optional addr:Address))
    (filter-topics      (List (Optional types:Bytes))))

  (declare make-log-filter ((Optional block:BlockTag) ->
                            (Optional block:BlockTag) ->
                            (Optional addr:Address) ->
                            (List (Optional types:Bytes)) ->
                            LogFilter))
  (define (make-log-filter from-block to-block address topics)
    "Create a log filter with all parameters"
    (LogFilter from-block to-block address topics))

  ;;; =========================================================================
  ;;; Query Functions
  ;;; =========================================================================

  (declare eth-get-logs (provider:HttpProvider -> LogFilter -> (types:Web3Result (List receipt:LogEntry))))
  (define (eth-get-logs provider log-filter)
    "Query event logs matching a filter via eth_getLogs.
     Sends JSON-RPC request directly to avoid double JSON encode/decode."
    (lisp (types:Web3Result (List receipt:LogEntry)) (provider log-filter)
      (cl:handler-case
          (cl:let* ((url (cl:slot-value provider 'coalton-library/classes::|_0|))
                    (params-str (%serialize-log-filter log-filter))
                    (request-body
                      (cl:format cl:nil
                                 "{\"jsonrpc\":\"2.0\",\"method\":\"eth_getLogs\",\"params\":~A,\"id\":1}"
                                 params-str))
                    (response
                      (dexador:post url
                                    :content request-body
                                    :headers '(("Content-Type" . "application/json"))))
                    (json (cl-json:decode-json-from-string response))
                    (error-val (cl:cdr (cl:assoc :error json)))
                    (result-val (cl:cdr (cl:assoc :result json))))
            (cl:cond
              (error-val
               (Err (web3/types:ProviderError
                     (cl:format cl:nil "RPC error: ~A"
                                (cl:if (cl:listp error-val)
                                       (cl:or (cl:cdr (cl:assoc :message error-val))
                                              (cl:format cl:nil "~A" error-val))
                                       (cl:format cl:nil "~A" error-val))))))
              ((cl:null result-val)
               (Ok coalton:Nil))
              ((cl:listp result-val)
               (Ok (%parse-logs result-val)))
              (cl:t
               (Err (web3/types:ProviderError "Invalid eth_getLogs response format")))))
        (cl:error (e)
          (Err (web3/types:ProviderError
                (cl:format cl:nil "eth_getLogs error: ~A" e)))))))

  (declare get-logs-by-event (provider:HttpProvider -> addr:Address ->
                              block:BlockTag -> block:BlockTag ->
                              types:Bytes ->
                              (types:Web3Result (List receipt:LogEntry))))
  (define (get-logs-by-event provider address from-block to-block event-topic)
    "Query logs for a specific event signature from a contract"
    (eth-get-logs provider
                  (make-log-filter
                   (Some from-block)
                   (Some to-block)
                   (Some address)
                   (Cons (Some event-topic) Nil))))

  (declare get-logs-by-address (provider:HttpProvider -> addr:Address ->
                                block:BlockTag -> block:BlockTag ->
                                (types:Web3Result (List receipt:LogEntry))))
  (define (get-logs-by-address provider address from-block to-block)
    "Query all logs from a contract address in a block range"
    (eth-get-logs provider
                  (make-log-filter
                   (Some from-block)
                   (Some to-block)
                   (Some address)
                   Nil))))


;;; =========================================================================
;;; CL-Level Helper Functions
;;; =========================================================================

;; Result helpers (same pattern as receipt/block modules)
(cl:defun result-ok-p (result)
  "Check if Coalton Result is Ok"
  (cl:let ((type-name (cl:symbol-name (cl:type-of result))))
    (cl:search "OK" type-name)))

(cl:defun result-value (result)
  "Extract the inner value from Ok or Err"
  (cl:slot-value result 'coalton-library/classes::|_0|))

;;; =========================================================================
;;; JSON Parsing Helpers (duplicated from receipt module)
;;; =========================================================================

(cl:defun %parse-hex-ufix (hex-str)
  "Parse hex string to UFix"
  (cl:if (cl:and hex-str (cl:> (cl:length hex-str) 2))
         (cl:parse-integer (cl:subseq hex-str 2) :radix 16)
         0))

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

(cl:defun %parse-address (hex-str)
  "Parse hex string to Address"
  (cl:if hex-str
         (cl:let ((result (coalton:coalton
                           (web3/address:address-from-hex
                            (coalton:lisp coalton:String () hex-str)))))
           (cl:if (result-ok-p result)
                  (result-value result)
                  cl:nil))
         cl:nil))

(cl:defun %parse-topics (topics-list)
  "Parse list of topic hex strings to Coalton list of Bytes"
  (cl:if (cl:null topics-list)
         coalton:Nil
         (coalton:Cons (%parse-hex-bytes32 (cl:first topics-list))
                       (%parse-topics (cl:rest topics-list)))))

(cl:defun %parse-single-log (log-obj)
  "Parse a single log entry from alist. Returns a LogEntry or signals an error."
  (cl:let* ((address-hex (cl:cdr (cl:assoc :address log-obj)))
            (addr (%parse-address address-hex)))
    (cl:unless addr
      (cl:error "Invalid address in log entry: ~A" address-hex))
    (cl:let* ((topics-list (cl:cdr (cl:assoc :topics log-obj)))
              (data-hex (cl:cdr (cl:assoc :data log-obj)))
              (block-num-hex (cl:cdr (cl:assoc :block-number log-obj)))
              (tx-hash-hex (cl:cdr (cl:assoc :transaction-hash log-obj)))
              (tx-idx-hex (cl:cdr (cl:assoc :transaction-index log-obj)))
              (block-hash-hex (cl:cdr (cl:assoc :block-hash log-obj)))
              (log-idx-hex (cl:cdr (cl:assoc :log-index log-obj)))
              (removed (cl:cdr (cl:assoc :removed log-obj))))
      (web3/receipt::make-log-entry
       addr
       (%parse-topics topics-list)
       (%parse-hex-bytes data-hex)
       (%parse-hex-ufix block-num-hex)
       (%parse-hex-bytes32 tx-hash-hex)
       (%parse-hex-ufix tx-idx-hex)
       (%parse-hex-bytes32 block-hash-hex)
       (%parse-hex-ufix log-idx-hex)
       (cl:if removed coalton:True coalton:False)))))

(cl:defun %parse-logs (logs-list)
  "Parse list of log objects to Coalton list"
  (cl:if (cl:null logs-list)
         coalton:Nil
         (coalton:Cons (%parse-single-log (cl:first logs-list))
                       (%parse-logs (cl:rest logs-list)))))

;;; =========================================================================
;;; Filter Serialization
;;; =========================================================================

(cl:defun %block-tag-to-json (tag)
  "Convert a Coalton BlockTag to a JSON string value"
  (coalton:coalton
   (block:block-tag-to-string
    (coalton:lisp block:BlockTag () tag))))

(cl:defun %serialize-optional-bytes-topic (opt-bytes)
  "Serialize an Optional Bytes topic to JSON (null or hex string)"
  ;; Check if it's None
  (cl:if (cl:eq opt-bytes coalton-library/classes:None)
         cl:nil  ;; Will become JSON null
         ;; It's Some - extract the inner bytes
         (cl:let ((bytes (cl:slot-value opt-bytes 'coalton-library/classes::|_0|)))
           (web3/types::hex-encode-prefixed bytes))))

;; Note: Coalton Lists are CL cons cells, so no conversion needed.
;; coalton:Cons creates CL cons, coalton:Nil is CL nil.

(cl:defun %serialize-log-filter (log-filter)
  "Serialize a Coalton LogFilter to JSON params string for eth_getLogs"
  (cl:let* ((from-block (coalton:coalton
                          (web3/logs:.filter-from-block
                           (coalton:lisp web3/logs:LogFilter () log-filter))))
            (to-block (coalton:coalton
                        (web3/logs:.filter-to-block
                         (coalton:lisp web3/logs:LogFilter () log-filter))))
            (address (coalton:coalton
                       (web3/logs:.filter-address
                        (coalton:lisp web3/logs:LogFilter () log-filter))))
            (topics (coalton:coalton
                      (web3/logs:.filter-topics
                       (coalton:lisp web3/logs:LogFilter () log-filter))))
            ;; Build the filter object as alist
            (filter-alist cl:nil))
    ;; Add fromBlock if present
    (cl:unless (cl:eq from-block coalton-library/classes:None)
      (cl:push (cl:cons "fromBlock" (%block-tag-to-json
                                      (cl:slot-value from-block 'coalton-library/classes::|_0|)))
               filter-alist))
    ;; Add toBlock if present
    (cl:unless (cl:eq to-block coalton-library/classes:None)
      (cl:push (cl:cons "toBlock" (%block-tag-to-json
                                    (cl:slot-value to-block 'coalton-library/classes::|_0|)))
               filter-alist))
    ;; Add address if present
    (cl:unless (cl:eq address coalton-library/classes:None)
      (cl:let ((addr-val (cl:slot-value address 'coalton-library/classes::|_0|)))
        (cl:push (cl:cons "address"
                          (coalton:coalton
                           (addr:address-to-hex
                            (coalton:lisp addr:Address () addr-val))))
                 filter-alist)))
    ;; Add topics if non-empty (Coalton lists are CL cons cells)
    (cl:when topics
      (cl:push (cl:cons "topics"
                        (cl:mapcar #'%serialize-optional-bytes-topic topics))
               filter-alist))
    ;; Encode as JSON array with single filter object
    (cl:format cl:nil "[~A]"
               (cl-json:encode-json-to-string (cl:nreverse filter-alist)))))



;;; =========================================================================
;;; Exports
;;; =========================================================================

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:export '(LogFilter
               make-log-filter
               .filter-from-block
               .filter-to-block
               .filter-address
               .filter-topics
               eth-get-logs
               get-logs-by-event
               get-logs-by-address)
             (cl:find-package '#:web3/logs)))
