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
          (cl:let* ((url (cl:slot-value provider 'web3/provider::|_0|))
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
               (Ok (web3/receipt::%parse-logs result-val)))
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
;;; Filter Serialization
;;; =========================================================================

(cl:defun %block-tag-to-json (tag)
  "Convert a Coalton BlockTag to a JSON string value"
  (coalton:coalton
   (block:block-tag-to-string
    (coalton:lisp block:BlockTag () tag))))

(cl:defun %serialize-optional-bytes-topic (opt-bytes)
  "Serialize an Optional Bytes topic to JSON (null or hex string)"
  (cl:if (web3/types:%is-some-p opt-bytes)
         (web3/types:hex-encode-prefixed (web3/types:%unwrap-some opt-bytes))
         cl:nil))  ; -> JSON null

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
            (filter-alist cl:nil))
    (cl:when (web3/types:%is-some-p from-block)
      (cl:push (cl:cons "fromBlock"
                        (%block-tag-to-json (web3/types:%unwrap-some from-block)))
               filter-alist))
    (cl:when (web3/types:%is-some-p to-block)
      (cl:push (cl:cons "toBlock"
                        (%block-tag-to-json (web3/types:%unwrap-some to-block)))
               filter-alist))
    (cl:when (web3/types:%is-some-p address)
      (cl:let ((addr-val (web3/types:%unwrap-some address)))
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



;; Exports are declared in package.lisp defpackage :export clause
