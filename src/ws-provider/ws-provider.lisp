;;;; WebSocket Provider implementation
;;;; Real-time Ethereum subscriptions over WebSocket

(in-package #:web3/ws-provider)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Subscription Types
  ;;; =========================================================================

  (define-type SubscriptionType
    "Types of Ethereum subscriptions"
    SubNewHeads                    ; New block headers
    SubNewPendingTransactions      ; New pending transactions
    (SubLogs LogFilter)            ; Log events with filter
    SubSyncing)                    ; Sync status changes

  ;;; =========================================================================
  ;;; Log Filter
  ;;; =========================================================================

  (define-struct LogFilter
    "Filter for log subscriptions"
    (filter-address (Optional addr:Address))      ; Contract address to filter
    (filter-topics (List (Optional types:Bytes)))) ; Topic filters (up to 4)

  (declare make-log-filter ((Optional addr:Address) -> (List (Optional types:Bytes)) -> LogFilter))
  (define (make-log-filter address topics)
    (LogFilter address topics))

  ;;; =========================================================================
  ;;; Block Header (from newHeads subscription)
  ;;; =========================================================================

  (define-struct BlockHeader
    "Block header received from newHeads subscription"
    (header-number UFix)
    (header-hash types:Bytes)
    (header-parent-hash types:Bytes)
    (header-timestamp UFix)
    (header-gas-limit types:U256)
    (header-gas-used types:U256)
    (header-base-fee (Optional types:U256)))

  ;;; =========================================================================
  ;;; Log Entry (from logs subscription)
  ;;; =========================================================================

  (define-struct LogEntry
    "Log entry received from logs subscription"
    (log-address addr:Address)
    (log-topics (List types:Bytes))
    (log-data types:Bytes)
    (log-block-number UFix)
    (log-tx-hash types:Bytes)
    (log-tx-index UFix)
    (log-block-hash types:Bytes)
    (log-log-index UFix))

  ;;; =========================================================================
  ;;; Sync Status (from syncing subscription)
  ;;; =========================================================================

  (define-type SyncStatus
    "Sync status from syncing subscription"
    (Syncing UFix UFix UFix)  ; starting, current, highest
    NotSyncing)

  ;; Accessors for Syncing variant
  (declare sync-starting-block (SyncStatus -> (Optional UFix)))
  (define (sync-starting-block status)
    (match status
      ((Syncing s _ _) (Some s))
      ((NotSyncing) None)))

  (declare sync-current-block (SyncStatus -> (Optional UFix)))
  (define (sync-current-block status)
    (match status
      ((Syncing _ c _) (Some c))
      ((NotSyncing) None)))

  (declare sync-highest-block (SyncStatus -> (Optional UFix)))
  (define (sync-highest-block status)
    (match status
      ((Syncing _ _ h) (Some h))
      ((NotSyncing) None)))

  ;;; =========================================================================
  ;;; Subscription
  ;;; =========================================================================

  (define-struct Subscription
    "An active subscription"
    (subscription-id String)
    (subscription-type SubscriptionType))

  ;;; =========================================================================
  ;;; WebSocket Messages
  ;;; =========================================================================

  (define-type WsMessage
    "Messages that can be received over WebSocket"
    (WsSubscriptionData String String)  ; subscription-id, raw-json-data
    (WsUnsubscribed String Boolean)     ; subscription-id, success
    (WsError String))                   ; error message

  ;;; =========================================================================
  ;;; Request Encoding
  ;;; =========================================================================

  (declare encode-subscribe-request (UFix -> SubscriptionType -> String))
  (define (encode-subscribe-request request-id sub-type)
    "Encode an eth_subscribe JSON-RPC request"
    (lisp String (request-id sub-type)
      (%encode-subscribe-request request-id sub-type)))

  (declare encode-unsubscribe-request (UFix -> String -> String))
  (define (encode-unsubscribe-request request-id subscription-id)
    "Encode an eth_unsubscribe JSON-RPC request"
    (lisp String (request-id subscription-id)
      (cl:format cl:nil
                 "{\"jsonrpc\":\"2.0\",\"id\":~A,\"method\":\"eth_unsubscribe\",\"params\":[\"~A\"]}"
                 request-id subscription-id)))

  ;;; =========================================================================
  ;;; Response Parsing
  ;;; =========================================================================

  (declare parse-subscription-response (String -> (types:Web3Result String)))
  (define (parse-subscription-response json-str)
    "Parse eth_subscribe response to get subscription ID"
    (lisp (types:Web3Result String) (json-str)
      (cl:handler-case
          (cl:let* ((parsed (cl-json:decode-json-from-string json-str))
                    (result (cl:cdr (cl:assoc :result parsed)))
                    (error-obj (cl:cdr (cl:assoc :error parsed))))
            (cl:if error-obj
                   (Err (web3/types:ProviderError
                         (cl:format cl:nil "Subscription error: ~A"
                                    (cl:cdr (cl:assoc :message error-obj)))))
                   (cl:if result
                          (Ok result)
                          (Err (web3/types:ProviderError "No subscription ID in response")))))
        (cl:error (e)
          (Err (web3/types:ProviderError
                (cl:format cl:nil "Failed to parse response: ~A" e)))))))

  (declare parse-subscription-notification (String -> (types:Web3Result (Tuple String String))))
  (define (parse-subscription-notification json-str)
    "Parse subscription notification, returns (subscription-id, params-json)"
    (lisp (types:Web3Result (Tuple String String)) (json-str)
      (cl:handler-case
          (cl:let* ((parsed (cl-json:decode-json-from-string json-str))
                    (method (cl:cdr (cl:assoc :method parsed)))
                    (params (cl:cdr (cl:assoc :params parsed))))
            (cl:if (cl:and method (cl:string= method "eth_subscription") params)
                   (cl:let ((sub-id (cl:cdr (cl:assoc :subscription params)))
                            (result (cl:cdr (cl:assoc :result params))))
                     (cl:if (cl:and sub-id result)
                            (Ok (Tuple sub-id (cl-json:encode-json-to-string result)))
                            (Err (web3/types:ProviderError "Invalid notification format"))))
                   (Err (web3/types:ProviderError "Not a subscription notification"))))
        (cl:error (e)
          (Err (web3/types:ProviderError
                (cl:format cl:nil "Failed to parse notification: ~A" e))))))))


;;; =========================================================================
;;; CL-Level Request Encoding
;;; =========================================================================

(cl:defun %encode-subscribe-request (request-id sub-type)
  "Encode eth_subscribe request based on subscription type"
  (cl:let ((method-params
             (cl:typecase sub-type
               (web3/ws-provider::subscriptiontype/subnewheads
                "[\"newHeads\"]")
               (web3/ws-provider::subscriptiontype/subnewpendingtransactions
                "[\"newPendingTransactions\"]")
               (web3/ws-provider::subscriptiontype/sublogs
                (cl:let ((log-fltr (cl:slot-value sub-type 'web3/ws-provider::|_0|)))
                  (%encode-logs-params log-fltr)))
               (web3/ws-provider::subscriptiontype/subsyncing
                "[\"syncing\"]")
               (cl:t (cl:error "Unknown subscription type: ~A" sub-type)))))
    (cl:format cl:nil
               "{\"jsonrpc\":\"2.0\",\"id\":~A,\"method\":\"eth_subscribe\",\"params\":~A}"
               request-id method-params)))

(cl:defun %encode-logs-params (log-fltr)
  "Encode logs subscription parameters"
  ;; Access struct fields using Coalton accessors
  (cl:let* ((addr-opt (coalton:coalton
                       (web3/ws-provider:.filter-address
                        (coalton:lisp LogFilter () log-fltr))))
            (topics (coalton:coalton
                     (web3/ws-provider:.filter-topics
                      (coalton:lisp LogFilter () log-fltr))))
            (filter-obj (cl:make-hash-table :test 'cl:equal)))
    ;; Add address if present
    (cl:when (web3/types:%is-some-p addr-opt)
      (cl:let* ((addr (web3/types:%unwrap-some addr-opt))
                (addr-bytes (coalton:coalton
                             (addr:address-bytes
                              (coalton:lisp addr:Address () addr))))
                (addr-hex (coalton:coalton
                           (types:hex-encode-prefixed
                            (coalton:lisp types:Bytes () addr-bytes)))))
        (cl:setf (cl:gethash "address" filter-obj) addr-hex)))
    ;; Add topics if present
    (cl:when topics
      (cl:let ((topic-list (%convert-topics topics)))
        (cl:when topic-list
          (cl:setf (cl:gethash "topics" filter-obj) topic-list))))
    ;; Encode
    (cl:format cl:nil "[\"logs\",~A]" (cl-json:encode-json-to-string filter-obj))))

(cl:defun %convert-topics (topics)
  "Convert Coalton topic list to CL list for JSON encoding"
  (cl:if (cl:null topics)
         cl:nil
         (cl:let ((rest-topics (%convert-topics (cl:cdr topics)))
                  (topic-opt (cl:car topics)))
           (cl:cons
            (cl:if (web3/types:%is-some-p topic-opt)
                   (cl:let* ((topic-bytes (web3/types:%unwrap-some topic-opt))
                             (topic-hex (coalton:coalton
                                         (types:hex-encode-prefixed
                                          (coalton:lisp types:Bytes () topic-bytes)))))
                     topic-hex)
                   :null)
            rest-topics))))


;;; =========================================================================
;;; Block Header Parsing
;;; =========================================================================

(coalton-toplevel

  (declare parse-block-header (String -> (types:Web3Result BlockHeader)))
  (define (parse-block-header json-str)
    "Parse a block header from JSON"
    (lisp (types:Web3Result BlockHeader) (json-str)
      (cl:handler-case
          (cl:let* ((parsed (cl-json:decode-json-from-string json-str))
                    (number-hex (cl:cdr (cl:assoc :number parsed)))
                    (hash-hex (cl:cdr (cl:assoc :hash parsed)))
                    (parent-hex (cl:cdr (cl:assoc :parent-hash parsed)))
                    (timestamp-hex (cl:cdr (cl:assoc :timestamp parsed)))
                    (gas-limit-hex (cl:cdr (cl:assoc :gas-limit parsed)))
                    (gas-used-hex (cl:cdr (cl:assoc :gas-used parsed)))
                    (base-fee-hex (cl:cdr (cl:assoc :base-fee-per-gas parsed))))
            (Ok (BlockHeader
                 (web3/types:%parse-hex-ufix number-hex)
                 (web3/types:%parse-hex-bytes hash-hex)
                 (web3/types:%parse-hex-bytes parent-hex)
                 (web3/types:%parse-hex-ufix timestamp-hex)
                 (web3/types:%parse-hex-u256 gas-limit-hex)
                 (web3/types:%parse-hex-u256 gas-used-hex)
                 (cl:if base-fee-hex
                        (Some (web3/types:%parse-hex-u256 base-fee-hex))
                        None))))
        (cl:error (e)
          (Err (web3/types:ProviderError
                (cl:format cl:nil "Failed to parse block header: ~A" e)))))))

  (declare parse-log-entry (String -> (types:Web3Result LogEntry)))
  (define (parse-log-entry json-str)
    "Parse a log entry from JSON"
    (lisp (types:Web3Result LogEntry) (json-str)
      (cl:handler-case
          (cl:let* ((parsed (cl-json:decode-json-from-string json-str))
                    (addr-hex (cl:cdr (cl:assoc :address parsed)))
                    (topics-list (cl:cdr (cl:assoc :topics parsed)))
                    (data-hex (cl:cdr (cl:assoc :data parsed)))
                    (block-num-hex (cl:cdr (cl:assoc :block-number parsed)))
                    (tx-hash-hex (cl:cdr (cl:assoc :transaction-hash parsed)))
                    (tx-idx-hex (cl:cdr (cl:assoc :transaction-index parsed)))
                    (block-hash-hex (cl:cdr (cl:assoc :block-hash parsed)))
                    (log-idx-hex (cl:cdr (cl:assoc :log-index parsed))))
            (cl:let ((addr-result (%parse-address addr-hex)))
              ;; Check if result is Ok by checking its type
              (cl:if (web3/types:%result-ok-p addr-result)
                     (Ok (LogEntry
                          (web3/types:%unwrap-ok addr-result)
                          (%parse-topics-list topics-list)
                          (web3/types:%parse-hex-bytes data-hex)
                          (web3/types:%parse-hex-ufix block-num-hex)
                          (web3/types:%parse-hex-bytes tx-hash-hex)
                          (web3/types:%parse-hex-ufix tx-idx-hex)
                          (web3/types:%parse-hex-bytes block-hash-hex)
                          (web3/types:%parse-hex-ufix log-idx-hex)))
                     (Err (web3/types:ProviderError "Invalid address in log")))))
        (cl:error (e)
          (Err (web3/types:ProviderError
                (cl:format cl:nil "Failed to parse log entry: ~A" e)))))))

  (declare parse-sync-status (String -> (types:Web3Result SyncStatus)))
  (define (parse-sync-status json-str)
    "Parse sync status from JSON (either false or object with blocks)"
    (lisp (types:Web3Result SyncStatus) (json-str)
      (cl:handler-case
          (cl:let ((parsed (cl-json:decode-json-from-string json-str)))
            (cl:if (cl:eq parsed cl:nil)
                   (Ok NotSyncing)
                   (cl:if (cl:listp parsed)
                          (cl:let ((starting (cl:cdr (cl:assoc :starting-block parsed)))
                                   (current (cl:cdr (cl:assoc :current-block parsed)))
                                   (highest (cl:cdr (cl:assoc :highest-block parsed))))
                            (Ok (Syncing
                                 (web3/types:%parse-hex-ufix starting)
                                 (web3/types:%parse-hex-ufix current)
                                 (web3/types:%parse-hex-ufix highest))))
                          (Ok NotSyncing))))
        (cl:error (e)
          (Err (web3/types:ProviderError
                (cl:format cl:nil "Failed to parse sync status: ~A" e))))))))


;;; =========================================================================
;;; CL Helper Functions
;;; =========================================================================

(cl:defun %parse-address (hex-str)
  "Parse hex string to Address"
  (coalton:coalton
   (addr:address-from-hex (coalton:lisp coalton:String () hex-str))))

(cl:defun %parse-topics-list (topics)
  "Parse list of topic hex strings to Coalton list of Bytes"
  (cl:if (cl:null topics)
         coalton:Nil
         (coalton:Cons (web3/types:%parse-hex-bytes (cl:first topics))
                       (%parse-topics-list (cl:rest topics)))))


;;; =========================================================================
;;; Connection State (CL-level for managing subscriptions)
;;; =========================================================================

(cl:defstruct ws-connection-state
  "State for a WebSocket connection"
  (url "" :type cl:string)
  (subscriptions (cl:make-hash-table :test 'cl:equal) :type cl:hash-table)
  (next-id 1 :type cl:integer))

(cl:defun ws-state-add-subscription (state sub-id sub-type)
  "Add a subscription to state"
  (cl:setf (cl:gethash sub-id (ws-connection-state-subscriptions state))
           sub-type))

(cl:defun ws-state-remove-subscription (state sub-id)
  "Remove a subscription from state"
  (cl:remhash sub-id (ws-connection-state-subscriptions state)))

(cl:defun ws-state-get-subscription (state sub-id)
  "Get subscription type by ID"
  (cl:gethash sub-id (ws-connection-state-subscriptions state)))

(cl:defun ws-state-next-request-id (state)
  "Get next request ID and increment"
  (cl:prog1 (ws-connection-state-next-id state)
    (cl:incf (ws-connection-state-next-id state))))


