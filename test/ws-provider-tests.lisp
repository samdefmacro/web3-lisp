;;; WebSocket Provider tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; WebSocket Provider Tests
;;; =========================================================================

(defun run-ws-provider-tests ()
  (format t "~%=== WebSocket Provider Tests ===~%")

  ;;; =========================================================================
  ;;; Subscription Request Encoding Tests
  ;;; =========================================================================

  (test-case "encode-subscribe-request for newHeads"
    (let ((request (coalton:coalton
                    (web3/ws-provider:encode-subscribe-request
                     1
                     web3/ws-provider:SubNewHeads))))
      (assert (search "eth_subscribe" request))
      (assert (search "newHeads" request))
      (assert (search "\"id\":1" request))))

  (test-case "encode-subscribe-request for newPendingTransactions"
    (let ((request (coalton:coalton
                    (web3/ws-provider:encode-subscribe-request
                     2
                     web3/ws-provider:SubNewPendingTransactions))))
      (assert (search "eth_subscribe" request))
      (assert (search "newPendingTransactions" request))))

  (test-case "encode-subscribe-request for syncing"
    (let ((request (coalton:coalton
                    (web3/ws-provider:encode-subscribe-request
                     3
                     web3/ws-provider:SubSyncing))))
      (assert (search "eth_subscribe" request))
      (assert (search "syncing" request))))

  (test-case "encode-subscribe-request for logs with address"
    (let* ((addr-result (coalton:coalton
                         (web3/address:address-from-hex
                          "0xdAC17F958D2ee523a2206206994597C13D831ec7")))
           (addr (result-value addr-result))
           (filter (coalton:coalton
                    (web3/ws-provider:make-log-filter
                     (coalton-prelude:Some (coalton:lisp web3/address:Address () addr))
                     coalton:Nil)))
           (request (coalton:coalton
                     (web3/ws-provider:encode-subscribe-request
                      4
                      (web3/ws-provider:SubLogs
                       (coalton:lisp web3/ws-provider:LogFilter () filter))))))
      (assert (search "eth_subscribe" request))
      (assert (search "logs" request))
      (assert (search "address" request))))

  (test-case "encode-unsubscribe-request"
    (let ((request (coalton:coalton
                    (web3/ws-provider:encode-unsubscribe-request
                     5
                     "0x1234567890abcdef"))))
      (assert (search "eth_unsubscribe" request))
      (assert (search "0x1234567890abcdef" request))
      (assert (search "\"id\":5" request))))

  ;;; =========================================================================
  ;;; Response Parsing Tests
  ;;; =========================================================================

  (test-case "parse-subscription-response success"
    (let* ((response "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":\"0x1a2b3c4d\"}")
           (result (coalton:coalton
                    (web3/ws-provider:parse-subscription-response
                     (coalton:lisp coalton:String () response)))))
      (assert (result-ok-p result))
      (assert (string= (result-value result) "0x1a2b3c4d"))))

  (test-case "parse-subscription-response error"
    (let* ((response "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32000,\"message\":\"subscription not found\"}}")
           (result (coalton:coalton
                    (web3/ws-provider:parse-subscription-response
                     (coalton:lisp coalton:String () response)))))
      (assert (result-err-p result))))

  (test-case "parse-subscription-notification"
    (let* ((notification "{\"jsonrpc\":\"2.0\",\"method\":\"eth_subscription\",\"params\":{\"subscription\":\"0x1a2b3c\",\"result\":{\"number\":\"0x100\"}}}")
           (result (coalton:coalton
                    (web3/ws-provider:parse-subscription-notification
                     (coalton:lisp coalton:String () notification)))))
      (assert (result-ok-p result))))

  ;;; =========================================================================
  ;;; Block Header Parsing Tests
  ;;; =========================================================================

  (test-case "parse-block-header"
    (let* ((header-json "{\"number\":\"0x10d4f\",\"hash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"parentHash\":\"0xabcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890\",\"timestamp\":\"0x5f5e100\",\"gasLimit\":\"0x1c9c380\",\"gasUsed\":\"0x100000\",\"baseFeePerGas\":\"0x3b9aca00\"}")
           (result (coalton:coalton
                    (web3/ws-provider:parse-block-header
                     (coalton:lisp coalton:String () header-json)))))
      (assert (result-ok-p result))
      (let ((header (result-value result)))
        ;; Check block number (0x10d4f = 68943)
        (assert (= (coalton:coalton
                    (web3/ws-provider:.header-number
                     (coalton:lisp web3/ws-provider:BlockHeader () header)))
                   68943)))))

  (test-case "parse-block-header without baseFee (pre-London)"
    (let* ((header-json "{\"number\":\"0x100\",\"hash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"parentHash\":\"0xabcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890\",\"timestamp\":\"0x1000\",\"gasLimit\":\"0x1000000\",\"gasUsed\":\"0x500000\"}")
           (result (coalton:coalton
                    (web3/ws-provider:parse-block-header
                     (coalton:lisp coalton:String () header-json)))))
      (assert (result-ok-p result))))

  ;;; =========================================================================
  ;;; Log Entry Parsing Tests
  ;;; =========================================================================

  (test-case "parse-log-entry"
    (let* ((log-json "{\"address\":\"0xdAC17F958D2ee523a2206206994597C13D831ec7\",\"topics\":[\"0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef\",\"0x0000000000000000000000001111111111111111111111111111111111111111\",\"0x0000000000000000000000002222222222222222222222222222222222222222\"],\"data\":\"0x00000000000000000000000000000000000000000000000000000000000003e8\",\"blockNumber\":\"0x100\",\"transactionHash\":\"0xabcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"logIndex\":\"0x0\"}")
           (result (coalton:coalton
                    (web3/ws-provider:parse-log-entry
                     (coalton:lisp coalton:String () log-json)))))
      (assert (result-ok-p result))
      (let ((log (result-value result)))
        ;; Check block number (0x100 = 256)
        (assert (= (coalton:coalton
                    (web3/ws-provider:.log-block-number
                     (coalton:lisp web3/ws-provider:LogEntry () log)))
                   256)))))

  ;;; =========================================================================
  ;;; Sync Status Parsing Tests
  ;;; =========================================================================

  (test-case "parse-sync-status not syncing"
    (let* ((result (coalton:coalton
                    (web3/ws-provider:parse-sync-status "null"))))
      (assert (result-ok-p result))))

  (test-case "parse-sync-status syncing"
    (let* ((sync-json "{\"startingBlock\":\"0x100\",\"currentBlock\":\"0x200\",\"highestBlock\":\"0x300\"}")
           (result (coalton:coalton
                    (web3/ws-provider:parse-sync-status
                     (coalton:lisp coalton:String () sync-json)))))
      (assert (result-ok-p result))))

  ;;; =========================================================================
  ;;; Connection State Tests
  ;;; =========================================================================

  (test-case "ws-connection-state management"
    (let ((state (web3/ws-provider:make-ws-connection-state :url "ws://localhost:8546")))
      ;; Check initial state
      (assert (string= (web3/ws-provider:ws-connection-state-url state) "ws://localhost:8546"))
      (assert (= (web3/ws-provider:ws-connection-state-next-id state) 1))
      ;; Add subscription
      (web3/ws-provider:ws-state-add-subscription state "0x123" 'new-heads)
      (assert (eq (web3/ws-provider:ws-state-get-subscription state "0x123") 'new-heads))
      ;; Remove subscription
      (web3/ws-provider:ws-state-remove-subscription state "0x123")
      (assert (null (web3/ws-provider:ws-state-get-subscription state "0x123")))))

  (test-case "ws-state-next-request-id increments"
    (let ((state (web3/ws-provider:make-ws-connection-state :url "ws://localhost:8546")))
      (assert (= (web3/ws-provider:ws-state-next-request-id state) 1))
      (assert (= (web3/ws-provider:ws-state-next-request-id state) 2))
      (assert (= (web3/ws-provider:ws-state-next-request-id state) 3))))

  ;;; =========================================================================
  ;;; LogFilter Tests
  ;;; =========================================================================

  (test-case "make-log-filter with address only"
    (let* ((addr-result (coalton:coalton
                         (web3/address:address-from-hex
                          "0xdAC17F958D2ee523a2206206994597C13D831ec7")))
           (addr (result-value addr-result))
           (filter (coalton:coalton
                    (web3/ws-provider:make-log-filter
                     (coalton-prelude:Some (coalton:lisp web3/address:Address () addr))
                     coalton:Nil))))
      ;; Just check it was created successfully
      (assert filter)))

  (test-case "make-log-filter with topics"
    (let* ((topic-bytes (make-array 32 :fill-pointer 32 :adjustable t :initial-element #xdd)))
      (setf (aref topic-bytes 1) #xf2)
      (setf (aref topic-bytes 2) #x52)
      (setf (aref topic-bytes 3) #xad)
      (let ((filter (coalton:coalton
                     (web3/ws-provider:make-log-filter
                      coalton-prelude:None
                      (coalton:Cons
                       (coalton-prelude:Some (coalton:lisp web3/types:Bytes () topic-bytes))
                       coalton:Nil)))))
        (assert filter)))))
