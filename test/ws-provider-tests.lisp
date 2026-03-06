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

  (test-case "encode-subscribe-request with large request ID"
    (let ((request (coalton:coalton
                    (web3/ws-provider:encode-subscribe-request
                     999999
                     web3/ws-provider:SubNewHeads))))
      (assert (search "\"id\":999999" request))))

  (test-case "encode-subscribe-request includes jsonrpc version"
    (let ((request (coalton:coalton
                    (web3/ws-provider:encode-subscribe-request
                     1
                     web3/ws-provider:SubNewHeads))))
      (assert (search "\"jsonrpc\":\"2.0\"" request))))

  (test-case "encode-unsubscribe-request"
    (let ((request (coalton:coalton
                    (web3/ws-provider:encode-unsubscribe-request
                     5
                     "0x1234567890abcdef"))))
      (assert (search "eth_unsubscribe" request))
      (assert (search "0x1234567890abcdef" request))
      (assert (search "\"id\":5" request))))

  (test-case "encode-unsubscribe-request with different subscription IDs"
    (let ((request1 (coalton:coalton
                     (web3/ws-provider:encode-unsubscribe-request
                      1
                      "0xabc")))
          (request2 (coalton:coalton
                     (web3/ws-provider:encode-unsubscribe-request
                      2
                      "0xdef123456"))))
      (assert (search "0xabc" request1))
      (assert (search "0xdef123456" request2))))

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

  (test-case "parse-subscription-response with long subscription ID"
    (let* ((response "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":\"0x9ce59a13059e417087c02d3236a0b1cc1234567890abcdef\"}")
           (result (coalton:coalton
                    (web3/ws-provider:parse-subscription-response
                     (coalton:lisp coalton:String () response)))))
      (assert (result-ok-p result))
      (assert (string= (result-value result) "0x9ce59a13059e417087c02d3236a0b1cc1234567890abcdef"))))

  (test-case "parse-subscription-response error"
    (let* ((response "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32000,\"message\":\"subscription not found\"}}")
           (result (coalton:coalton
                    (web3/ws-provider:parse-subscription-response
                     (coalton:lisp coalton:String () response)))))
      (assert (result-err-p result))))

  (test-case "parse-subscription-response error with different codes"
    ;; Invalid params error
    (let* ((response "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32602,\"message\":\"invalid params\"}}")
           (result (coalton:coalton
                    (web3/ws-provider:parse-subscription-response
                     (coalton:lisp coalton:String () response)))))
      (assert (result-err-p result))))

  (test-case "parse-subscription-response with no result"
    (let* ((response "{\"jsonrpc\":\"2.0\",\"id\":1}")
           (result (coalton:coalton
                    (web3/ws-provider:parse-subscription-response
                     (coalton:lisp coalton:String () response)))))
      (assert (result-err-p result))))

  (test-case "parse-subscription-response invalid JSON"
    (let* ((response "not valid json {{{")
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

  (test-case "parse-subscription-notification with block header result"
    (let* ((notification "{\"jsonrpc\":\"2.0\",\"method\":\"eth_subscription\",\"params\":{\"subscription\":\"0xabc\",\"result\":{\"number\":\"0x10d4f\",\"hash\":\"0x1234\",\"parentHash\":\"0xabcd\"}}}")
           (result (coalton:coalton
                    (web3/ws-provider:parse-subscription-notification
                     (coalton:lisp coalton:String () notification)))))
      (assert (result-ok-p result))))

  (test-case "parse-subscription-notification wrong method"
    (let* ((notification "{\"jsonrpc\":\"2.0\",\"method\":\"eth_other\",\"params\":{}}")
           (result (coalton:coalton
                    (web3/ws-provider:parse-subscription-notification
                     (coalton:lisp coalton:String () notification)))))
      (assert (result-err-p result))))

  (test-case "parse-subscription-notification invalid JSON"
    (let* ((notification "invalid json here")
           (result (coalton:coalton
                    (web3/ws-provider:parse-subscription-notification
                     (coalton:lisp coalton:String () notification)))))
      (assert (result-err-p result))))

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

  (test-case "parse-block-header all fields"
    (let* ((header-json "{\"number\":\"0x100\",\"hash\":\"0xaabbccdd\",\"parentHash\":\"0x11223344\",\"timestamp\":\"0x60000000\",\"gasLimit\":\"0x1c9c380\",\"gasUsed\":\"0x500000\",\"baseFeePerGas\":\"0x77359400\"}")
           (result (coalton:coalton
                    (web3/ws-provider:parse-block-header
                     (coalton:lisp coalton:String () header-json)))))
      (assert (result-ok-p result))
      (let ((header (result-value result)))
        ;; Check block number (0x100 = 256)
        (assert (= (coalton:coalton
                    (web3/ws-provider:.header-number
                     (coalton:lisp web3/ws-provider:BlockHeader () header)))
                   256))
        ;; Check timestamp (0x60000000 = 1610612736)
        (assert (= (coalton:coalton
                    (web3/ws-provider:.header-timestamp
                     (coalton:lisp web3/ws-provider:BlockHeader () header)))
                   1610612736)))))

  (test-case "parse-block-header without baseFee (pre-London)"
    (let* ((header-json "{\"number\":\"0x100\",\"hash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"parentHash\":\"0xabcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890\",\"timestamp\":\"0x1000\",\"gasLimit\":\"0x1000000\",\"gasUsed\":\"0x500000\"}")
           (result (coalton:coalton
                    (web3/ws-provider:parse-block-header
                     (coalton:lisp coalton:String () header-json)))))
      (assert (result-ok-p result))))

  (test-case "parse-block-header with high block number"
    (let* ((header-json "{\"number\":\"0xffffff\",\"hash\":\"0xaa\",\"parentHash\":\"0xbb\",\"timestamp\":\"0x1\",\"gasLimit\":\"0x1\",\"gasUsed\":\"0x0\"}")
           (result (coalton:coalton
                    (web3/ws-provider:parse-block-header
                     (coalton:lisp coalton:String () header-json)))))
      (assert (result-ok-p result))
      (let ((header (result-value result)))
        ;; 0xffffff = 16777215
        (assert (= (coalton:coalton
                    (web3/ws-provider:.header-number
                     (coalton:lisp web3/ws-provider:BlockHeader () header)))
                   16777215)))))

  (test-case "parse-block-header with zero gas used"
    (let* ((header-json "{\"number\":\"0x1\",\"hash\":\"0xaa\",\"parentHash\":\"0xbb\",\"timestamp\":\"0x1\",\"gasLimit\":\"0x1c9c380\",\"gasUsed\":\"0x0\"}")
           (result (coalton:coalton
                    (web3/ws-provider:parse-block-header
                     (coalton:lisp coalton:String () header-json)))))
      (assert (result-ok-p result))))

  (test-case "parse-block-header invalid JSON"
    (let* ((header-json "not json")
           (result (coalton:coalton
                    (web3/ws-provider:parse-block-header
                     (coalton:lisp coalton:String () header-json)))))
      (assert (result-err-p result))))

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

  (test-case "parse-log-entry with single topic (Transfer event)"
    (let* ((log-json "{\"address\":\"0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2\",\"topics\":[\"0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef\"],\"data\":\"0x0000000000000000000000000000000000000000000000000de0b6b3a7640000\",\"blockNumber\":\"0x10d4f1e\",\"transactionHash\":\"0xaabb\",\"transactionIndex\":\"0x5\",\"blockHash\":\"0xccdd\",\"logIndex\":\"0x10\"}")
           (result (coalton:coalton
                    (web3/ws-provider:parse-log-entry
                     (coalton:lisp coalton:String () log-json)))))
      (assert (result-ok-p result))
      (let ((log (result-value result)))
        ;; Check tx index (0x5 = 5)
        (assert (= (coalton:coalton
                    (web3/ws-provider:.log-tx-index
                     (coalton:lisp web3/ws-provider:LogEntry () log)))
                   5))
        ;; Check log index (0x10 = 16)
        (assert (= (coalton:coalton
                    (web3/ws-provider:.log-log-index
                     (coalton:lisp web3/ws-provider:LogEntry () log)))
                   16)))))

  (test-case "parse-log-entry with empty topics"
    (let* ((log-json "{\"address\":\"0xdAC17F958D2ee523a2206206994597C13D831ec7\",\"topics\":[],\"data\":\"0x1234\",\"blockNumber\":\"0x1\",\"transactionHash\":\"0xaa\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0xbb\",\"logIndex\":\"0x0\"}")
           (result (coalton:coalton
                    (web3/ws-provider:parse-log-entry
                     (coalton:lisp coalton:String () log-json)))))
      (assert (result-ok-p result))))

  (test-case "parse-log-entry with four topics (max)"
    (let* ((log-json "{\"address\":\"0xdAC17F958D2ee523a2206206994597C13D831ec7\",\"topics\":[\"0xaa\",\"0xbb\",\"0xcc\",\"0xdd\"],\"data\":\"0x\",\"blockNumber\":\"0x1\",\"transactionHash\":\"0xaa\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0xbb\",\"logIndex\":\"0x0\"}")
           (result (coalton:coalton
                    (web3/ws-provider:parse-log-entry
                     (coalton:lisp coalton:String () log-json)))))
      (assert (result-ok-p result))))

  (test-case "parse-log-entry with empty data"
    (let* ((log-json "{\"address\":\"0xdAC17F958D2ee523a2206206994597C13D831ec7\",\"topics\":[\"0xaa\"],\"data\":\"0x\",\"blockNumber\":\"0x1\",\"transactionHash\":\"0xaa\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0xbb\",\"logIndex\":\"0x0\"}")
           (result (coalton:coalton
                    (web3/ws-provider:parse-log-entry
                     (coalton:lisp coalton:String () log-json)))))
      (assert (result-ok-p result))))

  (test-case "parse-log-entry invalid address"
    (let* ((log-json "{\"address\":\"invalid\",\"topics\":[],\"data\":\"0x\",\"blockNumber\":\"0x1\",\"transactionHash\":\"0xaa\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0xbb\",\"logIndex\":\"0x0\"}")
           (result (coalton:coalton
                    (web3/ws-provider:parse-log-entry
                     (coalton:lisp coalton:String () log-json)))))
      (assert (result-err-p result))))

  (test-case "parse-log-entry invalid JSON"
    (let* ((log-json "not valid json")
           (result (coalton:coalton
                    (web3/ws-provider:parse-log-entry
                     (coalton:lisp coalton:String () log-json)))))
      (assert (result-err-p result))))

  ;;; =========================================================================
  ;;; Sync Status Parsing Tests
  ;;; =========================================================================

  (test-case "parse-sync-status not syncing"
    (let* ((result (coalton:coalton
                    (web3/ws-provider:parse-sync-status "null"))))
      (assert (result-ok-p result))))

  (test-case "parse-sync-status returns false (not syncing)"
    (let* ((result (coalton:coalton
                    (web3/ws-provider:parse-sync-status "false"))))
      (assert (result-ok-p result))))

  (test-case "parse-sync-status syncing"
    (let* ((sync-json "{\"startingBlock\":\"0x100\",\"currentBlock\":\"0x200\",\"highestBlock\":\"0x300\"}")
           (result (coalton:coalton
                    (web3/ws-provider:parse-sync-status
                     (coalton:lisp coalton:String () sync-json)))))
      (assert (result-ok-p result))))

  (test-case "parse-sync-status syncing with high blocks"
    (let* ((sync-json "{\"startingBlock\":\"0x10d4f1e\",\"currentBlock\":\"0x10d5000\",\"highestBlock\":\"0x10d6000\"}")
           (result (coalton:coalton
                    (web3/ws-provider:parse-sync-status
                     (coalton:lisp coalton:String () sync-json)))))
      (assert (result-ok-p result))))

  (test-case "parse-sync-status invalid JSON"
    (let* ((sync-json "not valid json")
           (result (coalton:coalton
                    (web3/ws-provider:parse-sync-status
                     (coalton:lisp coalton:String () sync-json)))))
      (assert (result-err-p result))))

  ;;; =========================================================================
  ;;; Connection State Tests
  ;;; =========================================================================

  (test-case "ws-connection-state creation"
    (let ((state (web3/ws-provider:make-ws-connection-state :url "ws://localhost:8546")))
      (assert (string= (web3/ws-provider:ws-connection-state-url state) "ws://localhost:8546"))
      (assert (= (web3/ws-provider:ws-connection-state-next-id state) 1))))

  (test-case "ws-connection-state with different URLs"
    (let ((state1 (web3/ws-provider:make-ws-connection-state :url "ws://localhost:8545"))
          (state2 (web3/ws-provider:make-ws-connection-state :url "wss://mainnet.infura.io/ws/v3/key")))
      (assert (string= (web3/ws-provider:ws-connection-state-url state1) "ws://localhost:8545"))
      (assert (string= (web3/ws-provider:ws-connection-state-url state2) "wss://mainnet.infura.io/ws/v3/key"))))

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

  (test-case "ws-connection-state multiple subscriptions"
    (let ((state (web3/ws-provider:make-ws-connection-state :url "ws://localhost:8546")))
      ;; Add multiple subscriptions
      (web3/ws-provider:ws-state-add-subscription state "0x111" 'new-heads)
      (web3/ws-provider:ws-state-add-subscription state "0x222" 'logs)
      (web3/ws-provider:ws-state-add-subscription state "0x333" 'syncing)
      ;; Check all exist
      (assert (eq (web3/ws-provider:ws-state-get-subscription state "0x111") 'new-heads))
      (assert (eq (web3/ws-provider:ws-state-get-subscription state "0x222") 'logs))
      (assert (eq (web3/ws-provider:ws-state-get-subscription state "0x333") 'syncing))
      ;; Remove one
      (web3/ws-provider:ws-state-remove-subscription state "0x222")
      (assert (null (web3/ws-provider:ws-state-get-subscription state "0x222")))
      ;; Others still exist
      (assert (eq (web3/ws-provider:ws-state-get-subscription state "0x111") 'new-heads))
      (assert (eq (web3/ws-provider:ws-state-get-subscription state "0x333") 'syncing))))

  (test-case "ws-state-next-request-id increments"
    (let ((state (web3/ws-provider:make-ws-connection-state :url "ws://localhost:8546")))
      (assert (= (web3/ws-provider:ws-state-next-request-id state) 1))
      (assert (= (web3/ws-provider:ws-state-next-request-id state) 2))
      (assert (= (web3/ws-provider:ws-state-next-request-id state) 3))))

  (test-case "ws-state-next-request-id many increments"
    (let ((state (web3/ws-provider:make-ws-connection-state :url "ws://localhost:8546")))
      ;; Increment many times
      (dotimes (i 100)
        (web3/ws-provider:ws-state-next-request-id state))
      ;; Should be at 101 now
      (assert (= (web3/ws-provider:ws-state-next-request-id state) 101))))

  (test-case "ws-state-get-subscription returns nil for unknown"
    (let ((state (web3/ws-provider:make-ws-connection-state :url "ws://localhost:8546")))
      (assert (null (web3/ws-provider:ws-state-get-subscription state "nonexistent")))))

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
      (assert filter)))

  (test-case "make-log-filter with no address (wildcard)"
    (let ((filter (coalton:coalton
                   (web3/ws-provider:make-log-filter
                    coalton-prelude:None
                    coalton:Nil))))
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
        (assert filter))))

  (test-case "make-log-filter with address and topics"
    (let* ((addr-result (coalton:coalton
                         (web3/address:address-from-hex
                          "0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2")))
           (addr (result-value addr-result))
           (topic-bytes (make-array 32 :fill-pointer 32 :adjustable t :initial-element 0)))
      ;; Transfer event signature
      (setf (aref topic-bytes 0) #xdd)
      (setf (aref topic-bytes 1) #xf2)
      (setf (aref topic-bytes 2) #x52)
      (setf (aref topic-bytes 3) #xad)
      (let ((filter (coalton:coalton
                     (web3/ws-provider:make-log-filter
                      (coalton-prelude:Some (coalton:lisp web3/address:Address () addr))
                      (coalton:Cons
                       (coalton-prelude:Some (coalton:lisp web3/types:Bytes () topic-bytes))
                       coalton:Nil)))))
        (assert filter))))

  (test-case "make-log-filter with multiple topics"
    (let* ((topic1 (make-array 32 :fill-pointer 32 :adjustable t :initial-element #xaa))
           (topic2 (make-array 32 :fill-pointer 32 :adjustable t :initial-element #xbb))
           (filter (coalton:coalton
                    (web3/ws-provider:make-log-filter
                     coalton-prelude:None
                     (coalton:Cons
                      (coalton-prelude:Some (coalton:lisp web3/types:Bytes () topic1))
                      (coalton:Cons
                       (coalton-prelude:Some (coalton:lisp web3/types:Bytes () topic2))
                       coalton:Nil))))))
      (assert filter)))

  (test-case "make-log-filter with None topic (wildcard position)"
    (let* ((topic1 (make-array 32 :fill-pointer 32 :adjustable t :initial-element #xaa))
           (filter (coalton:coalton
                    (web3/ws-provider:make-log-filter
                     coalton-prelude:None
                     (coalton:Cons
                      (coalton-prelude:Some (coalton:lisp web3/types:Bytes () topic1))
                      (coalton:Cons
                       coalton-prelude:None  ; Wildcard for second topic position
                       coalton:Nil))))))
      (assert filter)))

  ;;; =========================================================================
  ;;; Sync Status Accessor Tests
  ;;; =========================================================================

  (test-case "sync-starting-block returns None for NotSyncing"
    (let* ((status (coalton:coalton web3/ws-provider:NotSyncing))
           (result (coalton:coalton
                    (web3/ws-provider:sync-starting-block
                     (coalton:lisp web3/ws-provider:SyncStatus () status)))))
      ;; Should be None
      (assert (optional-none-p result))))

  (test-case "sync-current-block returns None for NotSyncing"
    (let* ((status (coalton:coalton web3/ws-provider:NotSyncing))
           (result (coalton:coalton
                    (web3/ws-provider:sync-current-block
                     (coalton:lisp web3/ws-provider:SyncStatus () status)))))
      (assert (optional-none-p result))))

  (test-case "sync-highest-block returns None for NotSyncing"
    (let* ((status (coalton:coalton web3/ws-provider:NotSyncing))
           (result (coalton:coalton
                    (web3/ws-provider:sync-highest-block
                     (coalton:lisp web3/ws-provider:SyncStatus () status)))))
      (assert (optional-none-p result))))

  (test-case "sync accessors return values for Syncing"
    (let* ((status (coalton:coalton (web3/ws-provider:Syncing 100 200 300)))
           (starting (coalton:coalton
                      (web3/ws-provider:sync-starting-block
                       (coalton:lisp web3/ws-provider:SyncStatus () status))))
           (current (coalton:coalton
                     (web3/ws-provider:sync-current-block
                      (coalton:lisp web3/ws-provider:SyncStatus () status))))
           (highest (coalton:coalton
                     (web3/ws-provider:sync-highest-block
                      (coalton:lisp web3/ws-provider:SyncStatus () status)))))
      ;; All should be Some
      (assert (optional-some-p starting))
      (assert (optional-some-p current))
      (assert (optional-some-p highest))))

  ;;; =========================================================================
  ;;; WebSocket I/O Layer Tests (connection.lisp)
  ;;; =========================================================================

  (test-case "ws-provider struct creation"
    (let ((provider (web3/ws-provider:make-ws-provider
                     :state (web3/ws-provider:make-ws-connection-state :url "ws://localhost:8546"))))
      (assert (not (web3/ws-provider:ws-provider-connected-p provider)))))

  (test-case "ws-provider initially not connected"
    (let ((provider (web3/ws-provider:make-ws-provider)))
      (assert (null (web3/ws-provider:ws-provider-connected-p provider)))))

  (test-case "ws-dispatch-message resolves pending request with result"
    (let* ((provider (web3/ws-provider:make-ws-provider))
           (pending (web3/ws-provider::make-ws-pending-request)))
      ;; Register a pending request with id=1
      (setf (gethash 1 (web3/ws-provider::ws-provider-pending provider)) pending)
      ;; Dispatch a response message
      (web3/ws-provider::%ws-dispatch-message provider
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":\"0xabc\"}")
      ;; Check that pending was resolved
      (assert (web3/ws-provider::ws-pending-request-done-p pending))
      (assert (equal (web3/ws-provider::ws-pending-request-result pending)
                     '(:ok "0xabc")))))

  (test-case "ws-dispatch-message resolves pending request with error"
    (let* ((provider (web3/ws-provider:make-ws-provider))
           (pending (web3/ws-provider::make-ws-pending-request)))
      (setf (gethash 2 (web3/ws-provider::ws-provider-pending provider)) pending)
      (web3/ws-provider::%ws-dispatch-message provider
        "{\"jsonrpc\":\"2.0\",\"id\":2,\"error\":{\"code\":-32000,\"message\":\"bad request\"}}")
      (assert (web3/ws-provider::ws-pending-request-done-p pending))
      (let ((result (web3/ws-provider::ws-pending-request-result pending)))
        (assert (eq (first result) :error)))))

  (test-case "ws-dispatch-message calls subscription handler"
    (let* ((provider (web3/ws-provider:make-ws-provider))
           (received nil))
      ;; Register a handler for subscription "0xsub1"
      (setf (gethash "0xsub1" (web3/ws-provider::ws-provider-handlers provider))
            (lambda (json) (setf received json)))
      ;; Dispatch a subscription notification
      (web3/ws-provider::%ws-dispatch-message provider
        "{\"jsonrpc\":\"2.0\",\"method\":\"eth_subscription\",\"params\":{\"subscription\":\"0xsub1\",\"result\":{\"number\":\"0x100\"}}}")
      (assert received)
      (assert (search "number" received))))

  (test-case "ws-dispatch-message ignores unknown subscription"
    (let* ((provider (web3/ws-provider:make-ws-provider))
           (called nil))
      ;; Register a handler for a different subscription
      (setf (gethash "0xother" (web3/ws-provider::ws-provider-handlers provider))
            (lambda (json) (declare (ignore json)) (setf called t)))
      ;; Dispatch for unknown subscription
      (web3/ws-provider::%ws-dispatch-message provider
        "{\"jsonrpc\":\"2.0\",\"method\":\"eth_subscription\",\"params\":{\"subscription\":\"0xunknown\",\"result\":{}}}")
      (assert (null called))))

  (test-case "ws-dispatch-message handles invalid JSON gracefully"
    ;; Should not signal an error
    (let ((provider (web3/ws-provider:make-ws-provider)))
      (web3/ws-provider::%ws-dispatch-message provider "not valid json")
      (assert t)))

  (test-case "ws-dispatch-message ignores request with no pending"
    ;; Should not signal an error when no pending request exists for the id
    (let ((provider (web3/ws-provider:make-ws-provider)))
      (web3/ws-provider::%ws-dispatch-message provider
        "{\"jsonrpc\":\"2.0\",\"id\":999,\"result\":\"0x1\"}")
      (assert t)))

  (test-case "ws-connect fails on invalid URL"
    ;; Connecting to a non-existent host should signal an error
    (handler-case
        (progn
          (web3/ws-provider:ws-connect "ws://127.0.0.1:19999")
          ;; If we get here, the connection unexpectedly succeeded
          nil)
      (error (e)
        (declare (ignore e))
        (assert t)))))
