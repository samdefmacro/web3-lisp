;;; Receipt Parsing tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Receipt Parsing Tests
;;; =========================================================================

(defun run-receipt-tests ()
  (format t "~%=== Receipt Parsing Tests ===~%")

  ;;; =========================================================================
  ;;; Log Entry Parsing Tests
  ;;; =========================================================================

  (test-case "parse-log-entry parses valid log"
    (let* ((log-json "{\"address\":\"0xdac17f958d2ee523a2206206994597c13d831ec7\",\"topics\":[\"0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef\",\"0x000000000000000000000000a9d1e08c7793af67e9d92fe308d5697fb81d3e43\",\"0x00000000000000000000000028c6c06298d514db089934071355e5743bf21d60\"],\"data\":\"0x00000000000000000000000000000000000000000000000000000000773594c0\",\"blockNumber\":\"0x124f1c0\",\"transactionHash\":\"0xabc123\",\"transactionIndex\":\"0x5\",\"blockHash\":\"0xdef456\",\"logIndex\":\"0xa\",\"removed\":false}")
           (result (coalton:coalton
                    (web3/receipt:parse-log-entry
                     (coalton:lisp coalton:String () log-json)))))
      (assert (result-ok-p result))
      (let ((log (result-value result)))
        ;; Check address parsed correctly (USDT contract)
        (assert log)
        ;; Check we have 3 topics
        (let ((topics (coalton:coalton
                       (web3/receipt:.log-topics
                        (coalton:lisp web3/receipt:LogEntry () log)))))
          (assert (= (coalton:coalton
                      (coalton-library/list:length
                       (coalton:lisp (coalton:List web3/types:Bytes) () topics)))
                     3)))
        ;; Check block number
        (let ((block-num (coalton:coalton
                          (web3/receipt:.log-block-number
                           (coalton:lisp web3/receipt:LogEntry () log)))))
          (assert (= block-num #x124f1c0)))
        ;; Check log index
        (let ((log-idx (coalton:coalton
                        (web3/receipt:.log-log-index
                         (coalton:lisp web3/receipt:LogEntry () log)))))
          (assert (= log-idx 10)))
        ;; Check removed is false
        (let ((removed (coalton:coalton
                        (web3/receipt:.log-removed
                         (coalton:lisp web3/receipt:LogEntry () log)))))
          (assert (eq removed nil))))))

  (test-case "parse-log-entry with removed=true"
    (let* ((log-json "{\"address\":\"0x1234567890123456789012345678901234567890\",\"topics\":[],\"data\":\"0x\",\"blockNumber\":\"0x1\",\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"logIndex\":\"0x0\",\"removed\":true}")
           (result (coalton:coalton
                    (web3/receipt:parse-log-entry
                     (coalton:lisp coalton:String () log-json)))))
      (assert (result-ok-p result))
      (let* ((log (result-value result))
             (removed (coalton:coalton
                       (web3/receipt:.log-removed
                        (coalton:lisp web3/receipt:LogEntry () log)))))
        (assert (eq removed t)))))

  ;;; =========================================================================
  ;;; Receipt Parsing Tests
  ;;; =========================================================================

  (test-case "parse-receipt parses successful EIP-1559 transaction"
    (let* ((receipt-json "{\"transactionHash\":\"0x88df016429689c079f3b2f6ad39fa052532c56795b733da78a91ebe6a713944b\",\"transactionIndex\":\"0x5\",\"blockHash\":\"0x1d59ff54b1eb26b013ce3cb5fc9dab3705b415a67127a003c3e61eb445bb8df2\",\"blockNumber\":\"0x5daf3b\",\"from\":\"0xa7d9ddbe1f17865597fbd27ec712455208b6b76d\",\"to\":\"0xf02c1c8e6114b1dbe8937a39260b5b0a374432bb\",\"cumulativeGasUsed\":\"0x33bc\",\"gasUsed\":\"0x4dc\",\"effectiveGasPrice\":\"0x4a817c800\",\"contractAddress\":null,\"logs\":[],\"logsBloom\":\"0x00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\",\"status\":\"0x1\",\"type\":\"0x2\"}")
           (result (coalton:coalton
                    (web3/receipt:parse-receipt
                     (coalton:lisp coalton:String () receipt-json)))))
      (assert (result-ok-p result))
      (let ((receipt (result-value result)))
        ;; Check status is success
        (let ((success (coalton:coalton
                        (web3/receipt:receipt-success?
                         (coalton:lisp web3/receipt:Receipt () receipt)))))
          (assert (eq success t)))
        ;; Check it's not a contract creation
        (let ((is-contract (coalton:coalton
                            (web3/receipt:receipt-contract-created?
                             (coalton:lisp web3/receipt:Receipt () receipt)))))
          (assert (eq is-contract nil)))
        ;; Check gas used
        (let ((gas-used (coalton:coalton
                         (web3/receipt:.receipt-gas-used
                          (coalton:lisp web3/receipt:Receipt () receipt)))))
          (assert (= gas-used #x4dc)))
        ;; Check block number
        (let ((block-num (coalton:coalton
                          (web3/receipt:.receipt-block-number
                           (coalton:lisp web3/receipt:Receipt () receipt)))))
          (assert (= block-num #x5daf3b)))
        ;; Check transaction index
        (let ((tx-idx (coalton:coalton
                       (web3/receipt:.receipt-transaction-index
                        (coalton:lisp web3/receipt:Receipt () receipt)))))
          (assert (= tx-idx 5))))))

  (test-case "parse-receipt parses failed transaction"
    (let* ((receipt-json "{\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"blockNumber\":\"0x100\",\"from\":\"0xa7d9ddbe1f17865597fbd27ec712455208b6b76d\",\"to\":\"0xf02c1c8e6114b1dbe8937a39260b5b0a374432bb\",\"cumulativeGasUsed\":\"0x5208\",\"gasUsed\":\"0x5208\",\"effectiveGasPrice\":\"0x1\",\"contractAddress\":null,\"logs\":[],\"logsBloom\":\"0x\",\"status\":\"0x0\",\"type\":\"0x0\"}")
           (result (coalton:coalton
                    (web3/receipt:parse-receipt
                     (coalton:lisp coalton:String () receipt-json)))))
      (assert (result-ok-p result))
      (let ((receipt (result-value result)))
        ;; Check status is failed
        (let ((failed (coalton:coalton
                       (web3/receipt:receipt-failed?
                        (coalton:lisp web3/receipt:Receipt () receipt)))))
          (assert (eq failed t)))
        (let ((success (coalton:coalton
                        (web3/receipt:receipt-success?
                         (coalton:lisp web3/receipt:Receipt () receipt)))))
          (assert (eq success nil))))))

  (test-case "parse-receipt parses contract creation"
    (let* ((receipt-json "{\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000003\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000004\",\"blockNumber\":\"0x100\",\"from\":\"0xa7d9ddbe1f17865597fbd27ec712455208b6b76d\",\"to\":null,\"cumulativeGasUsed\":\"0x100000\",\"gasUsed\":\"0x100000\",\"effectiveGasPrice\":\"0x1\",\"contractAddress\":\"0x1234567890123456789012345678901234567890\",\"logs\":[],\"logsBloom\":\"0x\",\"status\":\"0x1\",\"type\":\"0x2\"}")
           (result (coalton:coalton
                    (web3/receipt:parse-receipt
                     (coalton:lisp coalton:String () receipt-json)))))
      (assert (result-ok-p result))
      (let ((receipt (result-value result)))
        ;; Check it's a contract creation
        (let ((is-contract (coalton:coalton
                            (web3/receipt:receipt-contract-created?
                             (coalton:lisp web3/receipt:Receipt () receipt)))))
          (assert (eq is-contract t)))
        ;; Check 'to' is None
        (let ((to-addr (coalton:coalton
                        (web3/receipt:.receipt-to
                         (coalton:lisp web3/receipt:Receipt () receipt)))))
          (assert (optional-none-p to-addr))))))

  (test-case "parse-receipt with logs"
    (let* ((receipt-json "{\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000005\",\"transactionIndex\":\"0x1\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000006\",\"blockNumber\":\"0x200\",\"from\":\"0xa7d9ddbe1f17865597fbd27ec712455208b6b76d\",\"to\":\"0xdac17f958d2ee523a2206206994597c13d831ec7\",\"cumulativeGasUsed\":\"0x10000\",\"gasUsed\":\"0x8000\",\"effectiveGasPrice\":\"0x2540be400\",\"contractAddress\":null,\"logs\":[{\"address\":\"0xdac17f958d2ee523a2206206994597c13d831ec7\",\"topics\":[\"0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef\"],\"data\":\"0x0000000000000000000000000000000000000000000000000000000000000064\",\"blockNumber\":\"0x200\",\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000005\",\"transactionIndex\":\"0x1\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000006\",\"logIndex\":\"0x0\",\"removed\":false},{\"address\":\"0xdac17f958d2ee523a2206206994597c13d831ec7\",\"topics\":[\"0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef\"],\"data\":\"0x00000000000000000000000000000000000000000000000000000000000000c8\",\"blockNumber\":\"0x200\",\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000005\",\"transactionIndex\":\"0x1\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000006\",\"logIndex\":\"0x1\",\"removed\":false}],\"logsBloom\":\"0x\",\"status\":\"0x1\",\"type\":\"0x2\"}")
           (result (coalton:coalton
                    (web3/receipt:parse-receipt
                     (coalton:lisp coalton:String () receipt-json)))))
      (assert (result-ok-p result))
      (let* ((receipt (result-value result))
             (logs (coalton:coalton
                    (web3/receipt:.receipt-logs
                     (coalton:lisp web3/receipt:Receipt () receipt))))
             (log-count (coalton:coalton
                         (coalton-library/list:length
                          (coalton:lisp (coalton:List web3/receipt:LogEntry) () logs)))))
        ;; Should have 2 logs
        (assert (= log-count 2)))))

  ;;; =========================================================================
  ;;; Transaction Type Tests
  ;;; =========================================================================

  (test-case "parse-receipt legacy transaction type"
    (let* ((receipt-json "{\"transactionHash\":\"0x1\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x2\",\"blockNumber\":\"0x1\",\"from\":\"0xa7d9ddbe1f17865597fbd27ec712455208b6b76d\",\"to\":\"0xf02c1c8e6114b1dbe8937a39260b5b0a374432bb\",\"cumulativeGasUsed\":\"0x5208\",\"gasUsed\":\"0x5208\",\"effectiveGasPrice\":\"0x1\",\"contractAddress\":null,\"logs\":[],\"logsBloom\":\"0x\",\"status\":\"0x1\",\"type\":\"0x0\"}")
           (result (coalton:coalton
                    (web3/receipt:parse-receipt
                     (coalton:lisp coalton:String () receipt-json)))))
      (assert (result-ok-p result))))

  (test-case "parse-receipt access list transaction type"
    (let* ((receipt-json "{\"transactionHash\":\"0x1\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x2\",\"blockNumber\":\"0x1\",\"from\":\"0xa7d9ddbe1f17865597fbd27ec712455208b6b76d\",\"to\":\"0xf02c1c8e6114b1dbe8937a39260b5b0a374432bb\",\"cumulativeGasUsed\":\"0x5208\",\"gasUsed\":\"0x5208\",\"effectiveGasPrice\":\"0x1\",\"contractAddress\":null,\"logs\":[],\"logsBloom\":\"0x\",\"status\":\"0x1\",\"type\":\"0x1\"}")
           (result (coalton:coalton
                    (web3/receipt:parse-receipt
                     (coalton:lisp coalton:String () receipt-json)))))
      (assert (result-ok-p result))))

  (test-case "parse-receipt blob transaction type"
    (let* ((receipt-json "{\"transactionHash\":\"0x1\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x2\",\"blockNumber\":\"0x1\",\"from\":\"0xa7d9ddbe1f17865597fbd27ec712455208b6b76d\",\"to\":\"0xf02c1c8e6114b1dbe8937a39260b5b0a374432bb\",\"cumulativeGasUsed\":\"0x5208\",\"gasUsed\":\"0x5208\",\"effectiveGasPrice\":\"0x1\",\"contractAddress\":null,\"logs\":[],\"logsBloom\":\"0x\",\"status\":\"0x1\",\"type\":\"0x3\"}")
           (result (coalton:coalton
                    (web3/receipt:parse-receipt
                     (coalton:lisp coalton:String () receipt-json)))))
      (assert (result-ok-p result))))

  ;;; =========================================================================
  ;;; Receipt Query Helpers Tests
  ;;; =========================================================================

  (test-case "receipt-total-cost calculates correctly"
    (let* ((receipt-json "{\"transactionHash\":\"0x1\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x2\",\"blockNumber\":\"0x1\",\"from\":\"0xa7d9ddbe1f17865597fbd27ec712455208b6b76d\",\"to\":\"0xf02c1c8e6114b1dbe8937a39260b5b0a374432bb\",\"cumulativeGasUsed\":\"0x5208\",\"gasUsed\":\"0x5208\",\"effectiveGasPrice\":\"0x2540be400\",\"contractAddress\":null,\"logs\":[],\"logsBloom\":\"0x\",\"status\":\"0x1\",\"type\":\"0x2\"}")
           (result (coalton:coalton
                    (web3/receipt:parse-receipt
                     (coalton:lisp coalton:String () receipt-json)))))
      (assert (result-ok-p result))
      (let* ((receipt (result-value result))
             ;; gas-used = 0x5208 = 21000
             ;; effective-gas-price = 0x2540be400 = 10 gwei = 10000000000 wei
             ;; total cost = 21000 * 10000000000 = 210000000000000 wei
             (cost (coalton:coalton
                    (web3/receipt:receipt-total-cost
                     (coalton:lisp web3/receipt:Receipt () receipt))))
             (cost-int (coalton:coalton
                        (web3/types:u256-to-integer
                         (coalton:lisp web3/types:U256 () cost)))))
        (assert (= cost-int 210000000000000)))))

  ;;; =========================================================================
  ;;; JSON-RPC Helpers Tests
  ;;; =========================================================================

  (test-case "encode-get-receipt-request creates valid params"
    (let* ((tx-hash (make-array 32 :element-type 't :fill-pointer 32 :adjustable t :initial-element #xab))
           (request (coalton:coalton
                     (web3/receipt:encode-get-receipt-request
                      (coalton:lisp web3/types:Bytes () tx-hash)))))
      (assert (search "0xab" request))))

  (test-case "parse-get-receipt-response with null result"
    (let* ((response "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":null}")
           (result (coalton:coalton
                    (web3/receipt:parse-get-receipt-response
                     (coalton:lisp coalton:String () response)))))
      (assert (result-ok-p result))
      ;; Should return None since transaction is pending/not found
      (let ((maybe-receipt (result-value result)))
        (assert (optional-none-p maybe-receipt)))))

  (test-case "parse-get-receipt-response with receipt"
    (let* ((response "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"transactionHash\":\"0x1\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x2\",\"blockNumber\":\"0x1\",\"from\":\"0xa7d9ddbe1f17865597fbd27ec712455208b6b76d\",\"to\":\"0xf02c1c8e6114b1dbe8937a39260b5b0a374432bb\",\"cumulativeGasUsed\":\"0x5208\",\"gasUsed\":\"0x5208\",\"effectiveGasPrice\":\"0x1\",\"contractAddress\":null,\"logs\":[],\"logsBloom\":\"0x\",\"status\":\"0x1\",\"type\":\"0x2\"}}")
           (result (coalton:coalton
                    (web3/receipt:parse-get-receipt-response
                     (coalton:lisp coalton:String () response)))))
      (assert (result-ok-p result))
      ;; Should return Some receipt
      (let ((maybe-receipt (result-value result)))
        (assert (not (eq maybe-receipt nil))))))

  ;;; =========================================================================
  ;;; Filter Helpers Tests
  ;;; =========================================================================

  (test-case "filter-logs-by-topic filters correctly"
    (let* ((receipt-json "{\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000007\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000008\",\"blockNumber\":\"0x1\",\"from\":\"0xa7d9ddbe1f17865597fbd27ec712455208b6b76d\",\"to\":\"0xdac17f958d2ee523a2206206994597c13d831ec7\",\"cumulativeGasUsed\":\"0x10000\",\"gasUsed\":\"0x8000\",\"effectiveGasPrice\":\"0x1\",\"contractAddress\":null,\"logs\":[{\"address\":\"0xdac17f958d2ee523a2206206994597c13d831ec7\",\"topics\":[\"0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef\"],\"data\":\"0x\",\"blockNumber\":\"0x1\",\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000007\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000008\",\"logIndex\":\"0x0\",\"removed\":false},{\"address\":\"0xdac17f958d2ee523a2206206994597c13d831ec7\",\"topics\":[\"0x8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925\"],\"data\":\"0x\",\"blockNumber\":\"0x1\",\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000007\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000008\",\"logIndex\":\"0x1\",\"removed\":false}],\"logsBloom\":\"0x\",\"status\":\"0x1\",\"type\":\"0x0\"}")
           (result (coalton:coalton
                    (web3/receipt:parse-receipt
                     (coalton:lisp coalton:String () receipt-json)))))
      (assert (result-ok-p result))
      (let* ((receipt (result-value result))
             (logs (coalton:coalton
                    (web3/receipt:.receipt-logs
                     (coalton:lisp web3/receipt:Receipt () receipt))))
             ;; Transfer topic: 0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef
             ;; Parse the topic hex string
             (transfer-topic-result (coalton:coalton
                                     (web3/types:hex-decode
                                      "ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef")))
             (transfer-topic (result-value transfer-topic-result))
             (filtered (coalton:coalton
                        (web3/receipt:filter-logs-by-topic
                         (coalton:lisp (coalton:List web3/receipt:LogEntry) () logs)
                         (coalton:lisp web3/types:Bytes () transfer-topic)))))
        ;; Should find 1 Transfer log (the other is Approval)
        (let ((count (coalton:coalton
                      (coalton-library/list:length
                       (coalton:lisp (coalton:List web3/receipt:LogEntry) () filtered)))))
          (assert (= count 1))))))

  ;;; =========================================================================
  ;;; Additional LogEntry Accessor Tests
  ;;; =========================================================================

  (test-case "log-data accessor returns correct data"
    (let* ((log-json "{\"address\":\"0xdac17f958d2ee523a2206206994597c13d831ec7\",\"topics\":[],\"data\":\"0xdeadbeef\",\"blockNumber\":\"0x1\",\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"logIndex\":\"0x0\",\"removed\":false}")
           (result (coalton:coalton
                    (web3/receipt:parse-log-entry
                     (coalton:lisp coalton:String () log-json)))))
      (assert (result-ok-p result))
      (let* ((log (result-value result))
             (data (coalton:coalton
                    (web3/receipt:.log-data
                     (coalton:lisp web3/receipt:LogEntry () log)))))
        ;; 0xdeadbeef = 4 bytes
        (assert (= (length data) 4)))))

  (test-case "log-transaction-hash accessor returns correct hash"
    (let* ((log-json "{\"address\":\"0xdac17f958d2ee523a2206206994597c13d831ec7\",\"topics\":[],\"data\":\"0x\",\"blockNumber\":\"0x1\",\"transactionHash\":\"0xabcdef0000000000000000000000000000000000000000000000000000000123\",\"transactionIndex\":\"0x5\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"logIndex\":\"0x0\",\"removed\":false}")
           (result (coalton:coalton
                    (web3/receipt:parse-log-entry
                     (coalton:lisp coalton:String () log-json)))))
      (assert (result-ok-p result))
      (let* ((log (result-value result))
             (tx-hash (coalton:coalton
                       (web3/receipt:.log-transaction-hash
                        (coalton:lisp web3/receipt:LogEntry () log)))))
        ;; Should be 32 bytes
        (assert (= (length tx-hash) 32))
        ;; First byte should be 0xab
        (assert (= (aref tx-hash 0) #xab)))))

  (test-case "log-transaction-index accessor returns correct index"
    (let* ((log-json "{\"address\":\"0xdac17f958d2ee523a2206206994597c13d831ec7\",\"topics\":[],\"data\":\"0x\",\"blockNumber\":\"0x1\",\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"transactionIndex\":\"0xff\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"logIndex\":\"0x0\",\"removed\":false}")
           (result (coalton:coalton
                    (web3/receipt:parse-log-entry
                     (coalton:lisp coalton:String () log-json)))))
      (assert (result-ok-p result))
      (let* ((log (result-value result))
             (tx-idx (coalton:coalton
                      (web3/receipt:.log-transaction-index
                       (coalton:lisp web3/receipt:LogEntry () log)))))
        (assert (= tx-idx 255)))))

  (test-case "log-block-hash accessor returns correct hash"
    (let* ((log-json "{\"address\":\"0xdac17f958d2ee523a2206206994597c13d831ec7\",\"topics\":[],\"data\":\"0x\",\"blockNumber\":\"0x1\",\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x1234560000000000000000000000000000000000000000000000000000000789\",\"logIndex\":\"0x0\",\"removed\":false}")
           (result (coalton:coalton
                    (web3/receipt:parse-log-entry
                     (coalton:lisp coalton:String () log-json)))))
      (assert (result-ok-p result))
      (let* ((log (result-value result))
             (block-hash (coalton:coalton
                          (web3/receipt:.log-block-hash
                           (coalton:lisp web3/receipt:LogEntry () log)))))
        ;; Should be 32 bytes
        (assert (= (length block-hash) 32))
        ;; First byte should be 0x12
        (assert (= (aref block-hash 0) #x12)))))

  (test-case "log with empty topics list"
    (let* ((log-json "{\"address\":\"0xdac17f958d2ee523a2206206994597c13d831ec7\",\"topics\":[],\"data\":\"0x1234\",\"blockNumber\":\"0x1\",\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"logIndex\":\"0x0\",\"removed\":false}")
           (result (coalton:coalton
                    (web3/receipt:parse-log-entry
                     (coalton:lisp coalton:String () log-json)))))
      (assert (result-ok-p result))
      (let* ((log (result-value result))
             (topics (coalton:coalton
                      (web3/receipt:.log-topics
                       (coalton:lisp web3/receipt:LogEntry () log))))
             (count (coalton:coalton
                     (coalton-library/list:length
                      (coalton:lisp (coalton:List web3/types:Bytes) () topics)))))
        (assert (= count 0)))))

  (test-case "log with maximum 4 topics"
    (let* ((log-json "{\"address\":\"0xdac17f958d2ee523a2206206994597c13d831ec7\",\"topics\":[\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"0x0000000000000000000000000000000000000000000000000000000000000003\",\"0x0000000000000000000000000000000000000000000000000000000000000004\"],\"data\":\"0x\",\"blockNumber\":\"0x1\",\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"logIndex\":\"0x0\",\"removed\":false}")
           (result (coalton:coalton
                    (web3/receipt:parse-log-entry
                     (coalton:lisp coalton:String () log-json)))))
      (assert (result-ok-p result))
      (let* ((log (result-value result))
             (topics (coalton:coalton
                      (web3/receipt:.log-topics
                       (coalton:lisp web3/receipt:LogEntry () log))))
             (count (coalton:coalton
                     (coalton-library/list:length
                      (coalton:lisp (coalton:List web3/types:Bytes) () topics)))))
        (assert (= count 4)))))

  ;;; =========================================================================
  ;;; Additional Receipt Accessor Tests
  ;;; =========================================================================

  (test-case "receipt-transaction-hash accessor returns correct hash"
    (let* ((receipt-json "{\"transactionHash\":\"0xabcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"blockNumber\":\"0x1\",\"from\":\"0xa7d9ddbe1f17865597fbd27ec712455208b6b76d\",\"to\":\"0xf02c1c8e6114b1dbe8937a39260b5b0a374432bb\",\"cumulativeGasUsed\":\"0x5208\",\"gasUsed\":\"0x5208\",\"effectiveGasPrice\":\"0x1\",\"contractAddress\":null,\"logs\":[],\"logsBloom\":\"0x\",\"status\":\"0x1\",\"type\":\"0x0\"}")
           (result (coalton:coalton
                    (web3/receipt:parse-receipt
                     (coalton:lisp coalton:String () receipt-json)))))
      (assert (result-ok-p result))
      (let* ((receipt (result-value result))
             (tx-hash (coalton:coalton
                       (web3/receipt:.receipt-transaction-hash
                        (coalton:lisp web3/receipt:Receipt () receipt)))))
        (assert (= (length tx-hash) 32))
        (assert (= (aref tx-hash 0) #xab)))))

  (test-case "receipt-from accessor returns correct address"
    (let* ((receipt-json "{\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"blockNumber\":\"0x1\",\"from\":\"0xdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef\",\"to\":\"0xf02c1c8e6114b1dbe8937a39260b5b0a374432bb\",\"cumulativeGasUsed\":\"0x5208\",\"gasUsed\":\"0x5208\",\"effectiveGasPrice\":\"0x1\",\"contractAddress\":null,\"logs\":[],\"logsBloom\":\"0x\",\"status\":\"0x1\",\"type\":\"0x0\"}")
           (result (coalton:coalton
                    (web3/receipt:parse-receipt
                     (coalton:lisp coalton:String () receipt-json)))))
      (assert (result-ok-p result))
      (let* ((receipt (result-value result))
             (from-addr (coalton:coalton
                         (web3/receipt:.receipt-from
                          (coalton:lisp web3/receipt:Receipt () receipt))))
             (from-bytes (coalton:coalton
                          (web3/address:address-bytes
                           (coalton:lisp web3/address:Address () from-addr)))))
        ;; Address should be 20 bytes
        (assert (= (length from-bytes) 20))
        ;; First byte should be 0xde
        (assert (= (aref from-bytes 0) #xde)))))

  (test-case "receipt-cumulative-gas-used accessor"
    (let* ((receipt-json "{\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"transactionIndex\":\"0x5\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"blockNumber\":\"0x1\",\"from\":\"0xa7d9ddbe1f17865597fbd27ec712455208b6b76d\",\"to\":\"0xf02c1c8e6114b1dbe8937a39260b5b0a374432bb\",\"cumulativeGasUsed\":\"0xf4240\",\"gasUsed\":\"0x5208\",\"effectiveGasPrice\":\"0x1\",\"contractAddress\":null,\"logs\":[],\"logsBloom\":\"0x\",\"status\":\"0x1\",\"type\":\"0x0\"}")
           (result (coalton:coalton
                    (web3/receipt:parse-receipt
                     (coalton:lisp coalton:String () receipt-json)))))
      (assert (result-ok-p result))
      (let* ((receipt (result-value result))
             (cumulative (coalton:coalton
                          (web3/receipt:.receipt-cumulative-gas-used
                           (coalton:lisp web3/receipt:Receipt () receipt)))))
        ;; 0xf4240 = 1000000
        (assert (= cumulative 1000000)))))

  (test-case "receipt-effective-gas-price accessor"
    (let* ((receipt-json "{\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"blockNumber\":\"0x1\",\"from\":\"0xa7d9ddbe1f17865597fbd27ec712455208b6b76d\",\"to\":\"0xf02c1c8e6114b1dbe8937a39260b5b0a374432bb\",\"cumulativeGasUsed\":\"0x5208\",\"gasUsed\":\"0x5208\",\"effectiveGasPrice\":\"0x12a05f200\",\"contractAddress\":null,\"logs\":[],\"logsBloom\":\"0x\",\"status\":\"0x1\",\"type\":\"0x2\"}")
           (result (coalton:coalton
                    (web3/receipt:parse-receipt
                     (coalton:lisp coalton:String () receipt-json)))))
      (assert (result-ok-p result))
      (let* ((receipt (result-value result))
             (gas-price (coalton:coalton
                         (web3/receipt:.receipt-effective-gas-price
                          (coalton:lisp web3/receipt:Receipt () receipt))))
             (price-int (coalton:coalton
                         (web3/types:u256-to-integer
                          (coalton:lisp web3/types:U256 () gas-price)))))
        ;; 0x12a05f200 = 5000000000 = 5 gwei
        (assert (= price-int 5000000000)))))

  (test-case "receipt-logs-bloom accessor"
    ;; Logs bloom is 256 bytes = 512 hex characters
    ;; Using ab at start and cd at end with zeros in between
    (let* ((receipt-json "{\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"blockNumber\":\"0x1\",\"from\":\"0xa7d9ddbe1f17865597fbd27ec712455208b6b76d\",\"to\":\"0xf02c1c8e6114b1dbe8937a39260b5b0a374432bb\",\"cumulativeGasUsed\":\"0x5208\",\"gasUsed\":\"0x5208\",\"effectiveGasPrice\":\"0x1\",\"contractAddress\":null,\"logs\":[],\"logsBloom\":\"0xab0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000cd\",\"status\":\"0x1\",\"type\":\"0x0\"}")
           (result (coalton:coalton
                    (web3/receipt:parse-receipt
                     (coalton:lisp coalton:String () receipt-json)))))
      (assert (result-ok-p result))
      (let* ((receipt (result-value result))
             (bloom (coalton:coalton
                     (web3/receipt:.receipt-logs-bloom
                      (coalton:lisp web3/receipt:Receipt () receipt)))))
        ;; Logs bloom is 256 bytes
        (assert (= (length bloom) 256))
        ;; First byte should be 0xab
        (assert (= (aref bloom 0) #xab))
        ;; Last byte should be 0xcd
        (assert (= (aref bloom 255) #xcd)))))

  (test-case "receipt-status accessor returns StatusSuccess"
    (let* ((receipt-json "{\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"blockNumber\":\"0x1\",\"from\":\"0xa7d9ddbe1f17865597fbd27ec712455208b6b76d\",\"to\":\"0xf02c1c8e6114b1dbe8937a39260b5b0a374432bb\",\"cumulativeGasUsed\":\"0x5208\",\"gasUsed\":\"0x5208\",\"effectiveGasPrice\":\"0x1\",\"contractAddress\":null,\"logs\":[],\"logsBloom\":\"0x\",\"status\":\"0x1\",\"type\":\"0x0\"}")
           (result (coalton:coalton
                    (web3/receipt:parse-receipt
                     (coalton:lisp coalton:String () receipt-json)))))
      (assert (result-ok-p result))
      (let* ((receipt (result-value result))
             (status (coalton:coalton
                      (web3/receipt:.receipt-status
                       (coalton:lisp web3/receipt:Receipt () receipt)))))
        (assert (typep status 'web3/receipt::receiptstatus/statussuccess)))))

  (test-case "receipt-status accessor returns StatusFailed"
    (let* ((receipt-json "{\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"blockNumber\":\"0x1\",\"from\":\"0xa7d9ddbe1f17865597fbd27ec712455208b6b76d\",\"to\":\"0xf02c1c8e6114b1dbe8937a39260b5b0a374432bb\",\"cumulativeGasUsed\":\"0x5208\",\"gasUsed\":\"0x5208\",\"effectiveGasPrice\":\"0x1\",\"contractAddress\":null,\"logs\":[],\"logsBloom\":\"0x\",\"status\":\"0x0\",\"type\":\"0x0\"}")
           (result (coalton:coalton
                    (web3/receipt:parse-receipt
                     (coalton:lisp coalton:String () receipt-json)))))
      (assert (result-ok-p result))
      (let* ((receipt (result-value result))
             (status (coalton:coalton
                      (web3/receipt:.receipt-status
                       (coalton:lisp web3/receipt:Receipt () receipt)))))
        (assert (typep status 'web3/receipt::receiptstatus/statusfailed)))))

  (test-case "receipt-type accessor returns TxTypeLegacy"
    (let* ((receipt-json "{\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"blockNumber\":\"0x1\",\"from\":\"0xa7d9ddbe1f17865597fbd27ec712455208b6b76d\",\"to\":\"0xf02c1c8e6114b1dbe8937a39260b5b0a374432bb\",\"cumulativeGasUsed\":\"0x5208\",\"gasUsed\":\"0x5208\",\"effectiveGasPrice\":\"0x1\",\"contractAddress\":null,\"logs\":[],\"logsBloom\":\"0x\",\"status\":\"0x1\",\"type\":\"0x0\"}")
           (result (coalton:coalton
                    (web3/receipt:parse-receipt
                     (coalton:lisp coalton:String () receipt-json)))))
      (assert (result-ok-p result))
      (let* ((receipt (result-value result))
             (tx-type (coalton:coalton
                       (web3/receipt:.receipt-type
                        (coalton:lisp web3/receipt:Receipt () receipt)))))
        (assert (typep tx-type 'web3/receipt::txtype/txtypelegacy)))))

  (test-case "receipt-type accessor returns TxTypeEip1559"
    (let* ((receipt-json "{\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"blockNumber\":\"0x1\",\"from\":\"0xa7d9ddbe1f17865597fbd27ec712455208b6b76d\",\"to\":\"0xf02c1c8e6114b1dbe8937a39260b5b0a374432bb\",\"cumulativeGasUsed\":\"0x5208\",\"gasUsed\":\"0x5208\",\"effectiveGasPrice\":\"0x1\",\"contractAddress\":null,\"logs\":[],\"logsBloom\":\"0x\",\"status\":\"0x1\",\"type\":\"0x2\"}")
           (result (coalton:coalton
                    (web3/receipt:parse-receipt
                     (coalton:lisp coalton:String () receipt-json)))))
      (assert (result-ok-p result))
      (let* ((receipt (result-value result))
             (tx-type (coalton:coalton
                       (web3/receipt:.receipt-type
                        (coalton:lisp web3/receipt:Receipt () receipt)))))
        (assert (typep tx-type 'web3/receipt::txtype/txtypeeip1559)))))

  ;;; =========================================================================
  ;;; filter-logs-by-address Tests
  ;;; =========================================================================

  (test-case "filter-logs-by-address filters matching address"
    (let* ((receipt-json "{\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"blockNumber\":\"0x1\",\"from\":\"0xa7d9ddbe1f17865597fbd27ec712455208b6b76d\",\"to\":\"0xdac17f958d2ee523a2206206994597c13d831ec7\",\"cumulativeGasUsed\":\"0x10000\",\"gasUsed\":\"0x8000\",\"effectiveGasPrice\":\"0x1\",\"contractAddress\":null,\"logs\":[{\"address\":\"0xdac17f958d2ee523a2206206994597c13d831ec7\",\"topics\":[\"0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef\"],\"data\":\"0x\",\"blockNumber\":\"0x1\",\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"logIndex\":\"0x0\",\"removed\":false},{\"address\":\"0x1234567890123456789012345678901234567890\",\"topics\":[\"0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef\"],\"data\":\"0x\",\"blockNumber\":\"0x1\",\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"logIndex\":\"0x1\",\"removed\":false}],\"logsBloom\":\"0x\",\"status\":\"0x1\",\"type\":\"0x0\"}")
           (result (coalton:coalton
                    (web3/receipt:parse-receipt
                     (coalton:lisp coalton:String () receipt-json)))))
      (assert (result-ok-p result))
      (let* ((receipt (result-value result))
             (logs (coalton:coalton
                    (web3/receipt:.receipt-logs
                     (coalton:lisp web3/receipt:Receipt () receipt))))
             ;; Filter for USDT address
             (usdt-addr-result (coalton:coalton
                                (web3/address:address-from-hex
                                 "0xdac17f958d2ee523a2206206994597c13d831ec7")))
             (usdt-addr (result-value usdt-addr-result))
             (filtered (coalton:coalton
                        (web3/receipt:filter-logs-by-address
                         (coalton:lisp (coalton:List web3/receipt:LogEntry) () logs)
                         (coalton:lisp web3/address:Address () usdt-addr))))
             (count (coalton:coalton
                     (coalton-library/list:length
                      (coalton:lisp (coalton:List web3/receipt:LogEntry) () filtered)))))
        ;; Should find 1 log from USDT address
        (assert (= count 1)))))

  (test-case "filter-logs-by-address returns empty for non-matching"
    (let* ((receipt-json "{\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"blockNumber\":\"0x1\",\"from\":\"0xa7d9ddbe1f17865597fbd27ec712455208b6b76d\",\"to\":\"0xdac17f958d2ee523a2206206994597c13d831ec7\",\"cumulativeGasUsed\":\"0x10000\",\"gasUsed\":\"0x8000\",\"effectiveGasPrice\":\"0x1\",\"contractAddress\":null,\"logs\":[{\"address\":\"0xdac17f958d2ee523a2206206994597c13d831ec7\",\"topics\":[],\"data\":\"0x\",\"blockNumber\":\"0x1\",\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"logIndex\":\"0x0\",\"removed\":false}],\"logsBloom\":\"0x\",\"status\":\"0x1\",\"type\":\"0x0\"}")
           (result (coalton:coalton
                    (web3/receipt:parse-receipt
                     (coalton:lisp coalton:String () receipt-json)))))
      (assert (result-ok-p result))
      (let* ((receipt (result-value result))
             (logs (coalton:coalton
                    (web3/receipt:.receipt-logs
                     (coalton:lisp web3/receipt:Receipt () receipt))))
             ;; Filter for different address (not in logs)
             (other-addr-result (coalton:coalton
                                 (web3/address:address-from-hex
                                  "0x0000000000000000000000000000000000000000")))
             (other-addr (result-value other-addr-result))
             (filtered (coalton:coalton
                        (web3/receipt:filter-logs-by-address
                         (coalton:lisp (coalton:List web3/receipt:LogEntry) () logs)
                         (coalton:lisp web3/address:Address () other-addr))))
             (count (coalton:coalton
                     (coalton-library/list:length
                      (coalton:lisp (coalton:List web3/receipt:LogEntry) () filtered)))))
        ;; Should find 0 logs
        (assert (= count 0)))))

  (test-case "filter-logs-by-topic returns empty for non-matching topic"
    (let* ((receipt-json "{\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"blockNumber\":\"0x1\",\"from\":\"0xa7d9ddbe1f17865597fbd27ec712455208b6b76d\",\"to\":\"0xdac17f958d2ee523a2206206994597c13d831ec7\",\"cumulativeGasUsed\":\"0x10000\",\"gasUsed\":\"0x8000\",\"effectiveGasPrice\":\"0x1\",\"contractAddress\":null,\"logs\":[{\"address\":\"0xdac17f958d2ee523a2206206994597c13d831ec7\",\"topics\":[\"0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef\"],\"data\":\"0x\",\"blockNumber\":\"0x1\",\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"logIndex\":\"0x0\",\"removed\":false}],\"logsBloom\":\"0x\",\"status\":\"0x1\",\"type\":\"0x0\"}")
           (result (coalton:coalton
                    (web3/receipt:parse-receipt
                     (coalton:lisp coalton:String () receipt-json)))))
      (assert (result-ok-p result))
      (let* ((receipt (result-value result))
             (logs (coalton:coalton
                    (web3/receipt:.receipt-logs
                     (coalton:lisp web3/receipt:Receipt () receipt))))
             ;; Use a non-matching topic
             (random-topic-result (coalton:coalton
                                   (web3/types:hex-decode
                                    "0000000000000000000000000000000000000000000000000000000000000000")))
             (random-topic (result-value random-topic-result))
             (filtered (coalton:coalton
                        (web3/receipt:filter-logs-by-topic
                         (coalton:lisp (coalton:List web3/receipt:LogEntry) () logs)
                         (coalton:lisp web3/types:Bytes () random-topic))))
             (count (coalton:coalton
                     (coalton-library/list:length
                      (coalton:lisp (coalton:List web3/receipt:LogEntry) () filtered)))))
        ;; Should find 0 logs
        (assert (= count 0)))))

  ;;; =========================================================================
  ;;; Edge Cases and Error Handling
  ;;; =========================================================================

  (test-case "receipt with high gas values"
    (let* ((receipt-json "{\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"blockNumber\":\"0x1\",\"from\":\"0xa7d9ddbe1f17865597fbd27ec712455208b6b76d\",\"to\":\"0xf02c1c8e6114b1dbe8937a39260b5b0a374432bb\",\"cumulativeGasUsed\":\"0x1c9c380\",\"gasUsed\":\"0x1c9c380\",\"effectiveGasPrice\":\"0x2540be400\",\"contractAddress\":null,\"logs\":[],\"logsBloom\":\"0x\",\"status\":\"0x1\",\"type\":\"0x2\"}")
           (result (coalton:coalton
                    (web3/receipt:parse-receipt
                     (coalton:lisp coalton:String () receipt-json)))))
      (assert (result-ok-p result))
      (let* ((receipt (result-value result))
             (gas-used (coalton:coalton
                        (web3/receipt:.receipt-gas-used
                         (coalton:lisp web3/receipt:Receipt () receipt)))))
        ;; 0x1c9c380 = 30000000 (30M gas - near block limit)
        (assert (= gas-used 30000000)))))

  (test-case "receipt with large block number"
    (let* ((receipt-json "{\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"blockNumber\":\"0x1312d00\",\"from\":\"0xa7d9ddbe1f17865597fbd27ec712455208b6b76d\",\"to\":\"0xf02c1c8e6114b1dbe8937a39260b5b0a374432bb\",\"cumulativeGasUsed\":\"0x5208\",\"gasUsed\":\"0x5208\",\"effectiveGasPrice\":\"0x1\",\"contractAddress\":null,\"logs\":[],\"logsBloom\":\"0x\",\"status\":\"0x1\",\"type\":\"0x0\"}")
           (result (coalton:coalton
                    (web3/receipt:parse-receipt
                     (coalton:lisp coalton:String () receipt-json)))))
      (assert (result-ok-p result))
      (let* ((receipt (result-value result))
             (block-num (coalton:coalton
                         (web3/receipt:.receipt-block-number
                          (coalton:lisp web3/receipt:Receipt () receipt)))))
        ;; 0x1312d00 = 20000000 (block 20M)
        (assert (= block-num 20000000)))))

  (test-case "parse-receipt with invalid JSON returns error"
    (let* ((invalid-json "not valid json {")
           (result (coalton:coalton
                    (web3/receipt:parse-receipt
                     (coalton:lisp coalton:String () invalid-json)))))
      (assert (result-err-p result))))

  (test-case "parse-log-entry with invalid JSON returns error"
    (let* ((invalid-json "{broken")
           (result (coalton:coalton
                    (web3/receipt:parse-log-entry
                     (coalton:lisp coalton:String () invalid-json)))))
      (assert (result-err-p result))))

  (test-case "cumulative gas always >= gas used"
    (let* ((receipt-json "{\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"transactionIndex\":\"0xa\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"blockNumber\":\"0x1\",\"from\":\"0xa7d9ddbe1f17865597fbd27ec712455208b6b76d\",\"to\":\"0xf02c1c8e6114b1dbe8937a39260b5b0a374432bb\",\"cumulativeGasUsed\":\"0x100000\",\"gasUsed\":\"0x5208\",\"effectiveGasPrice\":\"0x1\",\"contractAddress\":null,\"logs\":[],\"logsBloom\":\"0x\",\"status\":\"0x1\",\"type\":\"0x0\"}")
           (result (coalton:coalton
                    (web3/receipt:parse-receipt
                     (coalton:lisp coalton:String () receipt-json)))))
      (assert (result-ok-p result))
      (let* ((receipt (result-value result))
             (cumulative (coalton:coalton
                          (web3/receipt:.receipt-cumulative-gas-used
                           (coalton:lisp web3/receipt:Receipt () receipt))))
             (gas-used (coalton:coalton
                        (web3/receipt:.receipt-gas-used
                         (coalton:lisp web3/receipt:Receipt () receipt)))))
        ;; Cumulative should be >= gas used (includes previous txs in block)
        (assert (>= cumulative gas-used))))))
