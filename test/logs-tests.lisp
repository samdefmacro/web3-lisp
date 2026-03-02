;;; Event Log Querying tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Log Filter Tests
;;; =========================================================================

(defun run-logs-tests ()
  (format t "~%=== Event Log Querying Tests ===~%")

  ;;; =========================================================================
  ;;; LogFilter Construction Tests
  ;;; =========================================================================

  (test-case "make-log-filter with all None defaults"
    (let ((filter (coalton:coalton
                   (web3/logs:make-log-filter
                    (coalton:the (coalton-prelude:Optional web3/block:BlockTag) coalton-prelude:None)
                    (coalton:the (coalton-prelude:Optional web3/block:BlockTag) coalton-prelude:None)
                    (coalton:the (coalton-prelude:Optional web3/address:Address) coalton-prelude:None)
                    (coalton:the (coalton:List (coalton-prelude:Optional web3/types:Bytes)) coalton:Nil)))))
      (assert filter)
      ;; from-block should be None
      (assert (optional-none-p
               (coalton:coalton
                (web3/logs:.filter-from-block
                 (coalton:lisp web3/logs:LogFilter () filter)))))
      ;; to-block should be None
      (assert (optional-none-p
               (coalton:coalton
                (web3/logs:.filter-to-block
                 (coalton:lisp web3/logs:LogFilter () filter)))))
      ;; address should be None
      (assert (optional-none-p
               (coalton:coalton
                (web3/logs:.filter-address
                 (coalton:lisp web3/logs:LogFilter () filter)))))))

  (test-case "make-log-filter with block range"
    (let ((filter (coalton:coalton
                   (web3/logs:make-log-filter
                    (coalton-prelude:Some (web3/block:TagNumber 100))
                    (coalton-prelude:Some (web3/block:TagNumber 200))
                    (coalton:the (coalton-prelude:Optional web3/address:Address) coalton-prelude:None)
                    (coalton:the (coalton:List (coalton-prelude:Optional web3/types:Bytes)) coalton:Nil)))))
      (assert filter)
      ;; from-block should be Some
      (assert (optional-some-p
               (coalton:coalton
                (web3/logs:.filter-from-block
                 (coalton:lisp web3/logs:LogFilter () filter)))))
      ;; to-block should be Some
      (assert (optional-some-p
               (coalton:coalton
                (web3/logs:.filter-to-block
                 (coalton:lisp web3/logs:LogFilter () filter)))))))

  (test-case "make-log-filter with address"
    (let* ((addr (result-value (coalton:coalton
                                (web3/address:address-from-hex
                                 "0xdac17f958d2ee523a2206206994597c13d831ec7"))))
           (filter (coalton:coalton
                    (web3/logs:make-log-filter
                     (coalton-prelude:Some web3/block:TagLatest)
                     (coalton:the (coalton-prelude:Optional web3/block:BlockTag) coalton-prelude:None)
                     (coalton-prelude:Some (coalton:lisp web3/address:Address () addr))
                     (coalton:the (coalton:List (coalton-prelude:Optional web3/types:Bytes)) coalton:Nil)))))
      (assert filter)
      ;; address should be Some
      (assert (optional-some-p
               (coalton:coalton
                (web3/logs:.filter-address
                 (coalton:lisp web3/logs:LogFilter () filter)))))))

  (test-case "make-log-filter with topics"
    (let* ((topic0 (coalton:coalton
                    (web3/events:event-signature "Transfer(address,address,uint256)")))
           (filter (coalton:coalton
                    (web3/logs:make-log-filter
                     (coalton:the (coalton-prelude:Optional web3/block:BlockTag) coalton-prelude:None)
                     (coalton:the (coalton-prelude:Optional web3/block:BlockTag) coalton-prelude:None)
                     (coalton:the (coalton-prelude:Optional web3/address:Address) coalton-prelude:None)
                     (coalton:Cons (coalton-prelude:Some (coalton:lisp web3/types:Bytes () topic0)) coalton:Nil)))))
      (assert filter)
      ;; topics list should be non-empty
      (let ((topics (coalton:coalton
                     (web3/logs:.filter-topics
                      (coalton:lisp web3/logs:LogFilter () filter)))))
        (assert (= 1 (coalton:coalton
                       (coalton-library/list:length
                        (coalton:lisp (coalton:List (coalton-prelude:Optional web3/types:Bytes)) () topics))))))))

  ;;; =========================================================================
  ;;; get-logs-by-event filter construction test
  ;;; =========================================================================

  (test-case "get-logs-by-event builds filter with address and topic"
    ;; We can't call the actual RPC, but we can verify the convenience
    ;; function sets up correct LogFilter structure by testing make-log-filter
    (let* ((addr (result-value (coalton:coalton
                                (web3/address:address-from-hex
                                 "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"))))
           (transfer-topic (coalton:coalton
                            (web3/events:event-signature "Transfer(address,address,uint256)")))
           ;; Build the same filter that get-logs-by-event would
           (filter (coalton:coalton
                    (web3/logs:make-log-filter
                     (coalton-prelude:Some (web3/block:TagNumber 18000000))
                     (coalton-prelude:Some (web3/block:TagNumber 18000010))
                     (coalton-prelude:Some (coalton:lisp web3/address:Address () addr))
                     (coalton:Cons (coalton-prelude:Some (coalton:lisp web3/types:Bytes () transfer-topic)) coalton:Nil)))))
      (assert filter)
      ;; Verify all fields are set
      (assert (optional-some-p
               (coalton:coalton
                (web3/logs:.filter-from-block
                 (coalton:lisp web3/logs:LogFilter () filter)))))
      (assert (optional-some-p
               (coalton:coalton
                (web3/logs:.filter-to-block
                 (coalton:lisp web3/logs:LogFilter () filter)))))
      (assert (optional-some-p
               (coalton:coalton
                (web3/logs:.filter-address
                 (coalton:lisp web3/logs:LogFilter () filter)))))))

  ;;; =========================================================================
  ;;; Log Entry JSON Parsing Tests (reusing receipt parser)
  ;;; =========================================================================

  (test-case "parse single log entry from JSON"
    (let* ((log-json "{\"address\":\"0xdac17f958d2ee523a2206206994597c13d831ec7\",\"topics\":[\"0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef\",\"0x000000000000000000000000a9d1e08c7793af67e9d92fe308d5697fb81d3e43\",\"0x00000000000000000000000028c6c06298d514db089934071355e5743bf21d60\"],\"data\":\"0x00000000000000000000000000000000000000000000000000000000773594c0\",\"blockNumber\":\"0x124f1c0\",\"transactionHash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"transactionIndex\":\"0x5\",\"blockHash\":\"0xabcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890\",\"logIndex\":\"0xa\",\"removed\":false}")
           (result (coalton:coalton
                    (web3/receipt:parse-log-entry
                     (coalton:lisp coalton:String () log-json)))))
      (assert (result-ok-p result))
      (let ((log (result-value result)))
        ;; Check block number parsed correctly (0x124f1c0 = 19198400)
        (assert (= 19198400
                   (coalton:coalton
                    (web3/receipt:.log-block-number
                     (coalton:lisp web3/receipt:LogEntry () log)))))
        ;; Check log index
        (assert (= 10
                   (coalton:coalton
                    (web3/receipt:.log-log-index
                     (coalton:lisp web3/receipt:LogEntry () log)))))
        ;; Check we have 3 topics
        (assert (= 3
                   (coalton:coalton
                    (coalton-library/list:length
                     (web3/receipt:.log-topics
                      (coalton:lisp web3/receipt:LogEntry () log))))))
        ;; Check removed is false
        (assert (eq coalton:False
                    (coalton:coalton
                     (web3/receipt:.log-removed
                      (coalton:lisp web3/receipt:LogEntry () log))))))))

  (test-case "parse array of log entries"
    (let* ((logs-json "[{\"address\":\"0xdac17f958d2ee523a2206206994597c13d831ec7\",\"topics\":[\"0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef\"],\"data\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"blockNumber\":\"0x100\",\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"transactionIndex\":\"0x0\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"logIndex\":\"0x0\",\"removed\":false},{\"address\":\"0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48\",\"topics\":[\"0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef\"],\"data\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"blockNumber\":\"0x101\",\"transactionHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"transactionIndex\":\"0x1\",\"blockHash\":\"0x0000000000000000000000000000000000000000000000000000000000000002\",\"logIndex\":\"0x1\",\"removed\":false}]")
           (parsed (cl-json:decode-json-from-string logs-json)))
      ;; Verify we can parse the JSON array
      (assert (= 2 (length parsed)))
      ;; Parse each log entry individually via receipt parser
      (let* ((log1-json (cl-json:encode-json-to-string (first parsed)))
             (result1 (coalton:coalton
                       (web3/receipt:parse-log-entry
                        (coalton:lisp coalton:String () log1-json)))))
        (assert (result-ok-p result1))
        (let ((log1 (result-value result1)))
          (assert (= 256
                     (coalton:coalton
                      (web3/receipt:.log-block-number
                       (coalton:lisp web3/receipt:LogEntry () log1)))))))))

  ;;; =========================================================================
  ;;; Filter Serialization Tests
  ;;; =========================================================================

  (test-case "serialize empty filter to JSON"
    (let* ((filter (coalton:coalton
                    (web3/logs:make-log-filter
                     (coalton:the (coalton-prelude:Optional web3/block:BlockTag) coalton-prelude:None)
                     (coalton:the (coalton-prelude:Optional web3/block:BlockTag) coalton-prelude:None)
                     (coalton:the (coalton-prelude:Optional web3/address:Address) coalton-prelude:None)
                     (coalton:the (coalton:List (coalton-prelude:Optional web3/types:Bytes)) coalton:Nil))))
           (json (web3/logs::%serialize-log-filter filter)))
      ;; Should be a JSON array with an empty object
      (assert (stringp json))
      (assert (search "[" json))
      (assert (search "]" json))))

  (test-case "serialize filter with block range to JSON"
    (let* ((filter (coalton:coalton
                    (web3/logs:make-log-filter
                     (coalton-prelude:Some (web3/block:TagNumber 100))
                     (coalton-prelude:Some web3/block:TagLatest)
                     (coalton:the (coalton-prelude:Optional web3/address:Address) coalton-prelude:None)
                     (coalton:the (coalton:List (coalton-prelude:Optional web3/types:Bytes)) coalton:Nil))))
           (json (web3/logs::%serialize-log-filter filter)))
      ;; Should contain fromBlock and toBlock
      (assert (search "fromBlock" json))
      (assert (search "0x64" json))  ; 100 in hex
      (assert (search "toBlock" json))
      (assert (search "latest" json))))

  (test-case "serialize filter with address to JSON"
    (let* ((addr (result-value (coalton:coalton
                                (web3/address:address-from-hex
                                 "0xdac17f958d2ee523a2206206994597c13d831ec7"))))
           (filter (coalton:coalton
                    (web3/logs:make-log-filter
                     (coalton:the (coalton-prelude:Optional web3/block:BlockTag) coalton-prelude:None)
                     (coalton:the (coalton-prelude:Optional web3/block:BlockTag) coalton-prelude:None)
                     (coalton-prelude:Some (coalton:lisp web3/address:Address () addr))
                     (coalton:the (coalton:List (coalton-prelude:Optional web3/types:Bytes)) coalton:Nil))))
           (json (web3/logs::%serialize-log-filter filter)))
      ;; Should contain address
      (assert (search "address" json))
      (assert (search "dac17f958d2ee523a2206206994597c13d831ec7" json))))

  (test-case "serialize filter with topics to JSON"
    (let* ((topic0 (coalton:coalton
                    (web3/events:event-signature "Transfer(address,address,uint256)")))
           (filter (coalton:coalton
                    (web3/logs:make-log-filter
                     (coalton-prelude:Some (web3/block:TagNumber 0))
                     (coalton:the (coalton-prelude:Optional web3/block:BlockTag) coalton-prelude:None)
                     (coalton:the (coalton-prelude:Optional web3/address:Address) coalton-prelude:None)
                     (coalton:Cons (coalton-prelude:Some (coalton:lisp web3/types:Bytes () topic0)) coalton:Nil))))
           (json (web3/logs::%serialize-log-filter filter)))
      ;; Should contain topics array with the Transfer event signature hash
      (assert (search "topics" json))
      ;; Transfer(address,address,uint256) keccak256 = ddf252ad...
      (assert (search "ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef" json))
      ;; Should also have fromBlock
      (assert (search "fromBlock" json))))

  ;;; =========================================================================
  ;;; Network-Dependent Tests (gated by WEB3_LOGS_RPC env var)
  ;;; =========================================================================

  (let ((rpc-url (uiop:getenv "WEB3_LOGS_RPC")))
    (if rpc-url
        (progn
          (format t "~%  --- Network tests enabled (WEB3_LOGS_RPC=~A) ---~%" rpc-url)

          (test-case "eth-get-logs for USDC Transfer events in known block range"
            ;; USDC contract: 0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48
            ;; Query a small block range for Transfer events
            (let* ((provider (coalton:coalton
                              (web3/provider:make-http-provider
                               (coalton:lisp coalton:String () rpc-url))))
                   (addr (result-value (coalton:coalton
                                        (web3/address:address-from-hex
                                         "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"))))
                   (transfer-topic (coalton:coalton
                                    (web3/events:event-signature "Transfer(address,address,uint256)")))
                   (result (coalton:coalton
                            (web3/logs:get-logs-by-event
                             (coalton:lisp web3/provider:HttpProvider () provider)
                             (coalton:lisp web3/address:Address () addr)
                             (web3/block:TagNumber 18000000)
                             (web3/block:TagNumber 18000010)
                             (coalton:lisp web3/types:Bytes () transfer-topic)))))
              (assert (result-ok-p result))
              (let ((logs (result-value result)))
                ;; Should return some logs (USDC is very active)
                (format t "    Found ~A logs~%"
                        (coalton:coalton
                         (coalton-library/list:length
                          (coalton:lisp (coalton:List web3/receipt:LogEntry) () logs)))))))

          (test-case "get-logs-by-address returns logs for active contract"
            (let* ((provider (coalton:coalton
                              (web3/provider:make-http-provider
                               (coalton:lisp coalton:String () rpc-url))))
                   (addr (result-value (coalton:coalton
                                        (web3/address:address-from-hex
                                         "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"))))
                   (result (coalton:coalton
                            (web3/logs:get-logs-by-address
                             (coalton:lisp web3/provider:HttpProvider () provider)
                             (coalton:lisp web3/address:Address () addr)
                             (web3/block:TagNumber 18000000)
                             (web3/block:TagNumber 18000005)))))
              (assert (result-ok-p result)))))

        (test-case "Note: Network log tests require WEB3_LOGS_RPC env var"
          (assert t)))))
