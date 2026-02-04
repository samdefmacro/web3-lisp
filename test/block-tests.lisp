;;; Block Parsing tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Block Parsing Tests
;;; =========================================================================

(defun run-block-tests ()
  (format t "~%=== Block Parsing Tests ===~%")

  ;;; =========================================================================
  ;;; Block Tag Tests
  ;;; =========================================================================

  (test-case "block-tag-to-string for TagLatest"
    (let ((result (coalton:coalton (web3/block:block-tag-to-string web3/block:TagLatest))))
      (assert (string= result "latest"))))

  (test-case "block-tag-to-string for TagPending"
    (let ((result (coalton:coalton (web3/block:block-tag-to-string web3/block:TagPending))))
      (assert (string= result "pending"))))

  (test-case "block-tag-to-string for TagFinalized"
    (let ((result (coalton:coalton (web3/block:block-tag-to-string web3/block:TagFinalized))))
      (assert (string= result "finalized"))))

  (test-case "block-tag-to-string for TagSafe"
    (let ((result (coalton:coalton (web3/block:block-tag-to-string web3/block:TagSafe))))
      (assert (string= result "safe"))))

  (test-case "block-tag-to-string for TagEarliest"
    (let ((result (coalton:coalton (web3/block:block-tag-to-string web3/block:TagEarliest))))
      (assert (string= result "earliest"))))

  (test-case "block-tag-to-string for TagNumber"
    (let ((result (coalton:coalton (web3/block:block-tag-to-string (web3/block:TagNumber 12345)))))
      (assert (string= result "0x3039"))))

  ;;; =========================================================================
  ;;; Withdrawal Tests
  ;;; =========================================================================

  (test-case "make-withdrawal creates withdrawal"
    (let* ((addr (result-value (coalton:coalton
                                (web3/address:address-from-hex
                                 "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (withdrawal (coalton:coalton
                        (web3/block:make-withdrawal
                         1 100
                         (coalton:lisp web3/address:Address () addr)
                         1000000))))
      (assert (= (coalton:coalton
                  (web3/block:withdrawal-index
                   (coalton:lisp web3/block:Withdrawal () withdrawal)))
                 1))
      (assert (= (coalton:coalton
                  (web3/block:withdrawal-validator-index
                   (coalton:lisp web3/block:Withdrawal () withdrawal)))
                 100))
      (assert (= (coalton:coalton
                  (web3/block:withdrawal-amount
                   (coalton:lisp web3/block:Withdrawal () withdrawal)))
                 1000000))))

  ;;; =========================================================================
  ;;; Block Header Parsing Tests
  ;;; =========================================================================

  (test-case "parse-block-header parses valid header"
    (let* ((json-str "{\"number\":\"0x10d4f\",\"hash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"parentHash\":\"0xabcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890\",\"nonce\":\"0x0000000000000000\",\"sha3Uncles\":\"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"logsBloom\":\"0x00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\",\"transactionsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"stateRoot\":\"0xd5855eb08b3387c0af375e9cdb6acfc05eb8f519e419b874b6ff2ffda7ed1dff\",\"receiptsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"miner\":\"0x0000000000000000000000000000000000000000\",\"difficulty\":\"0x0\",\"totalDifficulty\":\"0xc70d815d562d3cfa955\",\"extraData\":\"0x\",\"size\":\"0x220\",\"gasLimit\":\"0x1c9c380\",\"gasUsed\":\"0x0\",\"timestamp\":\"0x65b9a123\",\"baseFeePerGas\":\"0x5f5e100\"}")
           (json-bytes (make-array (length json-str) :element-type 't
                                   :fill-pointer (length json-str) :adjustable t)))
      (dotimes (i (length json-str))
        (setf (aref json-bytes i) (char-code (char json-str i))))
      (let ((result (coalton:coalton
                     (web3/block:parse-block-header
                      (coalton:lisp web3/types:Bytes () json-bytes)))))
        (assert (result-ok-p result))
        (let* ((header (result-value result))
               (number (coalton:coalton
                        (web3/block:header-number
                         (coalton:lisp web3/block:BlockHeader () header))))
               (gas-limit (coalton:coalton
                           (web3/block:header-gas-limit
                            (coalton:lisp web3/block:BlockHeader () header))))
               (gas-used (coalton:coalton
                          (web3/block:header-gas-used
                           (coalton:lisp web3/block:BlockHeader () header)))))
          (assert (= number 68943))  ; 0x10d4f
          (assert (= gas-limit 30000000))  ; 0x1c9c380
          (assert (= gas-used 0))))))

  (test-case "parse-block-header extracts timestamp"
    (let* ((json-str "{\"number\":\"0x1\",\"hash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"parentHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"nonce\":\"0x0000000000000000\",\"sha3Uncles\":\"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"logsBloom\":\"0x00\",\"transactionsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"stateRoot\":\"0xd5855eb08b3387c0af375e9cdb6acfc05eb8f519e419b874b6ff2ffda7ed1dff\",\"receiptsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"miner\":\"0x0000000000000000000000000000000000000000\",\"difficulty\":\"0x0\",\"extraData\":\"0x\",\"size\":\"0x100\",\"gasLimit\":\"0x1c9c380\",\"gasUsed\":\"0x0\",\"timestamp\":\"0x3e8\"}")
           (json-bytes (make-array (length json-str) :element-type 't
                                   :fill-pointer (length json-str) :adjustable t)))
      (dotimes (i (length json-str))
        (setf (aref json-bytes i) (char-code (char json-str i))))
      (let ((result (coalton:coalton
                     (web3/block:parse-block-header
                      (coalton:lisp web3/types:Bytes () json-bytes)))))
        (assert (result-ok-p result))
        (let* ((header (result-value result))
               (timestamp (coalton:coalton
                           (web3/block:header-timestamp
                            (coalton:lisp web3/block:BlockHeader () header)))))
          (assert (= timestamp 1000))))))  ; 0x65b9a123

  (test-case "parse-block-header handles EIP-1559 base fee"
    (let* ((json-str "{\"number\":\"0x1\",\"hash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"parentHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"nonce\":\"0x0000000000000000\",\"sha3Uncles\":\"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"logsBloom\":\"0x00\",\"transactionsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"stateRoot\":\"0xd5855eb08b3387c0af375e9cdb6acfc05eb8f519e419b874b6ff2ffda7ed1dff\",\"receiptsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"miner\":\"0x0000000000000000000000000000000000000000\",\"difficulty\":\"0x0\",\"extraData\":\"0x\",\"size\":\"0x100\",\"gasLimit\":\"0x1c9c380\",\"gasUsed\":\"0x0\",\"timestamp\":\"0x65b9a123\",\"baseFeePerGas\":\"0x3b9aca00\"}")
           (json-bytes (make-array (length json-str) :element-type 't
                                   :fill-pointer (length json-str) :adjustable t)))
      (dotimes (i (length json-str))
        (setf (aref json-bytes i) (char-code (char json-str i))))
      (let ((result (coalton:coalton
                     (web3/block:parse-block-header
                      (coalton:lisp web3/types:Bytes () json-bytes)))))
        (assert (result-ok-p result))
        (let* ((header (result-value result))
               (base-fee-opt (coalton:coalton
                              (web3/block:header-base-fee
                               (coalton:lisp web3/block:BlockHeader () header)))))
          (assert (optional-some-p block-tests))
          (let ((base-fee (result-value base-fee-opt)))
            (assert (= base-fee 1000000000)))))))  ; 0x3b9aca00 = 1 gwei

  ;;; =========================================================================
  ;;; Block Utility Tests
  ;;; =========================================================================

  (test-case "is-post-merge detects post-merge block"
    (let* ((json-str "{\"number\":\"0x1\",\"hash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"parentHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"nonce\":\"0x0000000000000000\",\"sha3Uncles\":\"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"logsBloom\":\"0x00\",\"transactionsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"stateRoot\":\"0xd5855eb08b3387c0af375e9cdb6acfc05eb8f519e419b874b6ff2ffda7ed1dff\",\"receiptsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"miner\":\"0x0000000000000000000000000000000000000000\",\"difficulty\":\"0x0\",\"extraData\":\"0x\",\"size\":\"0x100\",\"gasLimit\":\"0x1c9c380\",\"gasUsed\":\"0x0\",\"timestamp\":\"0x65b9a123\"}")
           (json-bytes (make-array (length json-str) :element-type 't
                                   :fill-pointer (length json-str) :adjustable t)))
      (dotimes (i (length json-str))
        (setf (aref json-bytes i) (char-code (char json-str i))))
      (let ((result (coalton:coalton
                     (web3/block:parse-block-header
                      (coalton:lisp web3/types:Bytes () json-bytes)))))
        (assert (result-ok-p result))
        (let* ((header (result-value result))
               (post-merge (coalton:coalton
                            (web3/block:is-post-merge
                             (coalton:lisp web3/block:BlockHeader () header)))))
          (assert post-merge)))))

  (test-case "is-post-shanghai detects Shanghai block"
    (let* ((json-str "{\"number\":\"0x1\",\"hash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"parentHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"nonce\":\"0x0000000000000000\",\"sha3Uncles\":\"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"logsBloom\":\"0x00\",\"transactionsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"stateRoot\":\"0xd5855eb08b3387c0af375e9cdb6acfc05eb8f519e419b874b6ff2ffda7ed1dff\",\"receiptsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"miner\":\"0x0000000000000000000000000000000000000000\",\"difficulty\":\"0x0\",\"extraData\":\"0x\",\"size\":\"0x100\",\"gasLimit\":\"0x1c9c380\",\"gasUsed\":\"0x0\",\"timestamp\":\"0x65b9a123\",\"withdrawalsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\"}")
           (json-bytes (make-array (length json-str) :element-type 't
                                   :fill-pointer (length json-str) :adjustable t)))
      (dotimes (i (length json-str))
        (setf (aref json-bytes i) (char-code (char json-str i))))
      (let ((result (coalton:coalton
                     (web3/block:parse-block-header
                      (coalton:lisp web3/types:Bytes () json-bytes)))))
        (assert (result-ok-p result))
        (let* ((header (result-value result))
               (post-shanghai (coalton:coalton
                               (web3/block:is-post-shanghai
                                (coalton:lisp web3/block:BlockHeader () header)))))
          (assert post-shanghai)))))

  (test-case "is-post-cancun detects Cancun block"
    (let* ((json-str "{\"number\":\"0x1\",\"hash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"parentHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"nonce\":\"0x0000000000000000\",\"sha3Uncles\":\"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"logsBloom\":\"0x00\",\"transactionsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"stateRoot\":\"0xd5855eb08b3387c0af375e9cdb6acfc05eb8f519e419b874b6ff2ffda7ed1dff\",\"receiptsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"miner\":\"0x0000000000000000000000000000000000000000\",\"difficulty\":\"0x0\",\"extraData\":\"0x\",\"size\":\"0x100\",\"gasLimit\":\"0x1c9c380\",\"gasUsed\":\"0x0\",\"timestamp\":\"0x65b9a123\",\"blobGasUsed\":\"0x20000\",\"excessBlobGas\":\"0x0\"}")
           (json-bytes (make-array (length json-str) :element-type 't
                                   :fill-pointer (length json-str) :adjustable t)))
      (dotimes (i (length json-str))
        (setf (aref json-bytes i) (char-code (char json-str i))))
      (let ((result (coalton:coalton
                     (web3/block:parse-block-header
                      (coalton:lisp web3/types:Bytes () json-bytes)))))
        (assert (result-ok-p result))
        (let* ((header (result-value result))
               (post-cancun (coalton:coalton
                             (web3/block:is-post-cancun
                              (coalton:lisp web3/block:BlockHeader () header)))))
          (assert post-cancun)))))

  (test-case "gas-utilization calculates percentage"
    (let* ((json-str "{\"number\":\"0x1\",\"hash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"parentHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"nonce\":\"0x0000000000000000\",\"sha3Uncles\":\"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"logsBloom\":\"0x00\",\"transactionsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"stateRoot\":\"0xd5855eb08b3387c0af375e9cdb6acfc05eb8f519e419b874b6ff2ffda7ed1dff\",\"receiptsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"miner\":\"0x0000000000000000000000000000000000000000\",\"difficulty\":\"0x0\",\"extraData\":\"0x\",\"size\":\"0x100\",\"gasLimit\":\"0x64\",\"gasUsed\":\"0x32\",\"timestamp\":\"0x65b9a123\"}")
           (json-bytes (make-array (length json-str) :element-type 't
                                   :fill-pointer (length json-str) :adjustable t)))
      (dotimes (i (length json-str))
        (setf (aref json-bytes i) (char-code (char json-str i))))
      (let ((result (coalton:coalton
                     (web3/block:parse-block-header
                      (coalton:lisp web3/types:Bytes () json-bytes)))))
        (assert (result-ok-p result))
        (let* ((header (result-value result))
               (utilization (coalton:coalton
                             (web3/block:gas-utilization
                              (coalton:lisp web3/block:BlockHeader () header)))))
          ;; gasUsed = 0x32 = 50, gasLimit = 0x64 = 100, utilization = 50%
          (assert (= utilization 50))))))

  (test-case "block-age calculates age"
    (let* ((json-str "{\"number\":\"0x1\",\"hash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"parentHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"nonce\":\"0x0000000000000000\",\"sha3Uncles\":\"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"logsBloom\":\"0x00\",\"transactionsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"stateRoot\":\"0xd5855eb08b3387c0af375e9cdb6acfc05eb8f519e419b874b6ff2ffda7ed1dff\",\"receiptsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"miner\":\"0x0000000000000000000000000000000000000000\",\"difficulty\":\"0x0\",\"extraData\":\"0x\",\"size\":\"0x100\",\"gasLimit\":\"0x1c9c380\",\"gasUsed\":\"0x0\",\"timestamp\":\"0x64\"}")
           (json-bytes (make-array (length json-str) :element-type 't
                                   :fill-pointer (length json-str) :adjustable t)))
      (dotimes (i (length json-str))
        (setf (aref json-bytes i) (char-code (char json-str i))))
      (let ((result (coalton:coalton
                     (web3/block:parse-block-header
                      (coalton:lisp web3/types:Bytes () json-bytes)))))
        (assert (result-ok-p result))
        (let* ((header (result-value result))
               (age (coalton:coalton
                     (web3/block:block-age
                      (coalton:lisp web3/block:BlockHeader () header)
                      200))))  ; current = 200, block timestamp = 100 (0x64)
          (assert (= age 100))))))

  ;;; =========================================================================
  ;;; JSON-RPC Request Tests
  ;;; =========================================================================

  (test-case "encode-get-block-by-number-request for latest"
    (let ((request (coalton:coalton
                    (web3/block:encode-get-block-by-number-request
                     web3/block:TagLatest
                     coalton:False
                     1))))
      (assert (search "eth_getBlockByNumber" request))
      (assert (search "latest" request))))

  (test-case "encode-get-block-by-number-request for specific block"
    (let ((request (coalton:coalton
                    (web3/block:encode-get-block-by-number-request
                     (web3/block:TagNumber 12345)
                     coalton:True
                     1))))
      (assert (search "eth_getBlockByNumber" request))
      (assert (search "0x3039" request))))  ; 12345 in hex

  (test-case "encode-get-block-by-hash-request"
    (let* ((hash (result-value (coalton:coalton
                                (web3/types:hex-decode
                                 "1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef"))))
           (request (coalton:coalton
                     (web3/block:encode-get-block-by-hash-request
                      (coalton:lisp web3/types:Bytes () hash)
                      coalton:False
                      1))))
      (assert (search "eth_getBlockByHash" request))
      (assert (search "1234567890abcdef" request))))

  ;;; =========================================================================
  ;;; Response Parsing Tests
  ;;; =========================================================================

  (test-case "parse-get-block-response handles null result"
    (let* ((response "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":null}")
           (result (coalton:coalton
                    (web3/block:parse-get-block-response
                     (coalton:lisp coalton:String () response)
                     coalton:False))))
      (assert (result-ok-p result))
      (let ((block-opt (result-value result)))
        (assert (optional-none-p block-opt)))))

  (test-case "parse-get-block-response handles error"
    (let* ((response "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32602,\"message\":\"Invalid params\"}}")
           (result (coalton:coalton
                    (web3/block:parse-get-block-response
                     (coalton:lisp coalton:String () response)
                     coalton:False))))
      (assert (not (result-ok-p result)))))

  (test-case "parse-get-block-response parses valid block"
    (let* ((response "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"number\":\"0x10d4f\",\"hash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"parentHash\":\"0xabcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890\",\"nonce\":\"0x0000000000000000\",\"sha3Uncles\":\"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"logsBloom\":\"0x00\",\"transactionsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"stateRoot\":\"0xd5855eb08b3387c0af375e9cdb6acfc05eb8f519e419b874b6ff2ffda7ed1dff\",\"receiptsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"miner\":\"0x0000000000000000000000000000000000000000\",\"difficulty\":\"0x0\",\"extraData\":\"0x\",\"size\":\"0x220\",\"gasLimit\":\"0x1c9c380\",\"gasUsed\":\"0x5208\",\"timestamp\":\"0x65b9a123\",\"transactions\":[],\"uncles\":[]}}")
           (result (coalton:coalton
                    (web3/block:parse-get-block-response
                     (coalton:lisp coalton:String () response)
                     coalton:False))))
      (assert (result-ok-p result))
      (let ((block-opt (result-value result)))
        (assert (optional-some-p block-tests))
        (let* ((block (result-value block-opt))
               (header (coalton:coalton
                        (web3/block:block-header
                         (coalton:lisp web3/block:Block () block))))
               (number (coalton:coalton
                        (web3/block:header-number
                         (coalton:lisp web3/block:BlockHeader () header)))))
          (assert (= number 68943))))))  ; 0x10d4f

  ;;; =========================================================================
  ;;; Additional Withdrawal Tests
  ;;; =========================================================================

  (test-case "withdrawal-address accessor works"
    (let* ((addr (result-value (coalton:coalton
                                (web3/address:address-from-hex
                                 "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (withdrawal (coalton:coalton
                        (web3/block:make-withdrawal
                         5 200
                         (coalton:lisp web3/address:Address () addr)
                         500000)))
           (retrieved-addr (coalton:coalton
                            (web3/block:withdrawal-address
                             (coalton:lisp web3/block:Withdrawal () withdrawal))))
           (addr-hex (coalton:coalton
                      (web3/address:address-to-hex
                       (coalton:lisp web3/address:Address () retrieved-addr)))))
      (assert (string-equal (string-downcase addr-hex)
                            "0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266"))))

  (test-case "withdrawal with zero amount"
    (let* ((addr (result-value (coalton:coalton
                                (web3/address:address-from-hex
                                 "0x0000000000000000000000000000000000000001"))))
           (withdrawal (coalton:coalton
                        (web3/block:make-withdrawal
                         0 0
                         (coalton:lisp web3/address:Address () addr)
                         0))))
      (assert (= (coalton:coalton
                  (web3/block:withdrawal-index
                   (coalton:lisp web3/block:Withdrawal () withdrawal)))
                 0))
      (assert (= (coalton:coalton
                  (web3/block:withdrawal-amount
                   (coalton:lisp web3/block:Withdrawal () withdrawal)))
                 0))))

  (test-case "withdrawal with large validator index"
    (let* ((addr (result-value (coalton:coalton
                                (web3/address:address-from-hex
                                 "0x0000000000000000000000000000000000000001"))))
           (withdrawal (coalton:coalton
                        (web3/block:make-withdrawal
                         999999 1000000
                         (coalton:lisp web3/address:Address () addr)
                         32000000000))))  ; 32 ETH in Gwei
      (assert (= (coalton:coalton
                  (web3/block:withdrawal-validator-index
                   (coalton:lisp web3/block:Withdrawal () withdrawal)))
                 1000000))))

  ;;; =========================================================================
  ;;; Additional Block Header Accessor Tests
  ;;; =========================================================================

  (test-case "header-hash returns correct hash"
    (let* ((json-str "{\"number\":\"0x1\",\"hash\":\"0xabcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890\",\"parentHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"nonce\":\"0x0000000000000000\",\"sha3Uncles\":\"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"logsBloom\":\"0x00\",\"transactionsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"stateRoot\":\"0xd5855eb08b3387c0af375e9cdb6acfc05eb8f519e419b874b6ff2ffda7ed1dff\",\"receiptsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"miner\":\"0x0000000000000000000000000000000000000000\",\"difficulty\":\"0x0\",\"extraData\":\"0x\",\"size\":\"0x100\",\"gasLimit\":\"0x1c9c380\",\"gasUsed\":\"0x0\",\"timestamp\":\"0x65b9a123\"}")
           (json-bytes (make-array (length json-str) :element-type 't
                                   :fill-pointer (length json-str) :adjustable t)))
      (dotimes (i (length json-str))
        (setf (aref json-bytes i) (char-code (char json-str i))))
      (let ((result (coalton:coalton
                     (web3/block:parse-block-header
                      (coalton:lisp web3/types:Bytes () json-bytes)))))
        (assert (result-ok-p result))
        (let* ((header (result-value result))
               (hash (coalton:coalton
                      (web3/block:header-hash
                       (coalton:lisp web3/block:BlockHeader () header)))))
          (assert (= (length hash) 32))
          (assert (= (aref hash 0) #xab))
          (assert (= (aref hash 1) #xcd))))))

  (test-case "header-miner returns correct address"
    (let* ((json-str "{\"number\":\"0x1\",\"hash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"parentHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"nonce\":\"0x0000000000000000\",\"sha3Uncles\":\"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"logsBloom\":\"0x00\",\"transactionsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"stateRoot\":\"0xd5855eb08b3387c0af375e9cdb6acfc05eb8f519e419b874b6ff2ffda7ed1dff\",\"receiptsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"miner\":\"0xd8da6bf26964af9d7eed9e03e53415d37aa96045\",\"difficulty\":\"0x0\",\"extraData\":\"0x\",\"size\":\"0x100\",\"gasLimit\":\"0x1c9c380\",\"gasUsed\":\"0x0\",\"timestamp\":\"0x65b9a123\"}")
           (json-bytes (make-array (length json-str) :element-type 't
                                   :fill-pointer (length json-str) :adjustable t)))
      (dotimes (i (length json-str))
        (setf (aref json-bytes i) (char-code (char json-str i))))
      (let ((result (coalton:coalton
                     (web3/block:parse-block-header
                      (coalton:lisp web3/types:Bytes () json-bytes)))))
        (assert (result-ok-p result))
        (let* ((header (result-value result))
               (miner (coalton:coalton
                       (web3/block:header-miner
                        (coalton:lisp web3/block:BlockHeader () header))))
               (miner-hex (coalton:coalton
                           (web3/address:address-to-hex
                            (coalton:lisp web3/address:Address () miner)))))
          (assert (string-equal (string-downcase miner-hex)
                                "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))))

  (test-case "header-extra-data returns correct bytes"
    (let* ((json-str "{\"number\":\"0x1\",\"hash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"parentHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"nonce\":\"0x0000000000000000\",\"sha3Uncles\":\"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"logsBloom\":\"0x00\",\"transactionsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"stateRoot\":\"0xd5855eb08b3387c0af375e9cdb6acfc05eb8f519e419b874b6ff2ffda7ed1dff\",\"receiptsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"miner\":\"0x0000000000000000000000000000000000000000\",\"difficulty\":\"0x0\",\"extraData\":\"0xdeadbeef\",\"size\":\"0x100\",\"gasLimit\":\"0x1c9c380\",\"gasUsed\":\"0x0\",\"timestamp\":\"0x65b9a123\"}")
           (json-bytes (make-array (length json-str) :element-type 't
                                   :fill-pointer (length json-str) :adjustable t)))
      (dotimes (i (length json-str))
        (setf (aref json-bytes i) (char-code (char json-str i))))
      (let ((result (coalton:coalton
                     (web3/block:parse-block-header
                      (coalton:lisp web3/types:Bytes () json-bytes)))))
        (assert (result-ok-p result))
        (let* ((header (result-value result))
               (extra-data (coalton:coalton
                            (web3/block:header-extra-data
                             (coalton:lisp web3/block:BlockHeader () header)))))
          (assert (= (length extra-data) 4))
          (assert (= (aref extra-data 0) #xde))
          (assert (= (aref extra-data 1) #xad))
          (assert (= (aref extra-data 2) #xbe))
          (assert (= (aref extra-data 3) #xef))))))

  (test-case "header-size returns correct value"
    (let* ((json-str "{\"number\":\"0x1\",\"hash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"parentHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"nonce\":\"0x0000000000000000\",\"sha3Uncles\":\"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"logsBloom\":\"0x00\",\"transactionsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"stateRoot\":\"0xd5855eb08b3387c0af375e9cdb6acfc05eb8f519e419b874b6ff2ffda7ed1dff\",\"receiptsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"miner\":\"0x0000000000000000000000000000000000000000\",\"difficulty\":\"0x0\",\"extraData\":\"0x\",\"size\":\"0x1234\",\"gasLimit\":\"0x1c9c380\",\"gasUsed\":\"0x0\",\"timestamp\":\"0x65b9a123\"}")
           (json-bytes (make-array (length json-str) :element-type 't
                                   :fill-pointer (length json-str) :adjustable t)))
      (dotimes (i (length json-str))
        (setf (aref json-bytes i) (char-code (char json-str i))))
      (let ((result (coalton:coalton
                     (web3/block:parse-block-header
                      (coalton:lisp web3/types:Bytes () json-bytes)))))
        (assert (result-ok-p result))
        (let* ((header (result-value result))
               (size (coalton:coalton
                      (web3/block:header-size
                       (coalton:lisp web3/block:BlockHeader () header)))))
          (assert (= size #x1234))))))

  ;;; =========================================================================
  ;;; Pre-Merge Block Tests (PoW)
  ;;; =========================================================================

  (test-case "is-post-merge returns false for pre-merge block"
    (let* ((json-str "{\"number\":\"0x1\",\"hash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"parentHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"nonce\":\"0x1234567890abcdef\",\"sha3Uncles\":\"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"logsBloom\":\"0x00\",\"transactionsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"stateRoot\":\"0xd5855eb08b3387c0af375e9cdb6acfc05eb8f519e419b874b6ff2ffda7ed1dff\",\"receiptsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"miner\":\"0x0000000000000000000000000000000000000000\",\"difficulty\":\"0x2000000\",\"extraData\":\"0x\",\"size\":\"0x100\",\"gasLimit\":\"0x1c9c380\",\"gasUsed\":\"0x0\",\"timestamp\":\"0x65b9a123\"}")
           (json-bytes (make-array (length json-str) :element-type 't
                                   :fill-pointer (length json-str) :adjustable t)))
      (dotimes (i (length json-str))
        (setf (aref json-bytes i) (char-code (char json-str i))))
      (let ((result (coalton:coalton
                     (web3/block:parse-block-header
                      (coalton:lisp web3/types:Bytes () json-bytes)))))
        (assert (result-ok-p result))
        (let* ((header (result-value result))
               (post-merge (coalton:coalton
                            (web3/block:is-post-merge
                             (coalton:lisp web3/block:BlockHeader () header)))))
          (assert (not post-merge))))))

  (test-case "is-post-shanghai returns false for pre-Shanghai block"
    (let* ((json-str "{\"number\":\"0x1\",\"hash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"parentHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"nonce\":\"0x0000000000000000\",\"sha3Uncles\":\"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"logsBloom\":\"0x00\",\"transactionsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"stateRoot\":\"0xd5855eb08b3387c0af375e9cdb6acfc05eb8f519e419b874b6ff2ffda7ed1dff\",\"receiptsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"miner\":\"0x0000000000000000000000000000000000000000\",\"difficulty\":\"0x0\",\"extraData\":\"0x\",\"size\":\"0x100\",\"gasLimit\":\"0x1c9c380\",\"gasUsed\":\"0x0\",\"timestamp\":\"0x65b9a123\"}")
           (json-bytes (make-array (length json-str) :element-type 't
                                   :fill-pointer (length json-str) :adjustable t)))
      (dotimes (i (length json-str))
        (setf (aref json-bytes i) (char-code (char json-str i))))
      (let ((result (coalton:coalton
                     (web3/block:parse-block-header
                      (coalton:lisp web3/types:Bytes () json-bytes)))))
        (assert (result-ok-p result))
        (let* ((header (result-value result))
               (post-shanghai (coalton:coalton
                               (web3/block:is-post-shanghai
                                (coalton:lisp web3/block:BlockHeader () header)))))
          (assert (not post-shanghai))))))

  (test-case "is-post-cancun returns false for pre-Cancun block"
    (let* ((json-str "{\"number\":\"0x1\",\"hash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"parentHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"nonce\":\"0x0000000000000000\",\"sha3Uncles\":\"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"logsBloom\":\"0x00\",\"transactionsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"stateRoot\":\"0xd5855eb08b3387c0af375e9cdb6acfc05eb8f519e419b874b6ff2ffda7ed1dff\",\"receiptsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"miner\":\"0x0000000000000000000000000000000000000000\",\"difficulty\":\"0x0\",\"extraData\":\"0x\",\"size\":\"0x100\",\"gasLimit\":\"0x1c9c380\",\"gasUsed\":\"0x0\",\"timestamp\":\"0x65b9a123\",\"withdrawalsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\"}")
           (json-bytes (make-array (length json-str) :element-type 't
                                   :fill-pointer (length json-str) :adjustable t)))
      (dotimes (i (length json-str))
        (setf (aref json-bytes i) (char-code (char json-str i))))
      (let ((result (coalton:coalton
                     (web3/block:parse-block-header
                      (coalton:lisp web3/types:Bytes () json-bytes)))))
        (assert (result-ok-p result))
        (let* ((header (result-value result))
               (post-cancun (coalton:coalton
                             (web3/block:is-post-cancun
                              (coalton:lisp web3/block:BlockHeader () header)))))
          (assert (not post-cancun))))))

  ;;; =========================================================================
  ;;; Gas Utilization Edge Cases
  ;;; =========================================================================

  (test-case "gas-utilization with 100% usage"
    (let* ((json-str "{\"number\":\"0x1\",\"hash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"parentHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"nonce\":\"0x0000000000000000\",\"sha3Uncles\":\"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"logsBloom\":\"0x00\",\"transactionsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"stateRoot\":\"0xd5855eb08b3387c0af375e9cdb6acfc05eb8f519e419b874b6ff2ffda7ed1dff\",\"receiptsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"miner\":\"0x0000000000000000000000000000000000000000\",\"difficulty\":\"0x0\",\"extraData\":\"0x\",\"size\":\"0x100\",\"gasLimit\":\"0x1c9c380\",\"gasUsed\":\"0x1c9c380\",\"timestamp\":\"0x65b9a123\"}")
           (json-bytes (make-array (length json-str) :element-type 't
                                   :fill-pointer (length json-str) :adjustable t)))
      (dotimes (i (length json-str))
        (setf (aref json-bytes i) (char-code (char json-str i))))
      (let ((result (coalton:coalton
                     (web3/block:parse-block-header
                      (coalton:lisp web3/types:Bytes () json-bytes)))))
        (assert (result-ok-p result))
        (let* ((header (result-value result))
               (utilization (coalton:coalton
                             (web3/block:gas-utilization
                              (coalton:lisp web3/block:BlockHeader () header)))))
          (assert (= utilization 100))))))

  (test-case "gas-utilization with 0% usage"
    (let* ((json-str "{\"number\":\"0x1\",\"hash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"parentHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"nonce\":\"0x0000000000000000\",\"sha3Uncles\":\"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"logsBloom\":\"0x00\",\"transactionsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"stateRoot\":\"0xd5855eb08b3387c0af375e9cdb6acfc05eb8f519e419b874b6ff2ffda7ed1dff\",\"receiptsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"miner\":\"0x0000000000000000000000000000000000000000\",\"difficulty\":\"0x0\",\"extraData\":\"0x\",\"size\":\"0x100\",\"gasLimit\":\"0x1c9c380\",\"gasUsed\":\"0x0\",\"timestamp\":\"0x65b9a123\"}")
           (json-bytes (make-array (length json-str) :element-type 't
                                   :fill-pointer (length json-str) :adjustable t)))
      (dotimes (i (length json-str))
        (setf (aref json-bytes i) (char-code (char json-str i))))
      (let ((result (coalton:coalton
                     (web3/block:parse-block-header
                      (coalton:lisp web3/types:Bytes () json-bytes)))))
        (assert (result-ok-p result))
        (let* ((header (result-value result))
               (utilization (coalton:coalton
                             (web3/block:gas-utilization
                              (coalton:lisp web3/block:BlockHeader () header)))))
          (assert (= utilization 0))))))

  ;;; =========================================================================
  ;;; Block Accessor Tests
  ;;; =========================================================================

  (test-case "block-transactions returns empty list for block without txs"
    (let* ((response "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"number\":\"0x1\",\"hash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"parentHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"nonce\":\"0x0000000000000000\",\"sha3Uncles\":\"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"logsBloom\":\"0x00\",\"transactionsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"stateRoot\":\"0xd5855eb08b3387c0af375e9cdb6acfc05eb8f519e419b874b6ff2ffda7ed1dff\",\"receiptsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"miner\":\"0x0000000000000000000000000000000000000000\",\"difficulty\":\"0x0\",\"extraData\":\"0x\",\"size\":\"0x220\",\"gasLimit\":\"0x1c9c380\",\"gasUsed\":\"0x0\",\"timestamp\":\"0x65b9a123\",\"transactions\":[],\"uncles\":[]}}")
           (result (coalton:coalton
                    (web3/block:parse-get-block-response
                     (coalton:lisp coalton:String () response)
                     coalton:False))))
      (assert (result-ok-p result))
      (let ((block-opt (result-value result)))
        (assert (optional-some-p block-tests))
        (let* ((block (result-value block-opt))
               (txs (coalton:coalton
                     (web3/block:block-transactions
                      (coalton:lisp web3/block:Block () block))))
               (tx-count (coalton:coalton
                          (coalton-library/list:length
                           (coalton:lisp (coalton:List web3/block:BlockTx) () txs)))))
          (assert (= tx-count 0))))))

  (test-case "block-uncles returns empty list for block without uncles"
    (let* ((response "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"number\":\"0x1\",\"hash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"parentHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"nonce\":\"0x0000000000000000\",\"sha3Uncles\":\"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"logsBloom\":\"0x00\",\"transactionsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"stateRoot\":\"0xd5855eb08b3387c0af375e9cdb6acfc05eb8f519e419b874b6ff2ffda7ed1dff\",\"receiptsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"miner\":\"0x0000000000000000000000000000000000000000\",\"difficulty\":\"0x0\",\"extraData\":\"0x\",\"size\":\"0x220\",\"gasLimit\":\"0x1c9c380\",\"gasUsed\":\"0x0\",\"timestamp\":\"0x65b9a123\",\"transactions\":[],\"uncles\":[]}}")
           (result (coalton:coalton
                    (web3/block:parse-get-block-response
                     (coalton:lisp coalton:String () response)
                     coalton:False))))
      (assert (result-ok-p result))
      (let ((block-opt (result-value result)))
        (assert (optional-some-p block-tests))
        (let* ((block (result-value block-opt))
               (uncles (coalton:coalton
                        (web3/block:block-uncles
                         (coalton:lisp web3/block:Block () block))))
               (uncle-count (coalton:coalton
                             (coalton-library/list:length
                              (coalton:lisp (coalton:List web3/types:Bytes) () uncles)))))
          (assert (= uncle-count 0))))))

  (test-case "block-withdrawals returns empty list for pre-Shanghai block"
    (let* ((response "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"number\":\"0x1\",\"hash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"parentHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"nonce\":\"0x0000000000000000\",\"sha3Uncles\":\"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"logsBloom\":\"0x00\",\"transactionsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"stateRoot\":\"0xd5855eb08b3387c0af375e9cdb6acfc05eb8f519e419b874b6ff2ffda7ed1dff\",\"receiptsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"miner\":\"0x0000000000000000000000000000000000000000\",\"difficulty\":\"0x0\",\"extraData\":\"0x\",\"size\":\"0x220\",\"gasLimit\":\"0x1c9c380\",\"gasUsed\":\"0x0\",\"timestamp\":\"0x65b9a123\",\"transactions\":[],\"uncles\":[]}}")
           (result (coalton:coalton
                    (web3/block:parse-get-block-response
                     (coalton:lisp coalton:String () response)
                     coalton:False))))
      (assert (result-ok-p result))
      (let ((block-opt (result-value result)))
        (assert (optional-some-p block-tests))
        (let* ((block (result-value block-opt))
               (withdrawals (coalton:coalton
                             (web3/block:block-withdrawals
                              (coalton:lisp web3/block:Block () block))))
               (withdrawal-count (coalton:coalton
                                  (coalton-library/list:length
                                   (coalton:lisp (coalton:List web3/block:Withdrawal) () withdrawals)))))
          (assert (= withdrawal-count 0))))))

  ;;; =========================================================================
  ;;; Block Number Edge Cases
  ;;; =========================================================================

  (test-case "block-tag-to-string for block 0 (genesis)"
    (let ((result (coalton:coalton (web3/block:block-tag-to-string (web3/block:TagNumber 0)))))
      (assert (string= result "0x0"))))

  (test-case "block-tag-to-string for large block number"
    (let ((result (coalton:coalton (web3/block:block-tag-to-string (web3/block:TagNumber 20000000)))))
      (assert (string= result "0x1312D00"))))

  ;;; =========================================================================
  ;;; Cancun Block Fields Tests
  ;;; =========================================================================

  (test-case "header-blob-gas-used accessor"
    (let* ((json-str "{\"number\":\"0x1\",\"hash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"parentHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"nonce\":\"0x0000000000000000\",\"sha3Uncles\":\"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"logsBloom\":\"0x00\",\"transactionsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"stateRoot\":\"0xd5855eb08b3387c0af375e9cdb6acfc05eb8f519e419b874b6ff2ffda7ed1dff\",\"receiptsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"miner\":\"0x0000000000000000000000000000000000000000\",\"difficulty\":\"0x0\",\"extraData\":\"0x\",\"size\":\"0x100\",\"gasLimit\":\"0x1c9c380\",\"gasUsed\":\"0x0\",\"timestamp\":\"0x65b9a123\",\"blobGasUsed\":\"0x60000\",\"excessBlobGas\":\"0x20000\"}")
           (json-bytes (make-array (length json-str) :element-type 't
                                   :fill-pointer (length json-str) :adjustable t)))
      (dotimes (i (length json-str))
        (setf (aref json-bytes i) (char-code (char json-str i))))
      (let ((result (coalton:coalton
                     (web3/block:parse-block-header
                      (coalton:lisp web3/types:Bytes () json-bytes)))))
        (assert (result-ok-p result))
        (let* ((header (result-value result))
               (blob-gas-opt (coalton:coalton
                              (web3/block:header-blob-gas-used
                               (coalton:lisp web3/block:BlockHeader () header)))))
          (assert (optional-some-p block-tests))
          (let ((blob-gas (result-value blob-gas-opt)))
            (assert (= blob-gas #x60000)))))))

  (test-case "header-excess-blob-gas accessor"
    (let* ((json-str "{\"number\":\"0x1\",\"hash\":\"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef\",\"parentHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"nonce\":\"0x0000000000000000\",\"sha3Uncles\":\"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"logsBloom\":\"0x00\",\"transactionsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"stateRoot\":\"0xd5855eb08b3387c0af375e9cdb6acfc05eb8f519e419b874b6ff2ffda7ed1dff\",\"receiptsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"miner\":\"0x0000000000000000000000000000000000000000\",\"difficulty\":\"0x0\",\"extraData\":\"0x\",\"size\":\"0x100\",\"gasLimit\":\"0x1c9c380\",\"gasUsed\":\"0x0\",\"timestamp\":\"0x65b9a123\",\"blobGasUsed\":\"0x60000\",\"excessBlobGas\":\"0x40000\"}")
           (json-bytes (make-array (length json-str) :element-type 't
                                   :fill-pointer (length json-str) :adjustable t)))
      (dotimes (i (length json-str))
        (setf (aref json-bytes i) (char-code (char json-str i))))
      (let ((result (coalton:coalton
                     (web3/block:parse-block-header
                      (coalton:lisp web3/types:Bytes () json-bytes)))))
        (assert (result-ok-p result))
        (let* ((header (result-value result))
               (excess-blob-opt (coalton:coalton
                                 (web3/block:header-excess-blob-gas
                                  (coalton:lisp web3/block:BlockHeader () header)))))
          (assert (optional-some-p block-tests))
          (let ((excess-blob (result-value excess-blob-opt)))
            (assert (= excess-blob #x40000))))))))

