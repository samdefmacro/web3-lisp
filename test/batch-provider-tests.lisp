;;; Batch Provider module tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Batch Provider Tests
;;; =========================================================================

(defun run-batch-provider-tests ()
  (format t "~%=== Batch Provider Tests ===~%")

  ;;; =========================================================================
  ;;; RpcRequest Creation Tests
  ;;; =========================================================================

  (test-case "make-rpc-request creates request with id, method, params"
    (let ((req (coalton:coalton
                (web3/batch-provider:make-rpc-request 1 "eth_chainId" "[]"))))
      (assert (not (null req)))))

  (test-case "make-rpc-request with different IDs"
    (let ((req1 (coalton:coalton (web3/batch-provider:make-rpc-request 0 "eth_chainId" "[]")))
          (req2 (coalton:coalton (web3/batch-provider:make-rpc-request 1 "eth_blockNumber" "[]")))
          (req3 (coalton:coalton (web3/batch-provider:make-rpc-request 2 "eth_gasPrice" "[]"))))
      (assert (not (null req1)))
      (assert (not (null req2)))
      (assert (not (null req3)))))

  ;;; =========================================================================
  ;;; BatchResult Tests
  ;;; =========================================================================

  (test-case "BatchOk result-id returns correct id"
    (let ((id (coalton:coalton
               (web3/batch-provider:batch-result-id
                (web3/batch-provider:BatchOk 42 "0x1")))))
      (assert (= id 42))))

  (test-case "BatchErr result-id returns correct id"
    (let ((id (coalton:coalton
               (web3/batch-provider:batch-result-id
                (web3/batch-provider:BatchErr 7 "some error")))))
      (assert (= id 7))))

  (test-case "BatchOk result-value returns Ok"
    (let ((result (coalton:coalton
                   (web3/batch-provider:batch-result-value
                    (web3/batch-provider:BatchOk 0 "0x1")))))
      (assert (result-ok-p result))
      (assert (string= (result-value result) "0x1"))))

  (test-case "BatchErr result-value returns Err"
    (let ((result (coalton:coalton
                   (web3/batch-provider:batch-result-value
                    (web3/batch-provider:BatchErr 0 "RPC error: method not found")))))
      (assert (result-err-p result))))

  ;;; =========================================================================
  ;;; Convenience Builder Tests
  ;;; =========================================================================

  (test-case "batch-eth-chain-id creates valid request"
    (let ((req (coalton:coalton (web3/batch-provider:batch-eth-chain-id 0))))
      (assert (not (null req)))))

  (test-case "batch-eth-block-number creates valid request"
    (let ((req (coalton:coalton (web3/batch-provider:batch-eth-block-number 1))))
      (assert (not (null req)))))

  (test-case "batch-eth-gas-price creates valid request"
    (let ((req (coalton:coalton (web3/batch-provider:batch-eth-gas-price 2))))
      (assert (not (null req)))))

  (test-case "batch-eth-max-priority-fee-per-gas creates valid request"
    (let ((req (coalton:coalton (web3/batch-provider:batch-eth-max-priority-fee-per-gas 3))))
      (assert (not (null req)))))

  (test-case "batch-eth-get-balance creates request with address"
    (let* ((addr (result-value (coalton:coalton
                                (web3/address:address-from-hex
                                 "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (req (coalton:coalton
                 (web3/batch-provider:batch-eth-get-balance
                  0
                  (coalton:lisp web3/address:Address () addr)))))
      (assert (not (null req)))))

  (test-case "batch-eth-get-transaction-count creates request with address"
    (let* ((addr (result-value (coalton:coalton
                                (web3/address:address-from-hex
                                 "0x0000000000000000000000000000000000000000"))))
           (req (coalton:coalton
                 (web3/batch-provider:batch-eth-get-transaction-count
                  1
                  (coalton:lisp web3/address:Address () addr)))))
      (assert (not (null req)))))

  (test-case "batch-eth-call creates request with to address and data"
    (let* ((to (result-value (coalton:coalton
                              (web3/address:address-from-hex
                               "0xdAC17F958D2ee523a2206206994597C13D831ec7"))))
           (calldata (make-array 4 :fill-pointer 4 :adjustable t
                                  :initial-contents '(#x18 #x16 #x0d #xdd)))
           (req (coalton:coalton
                 (web3/batch-provider:batch-eth-call
                  0
                  coalton-prelude:None
                  (coalton:lisp web3/address:Address () to)
                  (coalton:lisp web3/types:Bytes () calldata)))))
      (assert (not (null req)))))

  (test-case "batch-eth-call creates request with from and to address"
    (let* ((from (result-value (coalton:coalton
                                (web3/address:address-from-hex
                                 "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (to (result-value (coalton:coalton
                              (web3/address:address-from-hex
                               "0xdAC17F958D2ee523a2206206994597C13D831ec7"))))
           (calldata (make-array 4 :fill-pointer 4 :adjustable t
                                  :initial-contents '(#x18 #x16 #x0d #xdd)))
           (req (coalton:coalton
                 (web3/batch-provider:batch-eth-call
                  0
                  (coalton-prelude:Some (coalton:lisp web3/address:Address () from))
                  (coalton:lisp web3/address:Address () to)
                  (coalton:lisp web3/types:Bytes () calldata)))))
      (assert (not (null req)))))

  (test-case "batch-eth-get-code creates request with address"
    (let* ((addr (result-value (coalton:coalton
                                (web3/address:address-from-hex
                                 "0xdAC17F958D2ee523a2206206994597C13D831ec7"))))
           (req (coalton:coalton
                 (web3/batch-provider:batch-eth-get-code
                  0
                  (coalton:lisp web3/address:Address () addr)))))
      (assert (not (null req)))))

  (test-case "batch-eth-get-storage-at creates request with address and slot"
    (let* ((addr (result-value (coalton:coalton
                                (web3/address:address-from-hex
                                 "0xdAC17F958D2ee523a2206206994597C13D831ec7"))))
           (slot (coalton:coalton web3/types:u256-zero))
           (req (coalton:coalton
                 (web3/batch-provider:batch-eth-get-storage-at
                  0
                  (coalton:lisp web3/address:Address () addr)
                  (coalton:lisp web3/types:U256 () slot)))))
      (assert (not (null req)))))

  ;;; =========================================================================
  ;;; CL-level list conversion tests
  ;;; =========================================================================

  (test-case "coalton-list-to-cl-triples extracts id, method, params"
    (let* ((lst (coalton:coalton
                (coalton:Cons
                 (web3/batch-provider:make-rpc-request 7 "eth_chainId" "[]")
                 (coalton:Cons
                  (web3/batch-provider:make-rpc-request 8 "eth_blockNumber" "[]")
                  coalton:Nil))))
           (triples (web3/batch-provider::%coalton-list-to-cl-triples lst)))
      (assert (= (length triples) 2))
      ;; First triple
      (assert (= (first (first triples)) 7))
      (assert (string= (second (first triples)) "eth_chainId"))
      (assert (string= (third (first triples)) "[]"))
      ;; Second triple
      (assert (= (first (second triples)) 8))
      (assert (string= (second (second triples)) "eth_blockNumber"))))

  (test-case "coalton-list-to-cl-triples handles empty list"
    (let ((triples (web3/batch-provider::%coalton-list-to-cl-triples nil)))
      (assert (null triples))))

  ;;; =========================================================================
  ;;; CL-level JSON building tests
  ;;; =========================================================================

  (test-case "batch JSON builds valid single-request array"
    (let ((json (web3/batch-provider::%build-batch-json
                 '((1 "eth_chainId" "[]")))))
      (assert (char= (char json 0) #\[))
      (assert (char= (char json (1- (length json))) #\]))
      (assert (search "eth_chainId" json))
      (assert (search "\"id\":1" json))))

  (test-case "batch JSON builds valid multi-request array"
    (let ((json (web3/batch-provider::%build-batch-json
                 '((0 "eth_chainId" "[]")
                   (1 "eth_blockNumber" "[]")
                   (2 "eth_gasPrice" "[]")))))
      ;; Should be a JSON array with 3 elements
      (assert (search "eth_chainId" json))
      (assert (search "eth_blockNumber" json))
      (assert (search "eth_gasPrice" json))
      (assert (search "\"id\":0" json))
      (assert (search "\"id\":1" json))
      (assert (search "\"id\":2" json))))

  (test-case "batch JSON builds valid request with params"
    (let ((json (web3/batch-provider::%build-batch-json
                 '((0 "eth_getBalance" "[\"0xd8da6bf26964af9d7eed9e03e53415d37aa96045\",\"latest\"]")))))
      (assert (search "eth_getBalance" json))
      (assert (search "0xd8da6bf26964af9d7eed9e03e53415d37aa96045" json))
      (assert (search "latest" json))))

  ;;; =========================================================================
  ;;; CL-level response parsing tests
  ;;; =========================================================================

  (test-case "parse batch response with single result"
    (let ((results (web3/batch-provider::%parse-batch-response
                    "[{\"jsonrpc\":\"2.0\",\"id\":0,\"result\":\"0x1\"}]")))
      (assert (= (length results) 1))
      (assert (= (first (first results)) 0))      ; id
      (assert (null (second (first results))))     ; no error
      (assert (string= (third (first results)) "0x1"))))  ; result

  (test-case "parse batch response with multiple results"
    (let ((results (web3/batch-provider::%parse-batch-response
                    "[{\"jsonrpc\":\"2.0\",\"id\":0,\"result\":\"0x1\"},{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":\"0xabc\"}]")))
      (assert (= (length results) 2))
      ;; Should be sorted by id
      (assert (= (first (first results)) 0))
      (assert (= (first (second results)) 1))
      (assert (string= (third (first results)) "0x1"))
      (assert (string= (third (second results)) "0xabc"))))

  (test-case "parse batch response with out-of-order ids"
    (let ((results (web3/batch-provider::%parse-batch-response
                    "[{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":\"0xc\"},{\"jsonrpc\":\"2.0\",\"id\":0,\"result\":\"0xa\"},{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":\"0xb\"}]")))
      (assert (= (length results) 3))
      ;; Should be sorted by id regardless of response order
      (assert (= (first (first results)) 0))
      (assert (= (first (second results)) 1))
      (assert (= (first (third results)) 2))
      (assert (string= (third (first results)) "0xa"))
      (assert (string= (third (second results)) "0xb"))
      (assert (string= (third (third results)) "0xc"))))

  (test-case "parse batch response with error entry"
    (let ((results (web3/batch-provider::%parse-batch-response
                    "[{\"jsonrpc\":\"2.0\",\"id\":0,\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}]")))
      (assert (= (length results) 1))
      (assert (= (first (first results)) 0))
      ;; error-val should be present
      (assert (not (null (second (first results)))))
      ;; result-val should be null
      (assert (null (third (first results))))))

  (test-case "parse batch response with mixed success and error"
    (let ((results (web3/batch-provider::%parse-batch-response
                    "[{\"jsonrpc\":\"2.0\",\"id\":0,\"result\":\"0x1\"},{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32602,\"message\":\"Invalid params\"}}]")))
      (assert (= (length results) 2))
      ;; First should be success
      (assert (null (second (first results))))
      (assert (string= (third (first results)) "0x1"))
      ;; Second should be error
      (assert (not (null (second (second results)))))
      (assert (null (third (second results))))))

  ;;; =========================================================================
  ;;; Batch call error handling tests (no network required)
  ;;; =========================================================================

  (test-case "batch-call with empty list returns Ok Nil"
    (let* ((provider (coalton:coalton
                      (web3/provider:make-http-provider "http://127.0.0.1:1")))
           (result (coalton:coalton
                    (web3/batch-provider:batch-call
                     (coalton:lisp web3/provider:HttpProvider () provider)
                     coalton:Nil))))
      ;; Should succeed without making any HTTP call
      (assert (result-ok-p result))))

  (test-case "batch-call-raw with empty list returns Ok Nil"
    (let* ((provider (coalton:coalton
                      (web3/provider:make-http-provider "http://127.0.0.1:1")))
           (result (coalton:coalton
                    (web3/batch-provider:batch-call-raw
                     (coalton:lisp web3/provider:HttpProvider () provider)
                     coalton:Nil))))
      ;; Should succeed without making any HTTP call
      (assert (result-ok-p result))))

  (test-case "batch-call returns error on connection failure"
    (let* ((provider (coalton:coalton
                      (web3/provider:make-http-provider "http://127.0.0.1:1")))
           (result (coalton:coalton
                    (web3/batch-provider:batch-call
                     (coalton:lisp web3/provider:HttpProvider () provider)
                     (coalton:Cons
                      (web3/batch-provider:batch-eth-chain-id 0)
                      (coalton:Cons
                       (web3/batch-provider:batch-eth-block-number 1)
                       coalton:Nil))))))
      (assert (result-err-p result))))

  (test-case "batch-call-raw returns error on connection failure"
    (let* ((provider (coalton:coalton
                      (web3/provider:make-http-provider "http://127.0.0.1:1")))
           (result (coalton:coalton
                    (web3/batch-provider:batch-call-raw
                     (coalton:lisp web3/provider:HttpProvider () provider)
                     (coalton:Cons
                      (web3/batch-provider:batch-eth-gas-price 0)
                      coalton:Nil)))))
      (assert (result-err-p result))))

  ;;; =========================================================================
  ;;; Integration Tests (require WEB3_TEST_RPC_URL environment variable)
  ;;; =========================================================================

  (if (uiop:getenv "WEB3_TEST_RPC_URL")
      (progn
        (format t "~%  --- Batch Provider Integration tests ---~%")
        (let ((provider (coalton:coalton
                         (web3/provider:make-http-provider
                          (coalton:lisp coalton:String ()
                            (uiop:getenv "WEB3_TEST_RPC_URL"))))))

          (test-case "batch-call with chain ID and block number (integration)"
            (let ((result (coalton:coalton
                           (web3/batch-provider:batch-call
                            (coalton:lisp web3/provider:HttpProvider () provider)
                            (coalton:Cons
                             (web3/batch-provider:batch-eth-chain-id 0)
                             (coalton:Cons
                              (web3/batch-provider:batch-eth-block-number 1)
                              coalton:Nil))))))
              (assert (result-ok-p result))))

          (test-case "batch-call with 3 different methods (integration)"
            (let ((result (coalton:coalton
                           (web3/batch-provider:batch-call
                            (coalton:lisp web3/provider:HttpProvider () provider)
                            (coalton:Cons
                             (web3/batch-provider:batch-eth-chain-id 0)
                             (coalton:Cons
                              (web3/batch-provider:batch-eth-block-number 1)
                              (coalton:Cons
                               (web3/batch-provider:batch-eth-gas-price 2)
                               coalton:Nil)))))))
              (assert (result-ok-p result))))

          (test-case "batch-call-raw returns BatchOk results (integration)"
            (let ((result (coalton:coalton
                           (web3/batch-provider:batch-call-raw
                            (coalton:lisp web3/provider:HttpProvider () provider)
                            (coalton:Cons
                             (web3/batch-provider:batch-eth-chain-id 0)
                             coalton:Nil)))))
              (assert (result-ok-p result))))

          (test-case "batch-call with balance queries (integration)"
            (let* ((addr (result-value (coalton:coalton
                                        (web3/address:address-from-hex
                                         "0x0000000000000000000000000000000000000000"))))
                   (result (coalton:coalton
                            (web3/batch-provider:batch-call
                             (coalton:lisp web3/provider:HttpProvider () provider)
                             (coalton:Cons
                              (web3/batch-provider:batch-eth-get-balance
                               0
                               (coalton:lisp web3/address:Address () addr))
                              (coalton:Cons
                               (web3/batch-provider:batch-eth-get-transaction-count
                                1
                                (coalton:lisp web3/address:Address () addr))
                               coalton:Nil))))))
              (assert (result-ok-p result))))))

      ;; No integration tests
      (format t "~%  Note: Set WEB3_TEST_RPC_URL for batch provider integration tests~%")))
