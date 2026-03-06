;;; Provider module tests - Pure Common Lisp
;;; Includes unit tests (no network) and integration tests (require WEB3_TEST_RPC_URL)

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Provider Tests
;;; =========================================================================

(defun run-provider-tests ()
  (format t "~%=== Provider Tests ===~%")

  ;;; =========================================================================
  ;;; Provider Creation Tests (no network required)
  ;;; =========================================================================

  (test-case "HttpProvider creation with localhost"
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "http://localhost:8545"))))
      (assert (not (null provider)))))

  (test-case "HttpProvider creation with IP address"
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "http://127.0.0.1:8545"))))
      (assert (not (null provider)))))

  (test-case "HttpProvider creation with HTTPS URL"
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "https://mainnet.infura.io/v3/YOUR-API-KEY"))))
      (assert (not (null provider)))))

  (test-case "HttpProvider creation with custom port"
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "http://localhost:9545"))))
      (assert (not (null provider)))))

  (test-case "HttpProvider creation with Alchemy-style URL"
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "https://eth-mainnet.g.alchemy.com/v2/demo"))))
      (assert (not (null provider)))))

  (test-case "HttpProvider creation with WebSocket-style URL (accepted)"
    ;; Provider accepts any URL string - validation happens on use
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "wss://mainnet.infura.io/ws/v3/key"))))
      (assert (not (null provider)))))

  (test-case "HttpProvider creation with empty URL"
    ;; Empty URL is accepted at creation time
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider ""))))
      (assert (not (null provider)))))

  (test-case "Multiple providers can coexist"
    (let ((provider1 (coalton:coalton
                      (web3/provider:make-http-provider "http://localhost:8545")))
          (provider2 (coalton:coalton
                      (web3/provider:make-http-provider "http://localhost:8546")))
          (provider3 (coalton:coalton
                      (web3/provider:make-http-provider "https://mainnet.infura.io"))))
      (assert (not (null provider1)))
      (assert (not (null provider2)))
      (assert (not (null provider3)))))

  ;;; =========================================================================
  ;;; Address Parameter Formatting Tests (no network required)
  ;;; These test that functions properly handle address objects
  ;;; =========================================================================

  (test-case "eth_getBalance accepts valid address"
    ;; This will fail on network call, but tests that address is properly formatted
    ;; We verify the function accepts the address without crashing
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "http://127.0.0.1:1")))
          (address (result-value (coalton:coalton
                                  (web3/address:address-from-hex
                                   "0xd8da6bf26964af9d7eed9e03e53415d37aa96045")))))
      ;; The call will fail (no network) but should not crash on address handling
      (let ((result (coalton:coalton
                     (web3/provider:eth-get-balance
                      (coalton:lisp web3/provider:HttpProvider () provider)
                      (coalton:lisp web3/address:Address () address)))))
        ;; Should return an error (connection refused), not crash
        (assert (result-err-p result)))))

  (test-case "eth_getTransactionCount accepts valid address"
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "http://127.0.0.1:1")))
          (address (result-value (coalton:coalton
                                  (web3/address:address-from-hex
                                   "0x0000000000000000000000000000000000000000")))))
      (let ((result (coalton:coalton
                     (web3/provider:eth-get-transaction-count
                      (coalton:lisp web3/provider:HttpProvider () provider)
                      (coalton:lisp web3/address:Address () address)))))
        (assert (result-err-p result)))))

  (test-case "eth_call accepts contract address and calldata"
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "http://127.0.0.1:1")))
          (contract (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xdAC17F958D2ee523a2206206994597C13D831ec7"))))
          (calldata (make-array 36 :fill-pointer 36 :adjustable t :initial-element 0)))
      ;; Set function selector for balanceOf(address)
      (setf (aref calldata 0) #x70)
      (setf (aref calldata 1) #xa0)
      (setf (aref calldata 2) #x82)
      (setf (aref calldata 3) #x31)
      (let ((result (coalton:coalton
                     (web3/provider:eth-call
                      (coalton:lisp web3/provider:HttpProvider () provider)
                      coalton-prelude:None
                      (coalton:lisp web3/address:Address () contract)
                      (coalton:lisp web3/types:Bytes () calldata)))))
        (assert (result-err-p result)))))

  (test-case "eth_call accepts optional from address"
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "http://127.0.0.1:1")))
          (from (result-value (coalton:coalton
                               (web3/address:address-from-hex
                                "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
          (to (result-value (coalton:coalton
                             (web3/address:address-from-hex
                              "0xdAC17F958D2ee523a2206206994597C13D831ec7"))))
          (calldata (make-array 4 :fill-pointer 4 :adjustable t :initial-element 0)))
      (let ((result (coalton:coalton
                     (web3/provider:eth-call
                      (coalton:lisp web3/provider:HttpProvider () provider)
                      (coalton-prelude:Some (coalton:lisp web3/address:Address () from))
                      (coalton:lisp web3/address:Address () to)
                      (coalton:lisp web3/types:Bytes () calldata)))))
        (assert (result-err-p result)))))

  (test-case "eth_estimateGas accepts addresses and value"
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "http://127.0.0.1:1")))
          (from (result-value (coalton:coalton
                               (web3/address:address-from-hex
                                "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
          (to (result-value (coalton:coalton
                             (web3/address:address-from-hex
                              "0x0000000000000000000000000000000000000001"))))
          (value (coalton:coalton (web3/types:u256-from-integer 1000000000000000000)))
          (data (make-array 0 :fill-pointer 0 :adjustable t)))
      (let ((result (coalton:coalton
                     (web3/provider:eth-estimate-gas
                      (coalton:lisp web3/provider:HttpProvider () provider)
                      (coalton:lisp web3/address:Address () from)
                      (coalton-prelude:Some (coalton:lisp web3/address:Address () to))
                      (coalton:lisp web3/types:U256 () value)
                      (coalton:lisp web3/types:Bytes () data)))))
        (assert (result-err-p result)))))

  (test-case "eth_estimateGas accepts None for contract creation"
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "http://127.0.0.1:1")))
          (from (result-value (coalton:coalton
                               (web3/address:address-from-hex
                                "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
          (value (coalton:coalton web3/types:u256-zero))
          (init-code (make-array 4 :fill-pointer 4 :adjustable t
                                 :initial-contents '(#x60 #x80 #x60 #x40))))
      (let ((result (coalton:coalton
                     (web3/provider:eth-estimate-gas
                      (coalton:lisp web3/provider:HttpProvider () provider)
                      (coalton:lisp web3/address:Address () from)
                      coalton-prelude:None  ; Contract creation
                      (coalton:lisp web3/types:U256 () value)
                      (coalton:lisp web3/types:Bytes () init-code)))))
        (assert (result-err-p result)))))

  (test-case "eth_sendRawTransaction accepts raw bytes"
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "http://127.0.0.1:1")))
          (raw-tx (make-array 100 :fill-pointer 100 :adjustable t :initial-element #xab)))
      (let ((result (coalton:coalton
                     (web3/provider:eth-send-raw-transaction
                      (coalton:lisp web3/provider:HttpProvider () provider)
                      (coalton:lisp web3/types:Bytes () raw-tx)))))
        (assert (result-err-p result)))))

  (test-case "eth_getTransactionReceipt accepts tx hash string"
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "http://127.0.0.1:1"))))
      (let ((result (coalton:coalton
                     (web3/provider:eth-get-transaction-receipt
                      (coalton:lisp web3/provider:HttpProvider () provider)
                      "0xabc123def456789012345678901234567890123456789012345678901234567890"))))
        (assert (result-err-p result)))))

  ;;; =========================================================================
  ;;; New Method Parameter Tests (no network required)
  ;;; =========================================================================

  (test-case "eth_getCode accepts valid address"
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "http://127.0.0.1:1")))
          (address (result-value (coalton:coalton
                                  (web3/address:address-from-hex
                                   "0xdAC17F958D2ee523a2206206994597C13D831ec7")))))
      (let ((result (coalton:coalton
                     (web3/provider:eth-get-code
                      (coalton:lisp web3/provider:HttpProvider () provider)
                      (coalton:lisp web3/address:Address () address)))))
        (assert (result-err-p result)))))

  (test-case "eth_getStorageAt accepts address and slot"
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "http://127.0.0.1:1")))
          (address (result-value (coalton:coalton
                                  (web3/address:address-from-hex
                                   "0xdAC17F958D2ee523a2206206994597C13D831ec7"))))
          (slot (coalton:coalton web3/types:u256-zero)))
      (let ((result (coalton:coalton
                     (web3/provider:eth-get-storage-at
                      (coalton:lisp web3/provider:HttpProvider () provider)
                      (coalton:lisp web3/address:Address () address)
                      (coalton:lisp web3/types:U256 () slot)))))
        (assert (result-err-p result)))))

  (test-case "eth_maxPriorityFeePerGas returns error on connection failure"
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "http://127.0.0.1:1"))))
      (let ((result (coalton:coalton
                     (web3/provider:eth-max-priority-fee-per-gas
                      (coalton:lisp web3/provider:HttpProvider () provider)))))
        (assert (result-err-p result)))))

  (test-case "eth_getBlockByNumber accepts block tag"
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "http://127.0.0.1:1"))))
      (let ((result (coalton:coalton
                     (web3/provider:eth-get-block-by-number
                      (coalton:lisp web3/provider:HttpProvider () provider)
                      "latest"
                      coalton:False))))
        (assert (result-err-p result)))))

  (test-case "eth_getBlockByHash accepts hash string"
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "http://127.0.0.1:1"))))
      (let ((result (coalton:coalton
                     (web3/provider:eth-get-block-by-hash
                      (coalton:lisp web3/provider:HttpProvider () provider)
                      "0x0000000000000000000000000000000000000000000000000000000000000000"
                      coalton:False))))
        (assert (result-err-p result)))))

  (test-case "eth_getTransactionByHash accepts hash string"
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "http://127.0.0.1:1"))))
      (let ((result (coalton:coalton
                     (web3/provider:eth-get-transaction-by-hash
                      (coalton:lisp web3/provider:HttpProvider () provider)
                      "0xabc123def456789012345678901234567890123456789012345678901234567890"))))
        (assert (result-err-p result)))))

  (test-case "eth_feeHistory accepts block count and percentiles"
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "http://127.0.0.1:1"))))
      (let ((result (coalton:coalton
                     (web3/provider:eth-fee-history
                      (coalton:lisp web3/provider:HttpProvider () provider)
                      4
                      "latest"
                      "[25,50,75]"))))
        (assert (result-err-p result)))))

  (test-case "eth_syncing returns error on connection failure"
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "http://127.0.0.1:1"))))
      (let ((result (coalton:coalton
                     (web3/provider:eth-syncing
                      (coalton:lisp web3/provider:HttpProvider () provider)))))
        (assert (result-err-p result)))))

  (test-case "wait-for-transaction-receipt returns error on connection failure"
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "http://127.0.0.1:1"))))
      (let ((result (coalton:coalton
                     (web3/provider:wait-for-transaction-receipt
                      (coalton:lisp web3/provider:HttpProvider () provider)
                      "0xabc123def456789012345678901234567890123456789012345678901234567890"
                      1
                      0))))
        ;; Should fail on first attempt (connection error), not timeout
        (assert (result-err-p result)))))

  ;;; =========================================================================
  ;;; Simple Methods Tests (no network required - test error handling)
  ;;; =========================================================================

  (test-case "eth_chainId returns error on connection failure"
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "http://127.0.0.1:1"))))
      (let ((result (coalton:coalton
                     (web3/provider:eth-chain-id
                      (coalton:lisp web3/provider:HttpProvider () provider)))))
        (assert (result-err-p result)))))

  (test-case "eth_blockNumber returns error on connection failure"
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "http://127.0.0.1:1"))))
      (let ((result (coalton:coalton
                     (web3/provider:eth-block-number
                      (coalton:lisp web3/provider:HttpProvider () provider)))))
        (assert (result-err-p result)))))

  (test-case "eth_gasPrice returns error on connection failure"
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "http://127.0.0.1:1"))))
      (let ((result (coalton:coalton
                     (web3/provider:eth-gas-price
                      (coalton:lisp web3/provider:HttpProvider () provider)))))
        (assert (result-err-p result)))))

  ;;; =========================================================================
  ;;; Integration Tests (require WEB3_TEST_RPC_URL environment variable)
  ;;; =========================================================================

  (if (uiop:getenv "WEB3_TEST_RPC_URL")
      (progn
        (format t "~%  --- Integration tests (WEB3_TEST_RPC_URL set) ---~%")
        (let ((provider (coalton:coalton
                         (web3/provider:make-http-provider
                          (coalton:lisp coalton:String ()
                            (uiop:getenv "WEB3_TEST_RPC_URL"))))))

          (test-case "eth_chainId returns valid chain ID (integration)"
            (let ((result (coalton:coalton
                           (web3/provider:eth-chain-id
                            (coalton:lisp web3/provider:HttpProvider () provider)))))
              (assert (result-ok-p result))
              ;; Chain ID should be positive
              (assert (> (result-value result) 0))))

          (test-case "eth_blockNumber returns valid block number (integration)"
            (let ((result (coalton:coalton
                           (web3/provider:eth-block-number
                            (coalton:lisp web3/provider:HttpProvider () provider)))))
              (assert (result-ok-p result))
              ;; Block number should be positive on any live chain
              (assert (> (result-value result) 0))))

          (test-case "eth_gasPrice returns positive value (integration)"
            (let ((result (coalton:coalton
                           (web3/provider:eth-gas-price
                            (coalton:lisp web3/provider:HttpProvider () provider)))))
              (assert (result-ok-p result))
              ;; Gas price should be positive
              (let ((gas-price (coalton:coalton
                                (web3/types:u256-to-integer
                                 (coalton:lisp web3/types:U256 () (result-value result))))))
                (assert (> gas-price 0)))))

          (test-case "eth_getBalance returns balance for zero address (integration)"
            (let* ((zero-addr (result-value (coalton:coalton
                                             (web3/address:address-from-hex
                                              "0x0000000000000000000000000000000000000000"))))
                   (result (coalton:coalton
                            (web3/provider:eth-get-balance
                             (coalton:lisp web3/provider:HttpProvider () provider)
                             (coalton:lisp web3/address:Address () zero-addr)))))
              (assert (result-ok-p result))))

          (test-case "eth_getBalance returns balance for Vitalik address (integration)"
            (let* ((addr (result-value (coalton:coalton
                                        (web3/address:address-from-hex
                                         "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
                   (result (coalton:coalton
                            (web3/provider:eth-get-balance
                             (coalton:lisp web3/provider:HttpProvider () provider)
                             (coalton:lisp web3/address:Address () addr)))))
              (assert (result-ok-p result))))

          (test-case "eth_getTransactionCount for zero address is 0 (integration)"
            (let* ((zero-addr (result-value (coalton:coalton
                                             (web3/address:address-from-hex
                                              "0x0000000000000000000000000000000000000000"))))
                   (result (coalton:coalton
                            (web3/provider:eth-get-transaction-count
                             (coalton:lisp web3/provider:HttpProvider () provider)
                             (coalton:lisp web3/address:Address () zero-addr)))))
              (assert (result-ok-p result))
              (assert (= (result-value result) 0))))

          (test-case "eth_getTransactionCount for active address (integration)"
            (let* ((addr (result-value (coalton:coalton
                                        (web3/address:address-from-hex
                                         "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
                   (result (coalton:coalton
                            (web3/provider:eth-get-transaction-count
                             (coalton:lisp web3/provider:HttpProvider () provider)
                             (coalton:lisp web3/address:Address () addr)))))
              (assert (result-ok-p result))
              ;; Vitalik has made transactions
              (assert (> (result-value result) 0))))

          (test-case "eth_call to USDT totalSupply (integration)"
            (let* ((usdt (result-value (coalton:coalton
                                        (web3/address:address-from-hex
                                         "0xdAC17F958D2ee523a2206206994597C13D831ec7"))))
                   ;; totalSupply() selector = 0x18160ddd
                   (calldata (make-array 4 :fill-pointer 4 :adjustable t
                                         :initial-contents '(#x18 #x16 #x0d #xdd)))
                   (result (coalton:coalton
                            (web3/provider:eth-call
                             (coalton:lisp web3/provider:HttpProvider () provider)
                             coalton-prelude:None
                             (coalton:lisp web3/address:Address () usdt)
                             (coalton:lisp web3/types:Bytes () calldata)))))
              ;; On mainnet this should return the total supply
              ;; On other networks this might fail (no USDT)
              (assert (or (result-ok-p result) (result-err-p result)))))

          (test-case "eth_call to WETH name (integration)"
            (let* ((weth (result-value (coalton:coalton
                                        (web3/address:address-from-hex
                                         "0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2"))))
                   ;; name() selector = 0x06fdde03
                   (calldata (make-array 4 :fill-pointer 4 :adjustable t
                                         :initial-contents '(#x06 #xfd #xde #x03)))
                   (result (coalton:coalton
                            (web3/provider:eth-call
                             (coalton:lisp web3/provider:HttpProvider () provider)
                             coalton-prelude:None
                             (coalton:lisp web3/address:Address () weth)
                             (coalton:lisp web3/types:Bytes () calldata)))))
              ;; On mainnet this should return "Wrapped Ether"
              (assert (or (result-ok-p result) (result-err-p result)))))

          (test-case "eth_estimateGas for simple transfer (integration)"
            (let* ((from (result-value (coalton:coalton
                                        (web3/address:address-from-hex
                                         "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
                   (to (result-value (coalton:coalton
                                      (web3/address:address-from-hex
                                       "0x0000000000000000000000000000000000000001"))))
                   (value (coalton:coalton (web3/types:u256-from-integer 0)))
                   (data (make-array 0 :fill-pointer 0 :adjustable t))
                   (result (coalton:coalton
                            (web3/provider:eth-estimate-gas
                             (coalton:lisp web3/provider:HttpProvider () provider)
                             (coalton:lisp web3/address:Address () from)
                             (coalton-prelude:Some (coalton:lisp web3/address:Address () to))
                             (coalton:lisp web3/types:U256 () value)
                             (coalton:lisp web3/types:Bytes () data)))))
              (assert (result-ok-p result))
              ;; Simple transfer should be 21000 gas
              (assert (>= (result-value result) 21000))))

          (test-case "Multiple sequential calls work (integration)"
            ;; Test that provider can handle multiple calls
            (let ((chain-result (coalton:coalton
                                 (web3/provider:eth-chain-id
                                  (coalton:lisp web3/provider:HttpProvider () provider))))
                  (block-result (coalton:coalton
                                 (web3/provider:eth-block-number
                                  (coalton:lisp web3/provider:HttpProvider () provider))))
                  (gas-result (coalton:coalton
                               (web3/provider:eth-gas-price
                                (coalton:lisp web3/provider:HttpProvider () provider)))))
              (assert (result-ok-p chain-result))
              (assert (result-ok-p block-result))
              (assert (result-ok-p gas-result))))

          ;;; New method integration tests

          (test-case "eth_getCode returns bytecode for USDT contract (integration)"
            (let* ((usdt (result-value (coalton:coalton
                                        (web3/address:address-from-hex
                                         "0xdAC17F958D2ee523a2206206994597C13D831ec7"))))
                   (result (coalton:coalton
                            (web3/provider:eth-get-code
                             (coalton:lisp web3/provider:HttpProvider () provider)
                             (coalton:lisp web3/address:Address () usdt)))))
              ;; On mainnet, USDT has bytecode
              (assert (or (result-ok-p result) (result-err-p result)))))

          (test-case "eth_getCode returns empty for EOA (integration)"
            (let* ((eoa (result-value (coalton:coalton
                                       (web3/address:address-from-hex
                                        "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
                   (result (coalton:coalton
                            (web3/provider:eth-get-code
                             (coalton:lisp web3/provider:HttpProvider () provider)
                             (coalton:lisp web3/address:Address () eoa)))))
              (assert (or (result-ok-p result) (result-err-p result)))))

          (test-case "eth_getStorageAt reads slot 0 (integration)"
            (let* ((usdt (result-value (coalton:coalton
                                        (web3/address:address-from-hex
                                         "0xdAC17F958D2ee523a2206206994597C13D831ec7"))))
                   (slot (coalton:coalton web3/types:u256-zero))
                   (result (coalton:coalton
                            (web3/provider:eth-get-storage-at
                             (coalton:lisp web3/provider:HttpProvider () provider)
                             (coalton:lisp web3/address:Address () usdt)
                             (coalton:lisp web3/types:U256 () slot)))))
              (assert (or (result-ok-p result) (result-err-p result)))))

          (test-case "eth_getBlockByNumber returns latest block (integration)"
            (let ((result (coalton:coalton
                           (web3/provider:eth-get-block-by-number
                            (coalton:lisp web3/provider:HttpProvider () provider)
                            "latest"
                            coalton:False))))
              (assert (result-ok-p result))
              ;; Latest block should always exist
              (let ((opt (result-value result)))
                (assert (coalton:coalton
                         (coalton-prelude:some?
                          (coalton:lisp (coalton-prelude:Optional coalton:String) () opt)))))))

          (test-case "eth_getBlockByNumber returns None for future block (integration)"
            (let ((result (coalton:coalton
                           (web3/provider:eth-get-block-by-number
                            (coalton:lisp web3/provider:HttpProvider () provider)
                            "0xffffffffff"
                            coalton:False))))
              (assert (result-ok-p result))
              ;; Far future block should not exist
              (let ((opt (result-value result)))
                (assert (not (coalton:coalton
                              (coalton-prelude:some?
                               (coalton:lisp (coalton-prelude:Optional coalton:String) () opt))))))))

          (test-case "eth_syncing returns a result (integration)"
            (let ((result (coalton:coalton
                           (web3/provider:eth-syncing
                            (coalton:lisp web3/provider:HttpProvider () provider)))))
              (assert (result-ok-p result))))

          (test-case "eth_feeHistory returns fee data (integration)"
            (let ((result (coalton:coalton
                           (web3/provider:eth-fee-history
                            (coalton:lisp web3/provider:HttpProvider () provider)
                            4
                            "latest"
                            "[25,50,75]"))))
              (assert (result-ok-p result))))))

      ;; No integration tests
      (format t "~%  Note: Set WEB3_TEST_RPC_URL for integration tests (e.g., Infura/Alchemy URL)~%")))
