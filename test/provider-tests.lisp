;;; Provider module tests - Pure Common Lisp
;;; Note: These are unit tests only. Integration tests require a running node.

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Provider Tests (unit tests, no network required)
;;; =========================================================================

(defun run-provider-tests ()
  (format t "~%=== Provider Tests ===~%")

  (test-case "HttpProvider creation"
    ;; Just verify provider can be created without error
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider "http://localhost:8545"))))
      (assert (not (null provider)))))

  ;; Integration tests guarded by environment variable
  (when (uiop:getenv "WEB3_TEST_RPC_URL")
    (format t "~%  --- Integration tests (WEB3_TEST_RPC_URL set) ---~%")
    (let ((provider (coalton:coalton
                     (web3/provider:make-http-provider
                      (coalton:lisp coalton:String ()
                        (uiop:getenv "WEB3_TEST_RPC_URL"))))))

      (test-case "eth_chainId (integration)"
        (let ((result (coalton:coalton
                       (web3/provider:eth-chain-id
                        (coalton:lisp web3/provider:HttpProvider () provider)))))
          (assert (result-ok-p result))))

      (test-case "eth_blockNumber (integration)"
        (let ((result (coalton:coalton
                       (web3/provider:eth-block-number
                        (coalton:lisp web3/provider:HttpProvider () provider)))))
          (assert (result-ok-p result))))

      (test-case "eth_gasPrice (integration)"
        (let ((result (coalton:coalton
                       (web3/provider:eth-gas-price
                        (coalton:lisp web3/provider:HttpProvider () provider)))))
          (assert (result-ok-p result)))))))
