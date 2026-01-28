;;; Wallet module tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Wallet Tests
;;; =========================================================================

(defun run-wallet-tests ()
  (format t "~%=== Wallet Tests ===~%")

  (test-case "Wallet creation"
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (wallet (coalton:coalton
                    (web3/wallet:make-wallet
                     (coalton:lisp web3/types:Bytes () pk)))))
      (assert (not (null wallet)))))

  (test-case "Wallet with provider"
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (wallet (coalton:coalton
                    (web3/wallet:wallet-with-provider
                     (coalton:lisp web3/types:Bytes () pk)
                     (web3/provider:make-http-provider "http://localhost:8545")))))
      (assert (not (null wallet)))))

  (test-case "Wallet send without provider returns error"
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (wallet (coalton:coalton
                    (web3/wallet:make-wallet
                     (coalton:lisp web3/types:Bytes () pk))))
           (tx (coalton:coalton
                (web3/transaction:make-transaction
                 web3/transaction:LegacyTx
                 1 0
                 (web3/types:u256-from-integer 20000000000)
                 (web3/types:u256-zero coalton:Unit)
                 21000
                 coalton-prelude:None
                 (web3/types:u256-zero coalton:Unit)
                 (web3/types:bytes-empty coalton:Unit)
                 coalton:Nil)))
           (result (coalton:coalton
                    (web3/wallet:wallet-send-transaction
                     (coalton:lisp web3/wallet:Wallet () wallet)
                     (coalton:lisp web3/transaction:Transaction () tx)))))
      ;; Should fail because no provider
      (assert (result-err-p result)))))

;;; =========================================================================
;;; Main Test Runner
;;; =========================================================================

(defun run-all-tests ()
  "Run all web3-coalton tests"
  (setf *tests-passed* 0)
  (setf *tests-failed* 0)
  (format t "~%========================================~%")
  (format t "Running web3-coalton Tests~%")
  (format t "========================================~%")

  ;; Phase 1: Types + RLP + Crypto
  (run-hex-tests)
  (run-u256-tests)
  (run-bytes-tests)
  (run-unit-conversion-tests)
  (run-rlp-tests)
  (run-crypto-tests)

  ;; Phase 2: Address
  (run-address-tests)

  ;; Phase 3: ABI
  (run-abi-tests)

  ;; Phase 4: Transaction
  (run-transaction-tests)

  ;; Phase 5: Provider + Wallet
  (run-provider-tests)
  (run-wallet-tests)

  (format t "~%========================================~%")
  (format t "Results: ~A passed, ~A failed~%" *tests-passed* *tests-failed*)
  (format t "========================================~%")
  (values *tests-passed* *tests-failed*))
