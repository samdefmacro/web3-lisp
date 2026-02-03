;;; Transaction Simulation and Estimation tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Transaction Simulation Tests
;;; =========================================================================

(defun run-simulate-tests ()
  (format t "~%=== Simulation & Estimation Tests ===~%")

  ;;; =========================================================================
  ;;; BlockSpec Tests
  ;;; =========================================================================

  (test-case "BlockLatest to string"
    (let ((str (coalton:coalton
                (web3/simulate:block-spec-to-string
                 web3/simulate:BlockLatest))))
      (assert (string= str "\"latest\""))))

  (test-case "BlockPending to string"
    (let ((str (coalton:coalton
                (web3/simulate:block-spec-to-string
                 web3/simulate:BlockPending))))
      (assert (string= str "\"pending\""))))

  (test-case "BlockEarliest to string"
    (let ((str (coalton:coalton
                (web3/simulate:block-spec-to-string
                 web3/simulate:BlockEarliest))))
      (assert (string= str "\"earliest\""))))

  (test-case "BlockNumber to string"
    (let ((str (coalton:coalton
                (web3/simulate:block-spec-to-string
                 (web3/simulate:BlockNumber 100)))))
      (assert (string= str "\"0x64\""))))

  ;;; =========================================================================
  ;;; CallOptions Tests
  ;;; =========================================================================

  (test-case "default-call-options creates valid options"
    (let ((opts (coalton:coalton
                 (web3/simulate:default-call-options coalton:Unit))))
      (assert opts)))

  (test-case "make-call-options creates custom options"
    (let* ((opts (coalton:coalton
                  (web3/simulate:make-call-options
                   web3/simulate:BlockPending
                   coalton-prelude:None
                   (coalton-prelude:Some 100000)
                   coalton-prelude:None
                   coalton-prelude:None)))
           (block-spec (coalton:coalton
                        (web3/simulate:.call-options-block
                         (coalton:lisp web3/simulate:CallOptions () opts)))))
      (assert opts)
      (assert block-spec)))

  (test-case "call-options-gas-limit accessor works"
    (let* ((opts (coalton:coalton
                  (web3/simulate:make-call-options
                   web3/simulate:BlockLatest
                   coalton-prelude:None
                   (coalton-prelude:Some 50000)
                   coalton-prelude:None
                   coalton-prelude:None)))
           (gas-opt (coalton:coalton
                     (web3/simulate:.call-options-gas-limit
                      (coalton:lisp web3/simulate:CallOptions () opts)))))
      ;; gas-opt should be Some 50000
      (assert (typep gas-opt 'coalton-library/classes::optional/some))))

  ;;; =========================================================================
  ;;; SimulationResult Tests
  ;;; =========================================================================

  (test-case "make-simulation-result creates result"
    (let* ((data (make-array 4 :fill-pointer 4 :adjustable t
                              :initial-contents '(1 2 3 4)))
           (result (coalton:coalton
                    (web3/simulate:make-simulation-result
                     (coalton:lisp web3/types:Bytes () data)
                     21000
                     coalton:True))))
      (assert result)))

  (test-case "simulation-return-data accessor works"
    (let* ((data (make-array 4 :fill-pointer 4 :adjustable t
                              :initial-contents '(1 2 3 4)))
           (result (coalton:coalton
                    (web3/simulate:make-simulation-result
                     (coalton:lisp web3/types:Bytes () data)
                     21000
                     coalton:True)))
           (return-data (coalton:coalton
                         (web3/simulate:.simulation-return-data
                          (coalton:lisp web3/simulate:SimulationResult () result)))))
      (assert (= (length return-data) 4))))

  (test-case "simulation-gas-used accessor works"
    (let* ((data (make-array 0 :fill-pointer 0 :adjustable t))
           (result (coalton:coalton
                    (web3/simulate:make-simulation-result
                     (coalton:lisp web3/types:Bytes () data)
                     55000
                     coalton:True)))
           (gas-used (coalton:coalton
                      (web3/simulate:.simulation-gas-used
                       (coalton:lisp web3/simulate:SimulationResult () result)))))
      (assert (= gas-used 55000))))

  (test-case "simulation-success accessor works"
    (let* ((data (make-array 0 :fill-pointer 0 :adjustable t))
           (result (coalton:coalton
                    (web3/simulate:make-simulation-result
                     (coalton:lisp web3/types:Bytes () data)
                     21000
                     coalton:False)))
           (success (coalton:coalton
                     (web3/simulate:.simulation-success
                      (coalton:lisp web3/simulate:SimulationResult () result)))))
      (assert (eq success coalton:False))))

  ;;; =========================================================================
  ;;; GasEstimate Tests
  ;;; =========================================================================

  (test-case "make-gas-estimate creates estimate"
    (let* ((gas-price (coalton:coalton (web3/types:u256-from-integer 20000000000)))
           (max-fee (coalton:coalton (web3/types:u256-from-integer 40000000000)))
           (priority (coalton:coalton (web3/types:u256-from-integer 1500000000)))
           (total (coalton:coalton (web3/types:u256-from-integer 840000000000000)))
           (estimate (coalton:coalton
                      (web3/simulate:make-gas-estimate
                       21000
                       (coalton:lisp web3/types:U256 () gas-price)
                       (coalton:lisp web3/types:U256 () max-fee)
                       (coalton:lisp web3/types:U256 () priority)
                       (coalton:lisp web3/types:U256 () total)))))
      (assert estimate)))

  (test-case "gas-estimate-gas-limit accessor works"
    (let* ((gas-price (coalton:coalton (web3/types:u256-from-integer 20000000000)))
           (estimate (coalton:coalton
                      (web3/simulate:make-gas-estimate
                       65000
                       (coalton:lisp web3/types:U256 () gas-price)
                       (coalton:lisp web3/types:U256 () gas-price)
                       (web3/types:u256-zero coalton:Unit)
                       (coalton:lisp web3/types:U256 () gas-price))))
           (gas-limit (coalton:coalton
                       (web3/simulate:.gas-estimate-gas-limit
                        (coalton:lisp web3/simulate:GasEstimate () estimate)))))
      (assert (= gas-limit 65000))))

  (test-case "gas-estimate-total-cost accessor works"
    (let* ((total (coalton:coalton (web3/types:u256-from-integer 1000000000000000)))
           (estimate (coalton:coalton
                      (web3/simulate:make-gas-estimate
                       21000
                       (web3/types:u256-zero coalton:Unit)
                       (web3/types:u256-zero coalton:Unit)
                       (web3/types:u256-zero coalton:Unit)
                       (coalton:lisp web3/types:U256 () total))))
           (cost (coalton:coalton
                  (web3/simulate:.gas-estimate-total-cost
                   (coalton:lisp web3/simulate:GasEstimate () estimate))))
           (cost-int (coalton:coalton
                      (web3/types:u256-to-integer
                       (coalton:lisp web3/types:U256 () cost)))))
      (assert (= cost-int 1000000000000000))))

  ;;; =========================================================================
  ;;; AccessListResult Tests
  ;;; =========================================================================

  (test-case "make-access-list-result creates result"
    (let ((result (coalton:coalton
                   (web3/simulate:make-access-list-result
                    coalton:Nil
                    21000))))
      (assert result)))

  (test-case "access-list-result-gas-used accessor works"
    (let* ((result (coalton:coalton
                    (web3/simulate:make-access-list-result
                     coalton:Nil
                     45000)))
           (gas (coalton:coalton
                 (web3/simulate:.access-list-result-gas-used
                  (coalton:lisp web3/simulate:AccessListResult () result)))))
      (assert (= gas 45000))))

  (test-case "access-list-result-list accessor works"
    (let* ((result (coalton:coalton
                    (web3/simulate:make-access-list-result
                     coalton:Nil
                     21000)))
           (al (coalton:coalton
                (web3/simulate:.access-list-result-list
                 (coalton:lisp web3/simulate:AccessListResult () result))))
           (len (coalton:coalton
                 (coalton-library/list:length
                  (coalton:lisp web3/transaction:AccessList () al)))))
      (assert (= len 0))))

  ;;; =========================================================================
  ;;; Network-dependent tests note
  ;;; =========================================================================

  (test-case "Note: Network tests require running Ethereum node"
    ;; These functions require a provider:
    ;; - simulate-call
    ;; - simulate-transaction
    ;; - estimate-gas
    ;; - estimate-transaction-gas
    ;; - estimate-transaction-cost
    ;; - create-access-list
    ;; - populate-transaction
    (assert t)))
