;;; Multicall tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Multicall Selector Tests
;;; =========================================================================

(defun run-multicall-tests ()
  (format t "~%=== Multicall Tests ===~%")

  ;; Test function selectors
  (test-case "aggregate selector = 0x252dba42"
    (let ((selector (coalton:coalton (web3/multicall:aggregate-selector coalton:Unit))))
      (assert (= (length selector) 4))
      (assert (= (aref selector 0) #x25))
      (assert (= (aref selector 1) #x2d))
      (assert (= (aref selector 2) #xba))
      (assert (= (aref selector 3) #x42))))

  (test-case "tryAggregate selector = 0xbce38bd7"
    (let ((selector (coalton:coalton (web3/multicall:try-aggregate-selector coalton:Unit))))
      (assert (= (length selector) 4))
      (assert (= (aref selector 0) #xbc))
      (assert (= (aref selector 1) #xe3))
      (assert (= (aref selector 2) #x8b))
      (assert (= (aref selector 3) #xd7))))

  (test-case "aggregate3 selector = 0x82ad56cb"
    (let ((selector (coalton:coalton (web3/multicall:aggregate3-selector coalton:Unit))))
      (assert (= (length selector) 4))
      (assert (= (aref selector 0) #x82))
      (assert (= (aref selector 1) #xad))
      (assert (= (aref selector 2) #x56))
      (assert (= (aref selector 3) #xcb))))

  ;;; =========================================================================
  ;;; Multicall3 Address Test
  ;;; =========================================================================

  (test-case "multicall3 address is correct"
    (let ((addr (coalton:coalton (web3/multicall:multicall3-address coalton:Unit))))
      (assert (string-equal addr "0xcA11bde05977b3631167028862bE2a173976CA11"))))

  ;;; =========================================================================
  ;;; Call Struct Tests
  ;;; =========================================================================

  (test-case "make-Call creates valid struct"
    (let* ((target (result-value (coalton:coalton (web3/types:hex-decode "d8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (calldata (result-value (coalton:coalton (web3/types:hex-decode "70a08231"))))
           (call (coalton:coalton
                  (web3/multicall:Call
                   (coalton:lisp web3/types:Bytes () target)
                   (coalton:lisp web3/types:Bytes () calldata)))))
      (declare (ignore call))
      (assert t)))

  (test-case "Call3 creates valid struct with allow-failure"
    (let* ((target (result-value (coalton:coalton (web3/types:hex-decode "d8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (calldata (result-value (coalton:coalton (web3/types:hex-decode "70a08231"))))
           (call (coalton:coalton
                  (web3/multicall:Call3
                   (coalton:lisp web3/types:Bytes () target)
                   coalton:True
                   (coalton:lisp web3/types:Bytes () calldata)))))
      (declare (ignore call))
      (assert t)))

  ;;; =========================================================================
  ;;; Calldata Builder Tests
  ;;; =========================================================================

  (test-case "aggregate-calldata builds valid calldata"
    (let* ((target (result-value (coalton:coalton (web3/types:hex-decode "dac17f958d2ee523a2206206994597c13d831ec7"))))
           (calldata (result-value (coalton:coalton (web3/types:hex-decode "70a0823100000000000000000000000047ac0fb4f2d84898e4d9e7b4dab3c24507a6d503"))))
           (call (coalton:coalton
                  (web3/multicall:Call
                   (coalton:lisp web3/types:Bytes () target)
                   (coalton:lisp web3/types:Bytes () calldata))))
           (result (coalton:coalton
                    (web3/multicall:aggregate-calldata
                     (coalton:Cons (coalton:lisp web3/multicall:Call () call) coalton:Nil)))))
      ;; Should start with aggregate selector
      (assert (= (aref result 0) #x25))
      (assert (= (aref result 1) #x2d))
      (assert (= (aref result 2) #xba))
      (assert (= (aref result 3) #x42))
      ;; Should be longer than just the selector
      (assert (> (length result) 100))))

  (test-case "try-aggregate-calldata builds valid calldata"
    (let* ((target (result-value (coalton:coalton (web3/types:hex-decode "dac17f958d2ee523a2206206994597c13d831ec7"))))
           (calldata (result-value (coalton:coalton (web3/types:hex-decode "70a08231"))))
           (call (coalton:coalton
                  (web3/multicall:Call
                   (coalton:lisp web3/types:Bytes () target)
                   (coalton:lisp web3/types:Bytes () calldata))))
           (result (coalton:coalton
                    (web3/multicall:try-aggregate-calldata
                     coalton:False
                     (coalton:Cons (coalton:lisp web3/multicall:Call () call) coalton:Nil)))))
      ;; Should start with tryAggregate selector
      (assert (= (aref result 0) #xbc))
      (assert (= (aref result 1) #xe3))
      (assert (= (aref result 2) #x8b))
      (assert (= (aref result 3) #xd7))))

  (test-case "aggregate3-calldata builds valid calldata"
    (let* ((target (result-value (coalton:coalton (web3/types:hex-decode "dac17f958d2ee523a2206206994597c13d831ec7"))))
           (calldata (result-value (coalton:coalton (web3/types:hex-decode "70a08231"))))
           (call (coalton:coalton
                  (web3/multicall:Call3
                   (coalton:lisp web3/types:Bytes () target)
                   coalton:True
                   (coalton:lisp web3/types:Bytes () calldata))))
           (result (coalton:coalton
                    (web3/multicall:aggregate3-calldata
                     (coalton:Cons (coalton:lisp web3/multicall:Call3 () call) coalton:Nil)))))
      ;; Should start with aggregate3 selector
      (assert (= (aref result 0) #x82))
      (assert (= (aref result 1) #xad))
      (assert (= (aref result 2) #x56))
      (assert (= (aref result 3) #xcb))))

  ;;; =========================================================================
  ;;; High-Level Helper Tests
  ;;; =========================================================================

  (test-case "batch-calls creates aggregate calldata"
    (let* ((target1 (result-value (coalton:coalton (web3/types:hex-decode "dac17f958d2ee523a2206206994597c13d831ec7"))))
           (calldata1 (result-value (coalton:coalton (web3/types:hex-decode "70a08231"))))
           (target2 (result-value (coalton:coalton (web3/types:hex-decode "a0b86991c6218b36c1d19d4a2e9eb0ce3606eb48"))))
           (calldata2 (result-value (coalton:coalton (web3/types:hex-decode "18160ddd"))))
           (result (coalton:coalton
                    (web3/multicall:batch-calls
                     (coalton:Cons
                      (coalton-prelude:Tuple
                       (coalton:lisp web3/types:Bytes () target1)
                       (coalton:lisp web3/types:Bytes () calldata1))
                      (coalton:Cons
                       (coalton-prelude:Tuple
                        (coalton:lisp web3/types:Bytes () target2)
                        (coalton:lisp web3/types:Bytes () calldata2))
                       coalton:Nil))))))
      ;; Should start with aggregate selector
      (assert (= (aref result 0) #x25))
      (assert (= (aref result 1) #x2d))))

  (test-case "batch-calls-allow-failure creates aggregate3 calldata"
    (let* ((target1 (result-value (coalton:coalton (web3/types:hex-decode "dac17f958d2ee523a2206206994597c13d831ec7"))))
           (calldata1 (result-value (coalton:coalton (web3/types:hex-decode "70a08231"))))
           (result (coalton:coalton
                    (web3/multicall:batch-calls-allow-failure
                     (coalton:Cons
                      (coalton-prelude:Tuple
                       (coalton:lisp web3/types:Bytes () target1)
                       (coalton:lisp web3/types:Bytes () calldata1))
                      coalton:Nil)))))
      ;; Should start with aggregate3 selector
      (assert (= (aref result 0) #x82))
      (assert (= (aref result 1) #xad))))

  ;;; =========================================================================
  ;;; Multiple Calls Test
  ;;; =========================================================================

  (test-case "aggregate-calldata handles multiple calls"
    (let* ((target (result-value (coalton:coalton (web3/types:hex-decode "dac17f958d2ee523a2206206994597c13d831ec7"))))
           (call1-data (result-value (coalton:coalton (web3/types:hex-decode "70a08231"))))
           (call2-data (result-value (coalton:coalton (web3/types:hex-decode "18160ddd"))))
           (call3-data (result-value (coalton:coalton (web3/types:hex-decode "06fdde03"))))
           (call1 (coalton:coalton
                   (web3/multicall:Call
                    (coalton:lisp web3/types:Bytes () target)
                    (coalton:lisp web3/types:Bytes () call1-data))))
           (call2 (coalton:coalton
                   (web3/multicall:Call
                    (coalton:lisp web3/types:Bytes () target)
                    (coalton:lisp web3/types:Bytes () call2-data))))
           (call3 (coalton:coalton
                   (web3/multicall:Call
                    (coalton:lisp web3/types:Bytes () target)
                    (coalton:lisp web3/types:Bytes () call3-data))))
           (result (coalton:coalton
                    (web3/multicall:aggregate-calldata
                     (coalton:Cons (coalton:lisp web3/multicall:Call () call1)
                                   (coalton:Cons (coalton:lisp web3/multicall:Call () call2)
                                                 (coalton:Cons (coalton:lisp web3/multicall:Call () call3)
                                                               coalton:Nil)))))))
      ;; Should be significantly longer with 3 calls
      (assert (> (length result) 200)))))
