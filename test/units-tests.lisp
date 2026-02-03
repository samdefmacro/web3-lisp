;;; Units module tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Units Tests
;;; =========================================================================

(defun run-units-tests ()
  (format t "~%=== Units Tests ===~%")

  ;;; =========================================================================
  ;;; parse-units Tests
  ;;; =========================================================================

  (test-case "parse-units integer value"
    (let ((result (coalton:coalton (web3/units:parse-units "1" 18))))
      (assert (result-ok-p result))
      (let ((val (coalton:coalton
                  (web3/types:u256-to-integer
                   (coalton:lisp web3/types:U256 () (result-value result))))))
        (assert (= val 1000000000000000000)))))

  (test-case "parse-units decimal value"
    (let ((result (coalton:coalton (web3/units:parse-units "1.5" 18))))
      (assert (result-ok-p result))
      (let ((val (coalton:coalton
                  (web3/types:u256-to-integer
                   (coalton:lisp web3/types:U256 () (result-value result))))))
        (assert (= val 1500000000000000000)))))

  (test-case "parse-units small decimal"
    (let ((result (coalton:coalton (web3/units:parse-units "0.1" 18))))
      (assert (result-ok-p result))
      (let ((val (coalton:coalton
                  (web3/types:u256-to-integer
                   (coalton:lisp web3/types:U256 () (result-value result))))))
        (assert (= val 100000000000000000)))))

  (test-case "parse-units zero"
    (let ((result (coalton:coalton (web3/units:parse-units "0" 18))))
      (assert (result-ok-p result))
      (let ((val (coalton:coalton
                  (web3/types:u256-to-integer
                   (coalton:lisp web3/types:U256 () (result-value result))))))
        (assert (= val 0)))))

  (test-case "parse-units with different decimals"
    (let ((result (coalton:coalton (web3/units:parse-units "1.5" 6))))
      (assert (result-ok-p result))
      (let ((val (coalton:coalton
                  (web3/types:u256-to-integer
                   (coalton:lisp web3/types:U256 () (result-value result))))))
        (assert (= val 1500000)))))

  (test-case "parse-units truncates extra decimals"
    ;; 1.123456789 with 6 decimals should become 1123456
    (let ((result (coalton:coalton (web3/units:parse-units "1.123456789" 6))))
      (assert (result-ok-p result))
      (let ((val (coalton:coalton
                  (web3/types:u256-to-integer
                   (coalton:lisp web3/types:U256 () (result-value result))))))
        (assert (= val 1123456)))))

  (test-case "parse-units rejects negative"
    (let ((result (coalton:coalton (web3/units:parse-units "-1" 18))))
      (assert (result-err-p result))))

  (test-case "parse-units rejects empty string"
    (let ((result (coalton:coalton (web3/units:parse-units "" 18))))
      (assert (result-err-p result))))

  ;;; =========================================================================
  ;;; format-units Tests
  ;;; =========================================================================

  (test-case "format-units integer result"
    (let ((value (coalton:coalton (web3/types:u256-from-integer 1000000000000000000))))
      (let ((str (coalton:coalton
                  (web3/units:format-units
                   (coalton:lisp web3/types:U256 () value) 18))))
        (assert (string= str "1.0")))))

  (test-case "format-units decimal result"
    (let ((value (coalton:coalton (web3/types:u256-from-integer 1500000000000000000))))
      (let ((str (coalton:coalton
                  (web3/units:format-units
                   (coalton:lisp web3/types:U256 () value) 18))))
        (assert (string= str "1.5")))))

  (test-case "format-units zero"
    (let ((value (coalton:coalton (web3/types:u256-from-integer 0))))
      (let ((str (coalton:coalton
                  (web3/units:format-units
                   (coalton:lisp web3/types:U256 () value) 18))))
        (assert (string= str "0.0")))))

  (test-case "format-units trims trailing zeros"
    (let ((value (coalton:coalton (web3/types:u256-from-integer 1230000000000000000))))
      (let ((str (coalton:coalton
                  (web3/units:format-units
                   (coalton:lisp web3/types:U256 () value) 18))))
        (assert (string= str "1.23")))))

  ;;; =========================================================================
  ;;; Ether Convenience Tests
  ;;; =========================================================================

  (test-case "parse-ether basic"
    (let ((result (coalton:coalton (web3/units:parse-ether "1.0"))))
      (assert (result-ok-p result))
      (let ((val (coalton:coalton
                  (web3/types:u256-to-integer
                   (coalton:lisp web3/types:U256 () (result-value result))))))
        (assert (= val 1000000000000000000)))))

  (test-case "format-ether basic"
    (let ((value (coalton:coalton (web3/types:u256-from-integer 1000000000000000000))))
      (let ((str (coalton:coalton
                  (web3/units:format-ether
                   (coalton:lisp web3/types:U256 () value)))))
        (assert (string= str "1.0")))))

  (test-case "parse-ether and format-ether roundtrip"
    (let ((result (coalton:coalton (web3/units:parse-ether "123.456"))))
      (assert (result-ok-p result))
      (let ((formatted (coalton:coalton
                        (web3/units:format-ether
                         (coalton:lisp web3/types:U256 () (result-value result))))))
        (assert (string= formatted "123.456")))))

  ;;; =========================================================================
  ;;; Gwei Convenience Tests
  ;;; =========================================================================

  (test-case "parse-gwei basic"
    (let ((result (coalton:coalton (web3/units:parse-gwei "1.0"))))
      (assert (result-ok-p result))
      (let ((val (coalton:coalton
                  (web3/types:u256-to-integer
                   (coalton:lisp web3/types:U256 () (result-value result))))))
        (assert (= val 1000000000)))))

  (test-case "format-gwei basic"
    (let ((value (coalton:coalton (web3/types:u256-from-integer 1500000000))))
      (let ((str (coalton:coalton
                  (web3/units:format-gwei
                   (coalton:lisp web3/types:U256 () value)))))
        (assert (string= str "1.5")))))

  (test-case "parse-gwei and format-gwei roundtrip"
    (let ((result (coalton:coalton (web3/units:parse-gwei "50.5"))))
      (assert (result-ok-p result))
      (let ((formatted (coalton:coalton
                        (web3/units:format-gwei
                         (coalton:lisp web3/types:U256 () (result-value result))))))
        (assert (string= formatted "50.5"))))))
