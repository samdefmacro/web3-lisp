;;; Types module tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Hex Encoding/Decoding Tests
;;; =========================================================================

(defun run-hex-tests ()
  (format t "~%=== Hex Encoding/Decoding Tests ===~%")

  (test-case "hex-encode empty bytes"
    (assert (eq (web3-tests:test-hex-encode-empty coalton:Unit) coalton:True)))

  (test-case "hex-encode deadbeef"
    (assert (eq (web3-tests:test-hex-encode-bytes coalton:Unit) coalton:True)))

  (test-case "hex-decode roundtrip"
    (assert (eq (web3-tests:test-hex-decode-roundtrip coalton:Unit) coalton:True)))

  (test-case "hex-decode with 0x prefix"
    (assert (eq (web3-tests:test-hex-decode-prefixed coalton:Unit) coalton:True)))

  (test-case "hex-decode 0xdeadbeef (direct CL)"
    (let ((result (coalton:coalton (web3/types:hex-decode "0xdeadbeef"))))
      (assert (result-ok-p result))
      (let ((bytes (result-value result)))
        (assert (= (length bytes) 4))
        (assert (= (aref bytes 0) #xde))
        (assert (= (aref bytes 1) #xad))
        (assert (= (aref bytes 2) #xbe))
        (assert (= (aref bytes 3) #xef)))))

  (test-case "hex-encode-prefixed"
    (let ((result (coalton:coalton
                   (web3/types:hex-encode-prefixed
                    (web3/types:bytes-from-list
                     (coalton:Cons 255 coalton:Nil))))))
      (assert (string= result "0xff")))))

;;; =========================================================================
;;; U256 Tests
;;; =========================================================================

(defun run-u256-tests ()
  (format t "~%=== U256 Tests ===~%")

  (test-case "U256 zero"
    (assert (eq (web3-tests:test-u256-zero coalton:Unit) coalton:True)))

  (test-case "U256 from-integer"
    (assert (eq (web3-tests:test-u256-from-integer coalton:Unit) coalton:True)))

  (test-case "U256 addition"
    (assert (eq (web3-tests:test-u256-add coalton:Unit) coalton:True)))

  (test-case "U256 subtraction"
    (assert (eq (web3-tests:test-u256-sub coalton:Unit) coalton:True)))

  (test-case "U256 multiplication"
    (assert (eq (web3-tests:test-u256-mul coalton:Unit) coalton:True)))

  (test-case "U256 to-bytes roundtrip"
    (assert (eq (web3-tests:test-u256-to-bytes-roundtrip coalton:Unit) coalton:True)))

  (test-case "U256 comparison"
    (assert (eq (web3-tests:test-u256-comparison coalton:Unit) coalton:True)))

  (test-case "U256 large value"
    ;; Test with 2^64 (a value that doesn't fit in U64)
    (let* ((back (coalton:coalton
                  (web3/types:u256-to-integer
                   (web3/types:u256-from-integer 18446744073709551616)))))
      (assert (= back 18446744073709551616)))))

;;; =========================================================================
;;; Bytes Utility Tests
;;; =========================================================================

(defun run-bytes-tests ()
  (format t "~%=== Bytes Utility Tests ===~%")

  (test-case "bytes-pad-left"
    (assert (eq (web3-tests:test-bytes-pad-left coalton:Unit) coalton:True)))

  (test-case "bytes-pad-right"
    (assert (eq (web3-tests:test-bytes-pad-right coalton:Unit) coalton:True)))

  (test-case "bytes-equal"
    (assert (eq (web3-tests:test-bytes-equal coalton:Unit) coalton:True)))

  (test-case "bytes-slice"
    (let ((result (coalton:coalton
                   (web3/types:bytes-slice 1 2
                    (web3/types:bytes-from-list
                     (coalton:Cons 10 (coalton:Cons 20 (coalton:Cons 30 coalton:Nil))))))))
      (assert (= (length result) 2))
      (assert (= (aref result 0) 20))
      (assert (= (aref result 1) 30))))

  (test-case "bytes-append"
    (let ((result (coalton:coalton
                   (web3/types:bytes-append
                    (web3/types:bytes-from-list (coalton:Cons 1 (coalton:Cons 2 coalton:Nil)))
                    (web3/types:bytes-from-list (coalton:Cons 3 (coalton:Cons 4 coalton:Nil)))))))
      (assert (= (length result) 4))
      (assert (= (aref result 0) 1))
      (assert (= (aref result 3) 4))))

  (test-case "bytes-reverse"
    (let ((result (coalton:coalton
                   (web3/types:bytes-reverse
                    (web3/types:bytes-from-list
                     (coalton:Cons 1 (coalton:Cons 2 (coalton:Cons 3 coalton:Nil))))))))
      (assert (= (aref result 0) 3))
      (assert (= (aref result 1) 2))
      (assert (= (aref result 2) 1)))))

;;; =========================================================================
;;; Unit Conversion Tests
;;; =========================================================================

(defun run-unit-conversion-tests ()
  (format t "~%=== Unit Conversion Tests ===~%")

  (test-case "ether-to-wei conversion"
    (assert (eq (web3-tests:test-ether-wei-conversion coalton:Unit) coalton:True)))

  (test-case "wei-to-ether-string 0"
    (let ((result (coalton:coalton
                   (web3/types:wei-to-ether-string web3/types:u256-zero))))
      (assert (string= result "0.0"))))

  (test-case "gwei-to-wei conversion"
    (assert (= (coalton:coalton (web3/types:u256-to-integer
                                 (web3/types:gwei-to-wei (web3/types:u256-from-integer 1))))
               1000000000))))
