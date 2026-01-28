;;; RLP module tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; RLP Encoding Tests
;;; =========================================================================

(defun run-rlp-tests ()
  (format t "~%=== RLP Encoding/Decoding Tests ===~%")

  ;; Test vectors from the Ethereum wiki: https://ethereum.org/en/developers/docs/data-structures-and-encoding/rlp/

  (test-case "RLP single byte (0x42)"
    (assert (eq (web3-tests:test-rlp-single-byte coalton:Unit) coalton:True)))

  (test-case "RLP encode 'dog'"
    (assert (eq (web3-tests:test-rlp-short-string coalton:Unit) coalton:True)))

  (test-case "RLP encode empty string"
    (assert (eq (web3-tests:test-rlp-empty-string coalton:Unit) coalton:True)))

  (test-case "RLP encode empty list"
    (assert (eq (web3-tests:test-rlp-empty-list coalton:Unit) coalton:True)))

  (test-case "RLP encode integer 0"
    (assert (eq (web3-tests:test-rlp-integer-zero coalton:Unit) coalton:True)))

  (test-case "RLP encode integer 15"
    (assert (eq (web3-tests:test-rlp-integer-small coalton:Unit) coalton:True)))

  (test-case "RLP encode nested list [ [], [[]], [ [], [[]] ] ]"
    (assert (eq (web3-tests:test-rlp-nested-list coalton:Unit) coalton:True)))

  (test-case "RLP decode roundtrip"
    (assert (eq (web3-tests:test-rlp-decode-roundtrip coalton:Unit) coalton:True)))

  ;; Additional CL-level tests

  (test-case "RLP encode 'cat' (direct)"
    (let ((result (coalton:coalton (web3/rlp:rlp-encode-string "cat"))))
      (assert (= (length result) 4))
      (assert (= (aref result 0) #x83))
      (assert (= (aref result 1) (char-code #\c)))
      (assert (= (aref result 2) (char-code #\a)))
      (assert (= (aref result 3) (char-code #\t)))))

  (test-case "RLP encode integer 1024"
    (let ((result (coalton:coalton (web3/rlp:rlp-encode-integer 1024))))
      ;; 1024 = 0x0400, so RLP = [0x82, 0x04, 0x00]
      (assert (= (length result) 3))
      (assert (= (aref result 0) #x82))
      (assert (= (aref result 1) #x04))
      (assert (= (aref result 2) #x00))))

  (test-case "RLP encode set of three strings"
    ;; The set theoretical representation of three: [ [], [[]], [ [], [[]] ] ]
    ;; Expected: 0xc7c0c1c0c3c0c1c0
    (let ((result (coalton:coalton
                   (web3/rlp:rlp-encode
                    (web3/rlp:RlpList
                     (coalton:Cons (web3/rlp:RlpList coalton:Nil)
                                   (coalton:Cons (web3/rlp:RlpList
                                                  (coalton:Cons (web3/rlp:RlpList coalton:Nil)
                                                                coalton:Nil))
                                                 (coalton:Cons (web3/rlp:RlpList
                                                                (coalton:Cons (web3/rlp:RlpList coalton:Nil)
                                                                              (coalton:Cons (web3/rlp:RlpList
                                                                                             (coalton:Cons (web3/rlp:RlpList coalton:Nil)
                                                                                                           coalton:Nil))
                                                                                            coalton:Nil)))
                                                               coalton:Nil))))))))
      (assert (= (length result) 8))
      (assert (= (aref result 0) #xc7))
      (assert (= (aref result 1) #xc0))
      (assert (= (aref result 2) #xc1))
      (assert (= (aref result 3) #xc0))
      (assert (= (aref result 4) #xc3))
      (assert (= (aref result 5) #xc0))
      (assert (= (aref result 6) #xc1))
      (assert (= (aref result 7) #xc0))))

  (test-case "RLP encode byte string 0x80"
    ;; Single byte 0x80 -> [0x81, 0x80] (needs length prefix since >= 0x80)
    (let ((result (coalton:coalton
                   (web3/rlp:rlp-encode
                    (web3/rlp:RlpBytes
                     (web3/types:bytes-from-list
                      (coalton:Cons #x80 coalton:Nil)))))))
      (assert (= (length result) 2))
      (assert (= (aref result 0) #x81))
      (assert (= (aref result 1) #x80))))

  (test-case "RLP decode single byte"
    (let* ((encoded (make-array 1 :initial-element #x42 :fill-pointer 1 :adjustable t))
           (result (coalton:coalton
                    (web3/rlp:rlp-decode
                     (coalton:lisp web3/types:Bytes ()
                       encoded)))))
      (assert (result-ok-p result)))))
