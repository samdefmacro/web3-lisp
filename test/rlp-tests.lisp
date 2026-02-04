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
      (assert (result-ok-p result))))

  ;;; =========================================================================
  ;;; Additional RLP Encoding Tests
  ;;; =========================================================================

  (test-case "RLP encode integer 127 (single byte boundary)"
    ;; 127 fits in a single byte, RLP = [0x7f]
    (let ((result (coalton:coalton (web3/rlp:rlp-encode-integer 127))))
      (assert (= (length result) 1))
      (assert (= (aref result 0) #x7f))))

  (test-case "RLP encode integer 128 (needs length prefix)"
    ;; 128 = 0x80, RLP = [0x81, 0x80]
    (let ((result (coalton:coalton (web3/rlp:rlp-encode-integer 128))))
      (assert (= (length result) 2))
      (assert (= (aref result 0) #x81))
      (assert (= (aref result 1) #x80))))

  (test-case "RLP encode integer 255"
    ;; 255 = 0xff, RLP = [0x81, 0xff]
    (let ((result (coalton:coalton (web3/rlp:rlp-encode-integer 255))))
      (assert (= (length result) 2))
      (assert (= (aref result 0) #x81))
      (assert (= (aref result 1) #xff))))

  (test-case "RLP encode integer 256 (two bytes)"
    ;; 256 = 0x0100, RLP = [0x82, 0x01, 0x00]
    (let ((result (coalton:coalton (web3/rlp:rlp-encode-integer 256))))
      (assert (= (length result) 3))
      (assert (= (aref result 0) #x82))
      (assert (= (aref result 1) #x01))
      (assert (= (aref result 2) #x00))))

  (test-case "RLP encode integer 65535"
    ;; 65535 = 0xffff, RLP = [0x82, 0xff, 0xff]
    (let ((result (coalton:coalton (web3/rlp:rlp-encode-integer 65535))))
      (assert (= (length result) 3))
      (assert (= (aref result 0) #x82))
      (assert (= (aref result 1) #xff))
      (assert (= (aref result 2) #xff))))

  (test-case "RLP encode integer 65536 (three bytes)"
    ;; 65536 = 0x010000, RLP = [0x83, 0x01, 0x00, 0x00]
    (let ((result (coalton:coalton (web3/rlp:rlp-encode-integer 65536))))
      (assert (= (length result) 4))
      (assert (= (aref result 0) #x83))
      (assert (= (aref result 1) #x01))))

  (test-case "RLP encode string 'hello world'"
    ;; 11 chars, RLP = [0x8b, 'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd']
    (let ((result (coalton:coalton (web3/rlp:rlp-encode-string "hello world"))))
      (assert (= (length result) 12))
      (assert (= (aref result 0) #x8b))  ; 0x80 + 11
      (assert (= (aref result 1) (char-code #\h)))))

  (test-case "RLP encode single character 'a'"
    ;; Single ASCII char < 0x80 is encoded as itself
    (let ((result (coalton:coalton (web3/rlp:rlp-encode-string "a"))))
      (assert (= (length result) 1))
      (assert (= (aref result 0) (char-code #\a)))))

  (test-case "RLP encode 55-byte string (short string boundary)"
    ;; Exactly 55 bytes uses short string encoding [0x80+55, data...]
    (let* ((str (make-string 55 :initial-element #\x))
           (result (coalton:coalton (web3/rlp:rlp-encode-string
                                     (coalton:lisp coalton:String () str)))))
      (assert (= (length result) 56))
      (assert (= (aref result 0) (+ #x80 55)))))

  (test-case "RLP encode 56-byte string (long string)"
    ;; 56 bytes requires long string encoding [0xb7+1, 56, data...]
    (let* ((str (make-string 56 :initial-element #\y))
           (result (coalton:coalton (web3/rlp:rlp-encode-string
                                     (coalton:lisp coalton:String () str)))))
      (assert (= (length result) 58))  ; 2 bytes prefix + 56 bytes
      (assert (= (aref result 0) #xb8))  ; 0xb7 + 1 (1 byte length)
      (assert (= (aref result 1) 56))))

  (test-case "RLP encode list with two strings"
    ;; ["cat", "dog"] = [0xc8, 0x83, 'c', 'a', 't', 0x83, 'd', 'o', 'g']
    ;; Build bytes from char codes
    (let ((result (coalton:coalton
                   (web3/rlp:rlp-encode
                    (web3/rlp:RlpList
                     (coalton:Cons (web3/rlp:RlpBytes
                                    (web3/types:bytes-from-list
                                     (coalton:Cons 99 (coalton:Cons 97 (coalton:Cons 116 coalton:Nil))))) ; 'c' 'a' 't'
                                   (coalton:Cons (web3/rlp:RlpBytes
                                                  (web3/types:bytes-from-list
                                                   (coalton:Cons 100 (coalton:Cons 111 (coalton:Cons 103 coalton:Nil))))) ; 'd' 'o' 'g'
                                                 coalton:Nil)))))))
      (assert (= (length result) 9))
      (assert (= (aref result 0) #xc8))
      (assert (= (aref result 1) #x83))  ; cat length prefix
      (assert (= (aref result 5) #x83)))) ; dog length prefix

  (test-case "RLP encode list with integers"
    ;; [1, 2, 3] encoded
    (let ((result (coalton:coalton
                   (web3/rlp:rlp-encode
                    (web3/rlp:RlpList
                     (coalton:Cons (web3/rlp:RlpBytes (web3/types:bytes-from-list (coalton:Cons 1 coalton:Nil)))
                                   (coalton:Cons (web3/rlp:RlpBytes (web3/types:bytes-from-list (coalton:Cons 2 coalton:Nil)))
                                                 (coalton:Cons (web3/rlp:RlpBytes (web3/types:bytes-from-list (coalton:Cons 3 coalton:Nil)))
                                                               coalton:Nil))))))))
      (assert (= (length result) 4))  ; 1 byte list prefix + 3 single bytes
      (assert (= (aref result 0) #xc3))  ; 0xc0 + 3
      (assert (= (aref result 1) 1))
      (assert (= (aref result 2) 2))
      (assert (= (aref result 3) 3))))

  (test-case "RLP encode byte 0x00"
    ;; Zero byte needs a length prefix: [0x00] -> [0x00]
    (let ((result (coalton:coalton
                   (web3/rlp:rlp-encode
                    (web3/rlp:RlpBytes
                     (web3/types:bytes-from-list (coalton:Cons 0 coalton:Nil)))))))
      (assert (= (length result) 1))
      (assert (= (aref result 0) #x00))))

  (test-case "RLP encode byte 0x7f (boundary)"
    ;; 0x7f encodes as itself (< 0x80)
    (let ((result (coalton:coalton
                   (web3/rlp:rlp-encode
                    (web3/rlp:RlpBytes
                     (web3/types:bytes-from-list (coalton:Cons #x7f coalton:Nil)))))))
      (assert (= (length result) 1))
      (assert (= (aref result 0) #x7f))))

  ;;; =========================================================================
  ;;; Additional RLP Decoding Tests
  ;;; =========================================================================

  (test-case "RLP decode short string"
    (let* ((encoded (make-array 4 :fill-pointer 4 :adjustable t
                                 :initial-contents (list #x83 (char-code #\c) (char-code #\a) (char-code #\t))))
           (result (coalton:coalton
                    (web3/rlp:rlp-decode
                     (coalton:lisp web3/types:Bytes () encoded)))))
      (assert (result-ok-p result))))

  (test-case "RLP decode empty string"
    (let* ((encoded (make-array 1 :fill-pointer 1 :adjustable t :initial-contents '(#x80)))
           (result (coalton:coalton
                    (web3/rlp:rlp-decode
                     (coalton:lisp web3/types:Bytes () encoded)))))
      (assert (result-ok-p result))))

  (test-case "RLP decode empty list"
    (let* ((encoded (make-array 1 :fill-pointer 1 :adjustable t :initial-contents '(#xc0)))
           (result (coalton:coalton
                    (web3/rlp:rlp-decode
                     (coalton:lisp web3/types:Bytes () encoded)))))
      (assert (result-ok-p result))))

  (test-case "RLP decode list with items"
    (let* ((encoded (make-array 9 :fill-pointer 9 :adjustable t
                                 :initial-contents (list #xc8 #x83 (char-code #\c) (char-code #\a) (char-code #\t)
                                                         #x83 (char-code #\d) (char-code #\o) (char-code #\g))))
           (result (coalton:coalton
                    (web3/rlp:rlp-decode
                     (coalton:lisp web3/types:Bytes () encoded)))))
      (assert (result-ok-p result))))

  (test-case "RLP decode multi-byte integer"
    (let* ((encoded (make-array 3 :fill-pointer 3 :adjustable t
                                 :initial-contents '(#x82 #x04 #x00)))  ; 1024
           (result (coalton:coalton
                    (web3/rlp:rlp-decode
                     (coalton:lisp web3/types:Bytes () encoded)))))
      (assert (result-ok-p result))))

  (test-case "RLP encode-decode roundtrip for string"
    (let* ((original "ethereum")
           (encoded (coalton:coalton (web3/rlp:rlp-encode-string
                                      (coalton:lisp coalton:String () original))))
           (decoded (coalton:coalton
                     (web3/rlp:rlp-decode
                      (coalton:lisp web3/types:Bytes () encoded)))))
      (assert (result-ok-p decoded))))

  (test-case "RLP encode-decode roundtrip for nested list"
    ;; Build bytes for "abc" from char codes: 97 98 99
    (let* ((item (coalton:coalton
                  (web3/rlp:RlpList
                   (coalton:Cons
                    (web3/rlp:RlpBytes
                     (web3/types:bytes-from-list
                      (coalton:Cons 97 (coalton:Cons 98 (coalton:Cons 99 coalton:Nil)))))
                    (coalton:Cons
                     (web3/rlp:RlpList coalton:Nil)
                     coalton:Nil)))))
           (encoded (coalton:coalton
                     (web3/rlp:rlp-encode
                      (coalton:lisp web3/rlp:RlpItem () item))))
           (decoded (coalton:coalton
                     (web3/rlp:rlp-decode
                      (coalton:lisp web3/types:Bytes () encoded)))))
      (assert (result-ok-p decoded)))))
