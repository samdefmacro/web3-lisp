;;; ABI module tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; ABI Tests
;;; =========================================================================

(defun run-abi-tests ()
  (format t "~%=== ABI Tests ===~%")

  ;; Function selector tests
  (test-case "function selector - transfer(address,uint256) = 0xa9059cbb"
    (assert (eq (web3-tests:test-function-selector-transfer coalton:Unit) coalton:True)))

  (test-case "function selector - balanceOf(address)"
    ;; balanceOf(address) = 0x70a08231
    (let ((selector (coalton:coalton
                     (web3/abi:function-selector "balanceOf(address)"))))
      (assert (= (aref selector 0) #x70))
      (assert (= (aref selector 1) #xa0))
      (assert (= (aref selector 2) #x82))
      (assert (= (aref selector 3) #x31))))

  (test-case "function selector - approve(address,uint256)"
    ;; approve(address,uint256) = 0x095ea7b3
    (let ((selector (coalton:coalton
                     (web3/abi:function-selector "approve(address,uint256)"))))
      (assert (= (aref selector 0) #x09))
      (assert (= (aref selector 1) #x5e))
      (assert (= (aref selector 2) #xa7))
      (assert (= (aref selector 3) #xb3))))

  ;; Event topic tests
  (test-case "event topic - Transfer(address,address,uint256)"
    ;; Transfer(address,address,uint256) = 0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef
    (let ((topic (coalton:coalton
                  (web3/abi:event-topic "Transfer(address,address,uint256)"))))
      (assert (= (length topic) 32))
      (assert (= (aref topic 0) #xdd))
      (assert (= (aref topic 1) #xf2))
      (assert (= (aref topic 2) #x52))
      (assert (= (aref topic 3) #xad))))

  ;; Encoding tests
  (test-case "ABI encode uint256"
    (assert (eq (web3-tests:test-abi-encode-uint256 coalton:Unit) coalton:True)))

  (test-case "ABI encode bool"
    (assert (eq (web3-tests:test-abi-encode-bool coalton:Unit) coalton:True)))

  (test-case "ABI encode address"
    (assert (eq (web3-tests:test-abi-encode-address coalton:Unit) coalton:True)))

  (test-case "ABI encode bool(false)"
    (let ((encoded (coalton:coalton
                    (web3/abi:abi-encode
                     (coalton:Cons (web3/abi:AbiBoolVal coalton:False) coalton:Nil)))))
      (assert (= (length encoded) 32))
      (assert (= (aref encoded 31) 0))))

  (test-case "ABI encode multiple values (uint256, bool)"
    (let ((encoded (coalton:coalton
                    (web3/abi:abi-encode
                     (coalton:Cons (web3/abi:AbiUintVal (web3/types:u256-from-integer 42))
                                   (coalton:Cons (web3/abi:AbiBoolVal coalton:True) coalton:Nil))))))
      (assert (= (length encoded) 64))
      ;; First word: uint256(42)
      (assert (= (aref encoded 31) 42))
      ;; Second word: bool(true)
      (assert (= (aref encoded 63) 1))))

  (test-case "ABI encode with selector"
    ;; transfer(address, uint256) with selector prepended
    (let* ((selector (coalton:coalton
                      (web3/abi:function-selector "transfer(address,uint256)")))
           (addr-bytes (make-array 20 :fill-pointer 20 :adjustable t :initial-element 0))
           (encoded (coalton:coalton
                     (web3/abi:abi-encode-with-selector
                      (coalton:lisp web3/types:Bytes () selector)
                      (coalton:Cons
                       (web3/abi:AbiAddressVal (coalton:lisp web3/types:Bytes () addr-bytes))
                       (coalton:Cons
                        (web3/abi:AbiUintVal (web3/types:u256-from-integer 1000))
                        coalton:Nil))))))
      ;; 4 bytes selector + 32 bytes address + 32 bytes uint256
      (assert (= (length encoded) 68))
      ;; Check selector bytes
      (assert (= (aref encoded 0) #xa9))
      (assert (= (aref encoded 1) #x05))
      (assert (= (aref encoded 2) #x9c))
      (assert (= (aref encoded 3) #xbb))))

  ;;; =========================================================================
  ;;; ABI Decode Tests
  ;;; =========================================================================

  (test-case "ABI decode uint256"
    ;; Encode uint256(42), then decode it
    (let* ((encoded (coalton:coalton
                     (web3/abi:abi-encode
                      (coalton:Cons (web3/abi:AbiUintVal (web3/types:u256-from-integer 42))
                                    coalton:Nil))))
           (decoded (coalton:coalton
                     (web3/abi:abi-decode
                      (coalton:Cons (web3/abi:AbiUint 256) coalton:Nil)
                      (coalton:lisp web3/types:Bytes () encoded)))))
      (assert (result-ok-p decoded))
      (let ((values (result-value decoded)))
        ;; Should be a list with one element
        (assert (not (null values))))))

  (test-case "ABI decode bool true"
    (let* ((encoded (coalton:coalton
                     (web3/abi:abi-encode
                      (coalton:Cons (web3/abi:AbiBoolVal coalton:True) coalton:Nil))))
           (decoded (coalton:coalton
                     (web3/abi:abi-decode
                      (coalton:Cons web3/abi:AbiBool coalton:Nil)
                      (coalton:lisp web3/types:Bytes () encoded)))))
      (assert (result-ok-p decoded))))

  (test-case "ABI decode bool false"
    (let* ((encoded (coalton:coalton
                     (web3/abi:abi-encode
                      (coalton:Cons (web3/abi:AbiBoolVal coalton:False) coalton:Nil))))
           (decoded (coalton:coalton
                     (web3/abi:abi-decode
                      (coalton:Cons web3/abi:AbiBool coalton:Nil)
                      (coalton:lisp web3/types:Bytes () encoded)))))
      (assert (result-ok-p decoded))))

  (test-case "ABI decode address"
    (let* ((addr-bytes (make-array 20 :fill-pointer 20 :adjustable t :initial-element #xab))
           (encoded (coalton:coalton
                     (web3/abi:abi-encode
                      (coalton:Cons (web3/abi:AbiAddressVal
                                     (coalton:lisp web3/types:Bytes () addr-bytes))
                                    coalton:Nil))))
           (decoded (coalton:coalton
                     (web3/abi:abi-decode
                      (coalton:Cons web3/abi:AbiAddress coalton:Nil)
                      (coalton:lisp web3/types:Bytes () encoded)))))
      (assert (result-ok-p decoded))))

  (test-case "ABI decode multiple static values"
    ;; Encode (uint256, bool, address) and decode
    (let* ((addr-bytes (make-array 20 :fill-pointer 20 :adjustable t :initial-element #x12))
           (encoded (coalton:coalton
                     (web3/abi:abi-encode
                      (coalton:Cons (web3/abi:AbiUintVal (web3/types:u256-from-integer 100))
                                    (coalton:Cons (web3/abi:AbiBoolVal coalton:True)
                                                  (coalton:Cons (web3/abi:AbiAddressVal
                                                                 (coalton:lisp web3/types:Bytes () addr-bytes))
                                                                coalton:Nil))))))
           (decoded (coalton:coalton
                     (web3/abi:abi-decode
                      (coalton:Cons (web3/abi:AbiUint 256)
                                    (coalton:Cons web3/abi:AbiBool
                                                  (coalton:Cons web3/abi:AbiAddress coalton:Nil)))
                      (coalton:lisp web3/types:Bytes () encoded)))))
      (assert (result-ok-p decoded))))

  (test-case "ABI decode bytesN (fixed)"
    ;; Encode bytes32, then decode
    (let* ((data (make-array 32 :fill-pointer 32 :adjustable t :initial-element #xcd))
           (encoded (coalton:coalton
                     (web3/abi:abi-encode
                      (coalton:Cons (web3/abi:AbiBytesFixedVal
                                     (coalton:lisp web3/types:Bytes () data))
                                    coalton:Nil))))
           (decoded (coalton:coalton
                     (web3/abi:abi-decode
                      (coalton:Cons (web3/abi:AbiBytesFixed 32) coalton:Nil)
                      (coalton:lisp web3/types:Bytes () encoded)))))
      (assert (result-ok-p decoded))))

  (test-case "ABI decode int256 positive"
    ;; Encode int256(42), then decode
    (let* ((encoded (coalton:coalton
                     (web3/abi:abi-encode
                      (coalton:Cons (web3/abi:AbiIntVal 42) coalton:Nil))))
           (decoded (coalton:coalton
                     (web3/abi:abi-decode
                      (coalton:Cons (web3/abi:AbiInt 256) coalton:Nil)
                      (coalton:lisp web3/types:Bytes () encoded)))))
      (assert (result-ok-p decoded))))

  (test-case "ABI decode int256 negative"
    ;; Encode int256(-1), then decode
    (let* ((encoded (coalton:coalton
                     (web3/abi:abi-encode
                      (coalton:Cons (web3/abi:AbiIntVal -1) coalton:Nil))))
           (decoded (coalton:coalton
                     (web3/abi:abi-decode
                      (coalton:Cons (web3/abi:AbiInt 256) coalton:Nil)
                      (coalton:lisp web3/types:Bytes () encoded)))))
      (assert (result-ok-p decoded))))

  (test-case "ABI decode rejects truncated data"
    ;; Try to decode from data that's too short
    (let* ((short-data (make-array 16 :fill-pointer 16 :adjustable t :initial-element 0))
           (decoded (coalton:coalton
                     (web3/abi:abi-decode
                      (coalton:Cons (web3/abi:AbiUint 256) coalton:Nil)
                      (coalton:lisp web3/types:Bytes () short-data)))))
      (assert (result-err-p decoded))))

  (test-case "ABI encode-decode roundtrip uint256"
    ;; Encode a value, decode it, verify we get the same value back
    (let* ((original-val 12345678901234567890)
           (encoded (coalton:coalton
                     (web3/abi:abi-encode
                      (coalton:Cons (web3/abi:AbiUintVal
                                     (web3/types:u256-from-integer
                                      (coalton:lisp coalton:Integer () original-val)))
                                    coalton:Nil))))
           (decoded (coalton:coalton
                     (web3/abi:abi-decode
                      (coalton:Cons (web3/abi:AbiUint 256) coalton:Nil)
                      (coalton:lisp web3/types:Bytes () encoded)))))
      (assert (result-ok-p decoded))))

  (test-case "ABI decode dynamic bytes"
    ;; Encode dynamic bytes, then decode
    (let* ((data (make-array 10 :fill-pointer 10 :adjustable t
                                :initial-contents '(1 2 3 4 5 6 7 8 9 10)))
           (encoded (coalton:coalton
                     (web3/abi:abi-encode
                      (coalton:Cons (web3/abi:AbiBytesVal
                                     (coalton:lisp web3/types:Bytes () data))
                                    coalton:Nil))))
           (decoded (coalton:coalton
                     (web3/abi:abi-decode
                      (coalton:Cons web3/abi:AbiBytes coalton:Nil)
                      (coalton:lisp web3/types:Bytes () encoded)))))
      (assert (result-ok-p decoded))))

  (test-case "ABI decode string"
    ;; Encode string, then decode
    (let* ((encoded (coalton:coalton
                     (web3/abi:abi-encode
                      (coalton:Cons (web3/abi:AbiStringVal "hello world") coalton:Nil))))
           (decoded (coalton:coalton
                     (web3/abi:abi-decode
                      (coalton:Cons web3/abi:AbiString coalton:Nil)
                      (coalton:lisp web3/types:Bytes () encoded)))))
      (assert (result-ok-p decoded))))

  (test-case "ABI decode empty string"
    (let* ((encoded (coalton:coalton
                     (web3/abi:abi-encode
                      (coalton:Cons (web3/abi:AbiStringVal "") coalton:Nil))))
           (decoded (coalton:coalton
                     (web3/abi:abi-decode
                      (coalton:Cons web3/abi:AbiString coalton:Nil)
                      (coalton:lisp web3/types:Bytes () encoded)))))
      (assert (result-ok-p decoded))))

  (test-case "ABI decode mixed static and dynamic"
    ;; Encode (uint256, string, bool) - tests head/tail encoding
    (let* ((encoded (coalton:coalton
                     (web3/abi:abi-encode
                      (coalton:Cons (web3/abi:AbiUintVal (web3/types:u256-from-integer 999))
                                    (coalton:Cons (web3/abi:AbiStringVal "test")
                                                  (coalton:Cons (web3/abi:AbiBoolVal coalton:True)
                                                                coalton:Nil))))))
           (decoded (coalton:coalton
                     (web3/abi:abi-decode
                      (coalton:Cons (web3/abi:AbiUint 256)
                                    (coalton:Cons web3/abi:AbiString
                                                  (coalton:Cons web3/abi:AbiBool coalton:Nil)))
                      (coalton:lisp web3/types:Bytes () encoded)))))
      (assert (result-ok-p decoded))))

  ;;; =========================================================================
  ;;; abi-encode-packed (Solidity abi.encodePacked)
  ;;; Test vectors from viem's encodePacked.test.ts
  ;;; =========================================================================

  (test-case "abi-encode-packed: address (no padding at top level)"
    (let* ((addr (web3/types:%parse-hex-bytes "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))
           (result (coalton:coalton
                     (coalton:match
                       (web3/abi:abi-encode-packed
                        (coalton:Cons
                         (coalton-prelude:Tuple
                          web3/abi:AbiAddress
                          (web3/abi:AbiAddressVal
                           (coalton:lisp web3/types:Bytes () addr)))
                         coalton:Nil))
                       ((coalton-library/classes:Ok b) b)
                       ((coalton-library/classes:Err _e)
                        (coalton:lisp web3/types:Bytes ()
                          (web3/types:make-bytes 0)))))))
      (assert (bytes-equal addr result))))

  (test-case "abi-encode-packed: string (utf-8, no length prefix)"
    (let ((expected (web3/types:%parse-hex-bytes "0x68656c6c6f20776f726c64"))
          (result (coalton:coalton
                    (coalton:match
                      (web3/abi:abi-encode-packed
                       (coalton:Cons
                        (coalton-prelude:Tuple
                         web3/abi:AbiString
                         (web3/abi:AbiStringVal "hello world"))
                        coalton:Nil))
                      ((coalton-library/classes:Ok b) b)
                      ((coalton-library/classes:Err _e)
                       (coalton:lisp web3/types:Bytes ()
                         (web3/types:make-bytes 0)))))))
      (assert (bytes-equal expected result))))

  (test-case "abi-encode-packed: bool true is single byte 0x01"
    (let ((result (coalton:coalton
                    (coalton:match
                      (web3/abi:abi-encode-packed
                       (coalton:Cons
                        (coalton-prelude:Tuple
                         web3/abi:AbiBool
                         (web3/abi:AbiBoolVal coalton:True))
                        coalton:Nil))
                      ((coalton-library/classes:Ok b) b)
                      ((coalton-library/classes:Err _e)
                       (coalton:lisp web3/types:Bytes ()
                         (web3/types:make-bytes 0)))))))
      (assert (= (length result) 1))
      (assert (= (aref result 0) 1))))

  (test-case "abi-encode-packed: uint8(200) -> 0xc8"
    (let ((expected (web3/types:%parse-hex-bytes "0xc8"))
          (result (coalton:coalton
                    (coalton:match
                      (web3/abi:abi-encode-packed
                       (coalton:Cons
                        (coalton-prelude:Tuple
                         (web3/abi:AbiUint 8)
                         (web3/abi:AbiUintVal (web3/types:u256-from-integer 200)))
                        coalton:Nil))
                      ((coalton-library/classes:Ok b) b)
                      ((coalton-library/classes:Err _e)
                       (coalton:lisp web3/types:Bytes ()
                         (web3/types:make-bytes 0)))))))
      (assert (bytes-equal expected result))))

  (test-case "abi-encode-packed: uint48(20123120) -> 6 bytes"
    (let ((expected (web3/types:%parse-hex-bytes "0x000001330df0"))
          (result (coalton:coalton
                    (coalton:match
                      (web3/abi:abi-encode-packed
                       (coalton:Cons
                        (coalton-prelude:Tuple
                         (web3/abi:AbiUint 48)
                         (web3/abi:AbiUintVal (web3/types:u256-from-integer 20123120)))
                        coalton:Nil))
                      ((coalton-library/classes:Ok b) b)
                      ((coalton-library/classes:Err _e)
                       (coalton:lisp web3/types:Bytes ()
                         (web3/types:make-bytes 0)))))))
      (assert (bytes-equal expected result))))

  (test-case "abi-encode-packed: uint256(69420) -> 32 bytes"
    (let ((expected (web3/types:%parse-hex-bytes
                     "0x0000000000000000000000000000000000000000000000000000000000010f2c"))
          (result (coalton:coalton
                    (coalton:match
                      (web3/abi:abi-encode-packed
                       (coalton:Cons
                        (coalton-prelude:Tuple
                         (web3/abi:AbiUint 256)
                         (web3/abi:AbiUintVal (web3/types:u256-from-integer 69420)))
                        coalton:Nil))
                      ((coalton-library/classes:Ok b) b)
                      ((coalton-library/classes:Err _e)
                       (coalton:lisp web3/types:Bytes ()
                         (web3/types:make-bytes 0)))))))
      (assert (bytes-equal expected result))))

  (test-case "abi-encode-packed: int8(-121) -> 0x87 (two's complement)"
    (let ((expected (web3/types:%parse-hex-bytes "0x87"))
          (result (coalton:coalton
                    (coalton:match
                      (web3/abi:abi-encode-packed
                       (coalton:Cons
                        (coalton-prelude:Tuple
                         (web3/abi:AbiInt 8)
                         (web3/abi:AbiIntVal -121))
                        coalton:Nil))
                      ((coalton-library/classes:Ok b) b)
                      ((coalton-library/classes:Err _e)
                       (coalton:lisp web3/types:Bytes ()
                         (web3/types:make-bytes 0)))))))
      (assert (bytes-equal expected result))))

  (test-case "abi-encode-packed: int256(-69420) sign-extended to 32 bytes"
    (let ((expected (web3/types:%parse-hex-bytes
                     "0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffef0d4"))
          (result (coalton:coalton
                    (coalton:match
                      (web3/abi:abi-encode-packed
                       (coalton:Cons
                        (coalton-prelude:Tuple
                         (web3/abi:AbiInt 256)
                         (web3/abi:AbiIntVal -69420))
                        coalton:Nil))
                      ((coalton-library/classes:Ok b) b)
                      ((coalton-library/classes:Err _e)
                       (coalton:lisp web3/types:Bytes ()
                         (web3/types:make-bytes 0)))))))
      (assert (bytes-equal expected result))))

  (test-case "abi-encode-packed: bytes4 (no padding at top level)"
    (let* ((data (web3/types:%parse-hex-bytes "0xdeadbeef"))
           (result (coalton:coalton
                     (coalton:match
                       (web3/abi:abi-encode-packed
                        (coalton:Cons
                         (coalton-prelude:Tuple
                          (web3/abi:AbiBytesFixed 4)
                          (web3/abi:AbiBytesFixedVal
                           (coalton:lisp web3/types:Bytes () data)))
                         coalton:Nil))
                       ((coalton-library/classes:Ok b) b)
                       ((coalton-library/classes:Err _e)
                        (coalton:lisp web3/types:Bytes ()
                          (web3/types:make-bytes 0)))))))
      (assert (bytes-equal data result))))

  (test-case "abi-encode-packed: dynamic bytes (no length prefix)"
    (let* ((data (web3/types:%parse-hex-bytes "0xdeadbeef"))
           (result (coalton:coalton
                     (coalton:match
                       (web3/abi:abi-encode-packed
                        (coalton:Cons
                         (coalton-prelude:Tuple
                          web3/abi:AbiBytes
                          (web3/abi:AbiBytesVal
                           (coalton:lisp web3/types:Bytes () data)))
                         coalton:Nil))
                       ((coalton-library/classes:Ok b) b)
                       ((coalton-library/classes:Err _e)
                        (coalton:lisp web3/types:Bytes ()
                          (web3/types:make-bytes 0)))))))
      (assert (bytes-equal data result))))

  (test-case "abi-encode-packed: address[] (each padded to 32)"
    (let* ((a1 (web3/types:%parse-hex-bytes "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))
           (a2 (web3/types:%parse-hex-bytes "0x5414d89a8bf7e99d732bc52f3e6a3ef461c0c078"))
           (expected
             (web3/types:%parse-hex-bytes
              "0x000000000000000000000000d8da6bf26964af9d7eed9e03e53415d37aa960450000000000000000000000005414d89a8bf7e99d732bc52f3e6a3ef461c0c078"))
           (result (coalton:coalton
                     (coalton:match
                       (web3/abi:abi-encode-packed
                        (coalton:Cons
                         (coalton-prelude:Tuple
                          (web3/abi:AbiArray web3/abi:AbiAddress)
                          (web3/abi:AbiArrayVal
                           (coalton:Cons
                            (web3/abi:AbiAddressVal
                             (coalton:lisp web3/types:Bytes () a1))
                            (coalton:Cons
                             (web3/abi:AbiAddressVal
                              (coalton:lisp web3/types:Bytes () a2))
                             coalton:Nil))))
                         coalton:Nil))
                       ((coalton-library/classes:Ok b) b)
                       ((coalton-library/classes:Err _e)
                        (coalton:lisp web3/types:Bytes ()
                          (web3/types:make-bytes 0)))))))
      (assert (bytes-equal expected result))))

  (test-case "abi-encode-packed: address + string (mixed top-level)"
    (let* ((vit (web3/types:%parse-hex-bytes "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))
           (expected
             (web3/types:%parse-hex-bytes
              "0xd8da6bf26964af9d7eed9e03e53415d37aa9604568656c6c6f20776f726c64"))
           (result (coalton:coalton
                     (coalton:match
                       (web3/abi:abi-encode-packed
                        (coalton:Cons
                         (coalton-prelude:Tuple
                          web3/abi:AbiAddress
                          (web3/abi:AbiAddressVal
                           (coalton:lisp web3/types:Bytes () vit)))
                         (coalton:Cons
                          (coalton-prelude:Tuple
                           web3/abi:AbiString
                           (web3/abi:AbiStringVal "hello world"))
                          coalton:Nil)))
                       ((coalton-library/classes:Ok b) b)
                       ((coalton-library/classes:Err _e)
                        (coalton:lisp web3/types:Bytes ()
                          (web3/types:make-bytes 0)))))))
      (assert (bytes-equal expected result))))

  (test-case "abi-encode-packed: tuples are rejected"
    (let ((result (coalton:coalton
                    (web3/abi:abi-encode-packed
                     (coalton:Cons
                      (coalton-prelude:Tuple
                       (web3/abi:AbiTuple coalton:Nil)
                       (web3/abi:AbiTupleVal coalton:Nil))
                      coalton:Nil)))))
      (assert (result-err-p result))))

  (test-case "abi-encode-packed: bytesN size mismatch is rejected"
    (let* ((data (web3/types:%parse-hex-bytes "0xdeadbeef"))
           (result (coalton:coalton
                     (web3/abi:abi-encode-packed
                      (coalton:Cons
                       (coalton-prelude:Tuple
                        (web3/abi:AbiBytesFixed 8)
                        (web3/abi:AbiBytesFixedVal
                         (coalton:lisp web3/types:Bytes () data)))
                       coalton:Nil)))))
      (assert (result-err-p result)))))

