;;; Coalton test helpers for web3-coalton

(cl:in-package #:web3-tests)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Types Tests
  ;;; =========================================================================

  (declare test-hex-encode-empty (Unit -> Boolean))
  (define (test-hex-encode-empty _)
    "hex-encode of empty bytes should be empty string"
    (== (types:hex-encode (types:bytes-empty)) ""))

  (declare test-hex-encode-bytes (Unit -> Boolean))
  (define (test-hex-encode-bytes _)
    "hex-encode of [0xde, 0xad, 0xbe, 0xef] should be \"deadbeef\""
    (== (types:hex-encode
         (types:bytes-from-list (Cons #xde (Cons #xad (Cons #xbe (Cons #xef Nil))))))
        "deadbeef"))

  (declare test-hex-decode-roundtrip (Unit -> Boolean))
  (define (test-hex-decode-roundtrip _)
    "hex-decode(hex-encode(bytes)) should roundtrip"
    (let ((original (types:bytes-from-list (Cons 1 (Cons 2 (Cons 255 Nil))))))
      (match (types:hex-decode (types:hex-encode original))
        ((Err _) False)
        ((Ok decoded) (types:bytes-equal? original decoded)))))

  (declare test-hex-decode-prefixed (Unit -> Boolean))
  (define (test-hex-decode-prefixed _)
    "hex-decode should handle 0x prefix"
    (match (types:hex-decode "0xdeadbeef")
      ((Err _) False)
      ((Ok bytes)
       (and (== (types:bytes-length bytes) 4)
            (== (types:bytes-ref-unsafe 0 bytes) #xde)
            (== (types:bytes-ref-unsafe 3 bytes) #xef)))))

  (declare test-u256-zero (Unit -> Boolean))
  (define (test-u256-zero _)
    "u256-zero should be all zeros"
    (types:u256-zero? (types:u256-zero)))

  (declare test-u256-from-integer (Unit -> Boolean))
  (define (test-u256-from-integer _)
    "u256-from-integer should preserve small values"
    (let ((u (types:u256-from-integer 42)))
      (== (types:u256-to-integer u) 42)))

  (declare test-u256-add (Unit -> Boolean))
  (define (test-u256-add _)
    "U256 addition"
    (let ((a (types:u256-from-integer 100))
          (b (types:u256-from-integer 200)))
      (== (types:u256-to-integer (types:u256-add a b)) 300)))

  (declare test-u256-sub (Unit -> Boolean))
  (define (test-u256-sub _)
    "U256 subtraction"
    (let ((a (types:u256-from-integer 300))
          (b (types:u256-from-integer 100)))
      (== (types:u256-to-integer (types:u256-sub a b)) 200)))

  (declare test-u256-mul (Unit -> Boolean))
  (define (test-u256-mul _)
    "U256 multiplication"
    (let ((a (types:u256-from-integer 15))
          (b (types:u256-from-integer 20)))
      (== (types:u256-to-integer (types:u256-mul a b)) 300)))

  (declare test-u256-to-bytes-roundtrip (Unit -> Boolean))
  (define (test-u256-to-bytes-roundtrip _)
    "U256 bytes roundtrip"
    (let ((u (types:u256-from-integer 123456789)))
      (match (types:u256-from-bytes (types:u256-to-bytes u))
        ((Err _) False)
        ((Ok u2) (types:u256-equal? u u2)))))

  (declare test-u256-comparison (Unit -> Boolean))
  (define (test-u256-comparison _)
    "U256 comparison operators"
    (let ((a (types:u256-from-integer 100))
          (b (types:u256-from-integer 200)))
      (and (types:u256-less-than? a b)
           (types:u256-greater-than? b a)
           (not (types:u256-less-than? b a)))))

  (declare test-bytes-pad-left (Unit -> Boolean))
  (define (test-bytes-pad-left _)
    "bytes-pad-left should pad with zeros on left"
    (let ((padded (types:bytes-pad-left 4 (types:bytes-from-list (Cons 1 (Cons 2 Nil))))))
      (and (== (types:bytes-length padded) 4)
           (== (types:bytes-ref-unsafe 0 padded) 0)
           (== (types:bytes-ref-unsafe 1 padded) 0)
           (== (types:bytes-ref-unsafe 2 padded) 1)
           (== (types:bytes-ref-unsafe 3 padded) 2))))

  (declare test-bytes-pad-right (Unit -> Boolean))
  (define (test-bytes-pad-right _)
    "bytes-pad-right should pad with zeros on right"
    (let ((padded (types:bytes-pad-right 4 (types:bytes-from-list (Cons 1 (Cons 2 Nil))))))
      (and (== (types:bytes-length padded) 4)
           (== (types:bytes-ref-unsafe 0 padded) 1)
           (== (types:bytes-ref-unsafe 1 padded) 2)
           (== (types:bytes-ref-unsafe 2 padded) 0)
           (== (types:bytes-ref-unsafe 3 padded) 0))))

  (declare test-bytes-equal (Unit -> Boolean))
  (define (test-bytes-equal _)
    "bytes-equal? should work correctly"
    (let ((a (types:bytes-from-list (Cons 1 (Cons 2 (Cons 3 Nil)))))
          (b (types:bytes-from-list (Cons 1 (Cons 2 (Cons 3 Nil)))))
          (c (types:bytes-from-list (Cons 1 (Cons 2 (Cons 4 Nil))))))
      (and (types:bytes-equal? a b)
           (not (types:bytes-equal? a c)))))

  (declare test-ether-wei-conversion (Unit -> Boolean))
  (define (test-ether-wei-conversion _)
    "1 ether = 10^18 wei"
    (match (types:ether-to-wei "1.0")
      ((Err _) False)
      ((Ok wei)
       (== (types:wei-to-ether-string wei) "1.0"))))

  ;;; =========================================================================
  ;;; RLP Tests
  ;;; =========================================================================

  (declare test-rlp-single-byte (Unit -> Boolean))
  (define (test-rlp-single-byte _)
    "RLP encode single byte (0x00-0x7f) should be itself"
    (let ((encoded (rlp:rlp-encode (rlp:RlpBytes (types:bytes-from-list (Cons #x42 Nil))))))
      (and (== (types:bytes-length encoded) 1)
           (== (types:bytes-ref-unsafe 0 encoded) #x42))))

  (declare test-rlp-short-string (Unit -> Boolean))
  (define (test-rlp-short-string _)
    "RLP encode 'dog' should be [0x83, 'd', 'o', 'g']"
    (let ((encoded (rlp:rlp-encode-string "dog")))
      (and (== (types:bytes-length encoded) 4)
           (== (types:bytes-ref-unsafe 0 encoded) #x83))))

  (declare test-rlp-empty-string (Unit -> Boolean))
  (define (test-rlp-empty-string _)
    "RLP encode empty string should be [0x80]"
    (let ((encoded (rlp:rlp-encode (rlp:RlpBytes (types:bytes-empty)))))
      (and (== (types:bytes-length encoded) 1)
           (== (types:bytes-ref-unsafe 0 encoded) #x80))))

  (declare test-rlp-empty-list (Unit -> Boolean))
  (define (test-rlp-empty-list _)
    "RLP encode empty list should be [0xc0]"
    (let ((encoded (rlp:rlp-encode (rlp:RlpList Nil))))
      (and (== (types:bytes-length encoded) 1)
           (== (types:bytes-ref-unsafe 0 encoded) #xc0))))

  (declare test-rlp-integer-zero (Unit -> Boolean))
  (define (test-rlp-integer-zero _)
    "RLP encode integer 0 should be [0x80] (empty byte string)"
    (let ((encoded (rlp:rlp-encode-integer 0)))
      (and (== (types:bytes-length encoded) 1)
           (== (types:bytes-ref-unsafe 0 encoded) #x80))))

  (declare test-rlp-integer-small (Unit -> Boolean))
  (define (test-rlp-integer-small _)
    "RLP encode integer 15 should be [0x0f]"
    (let ((encoded (rlp:rlp-encode-integer 15)))
      (and (== (types:bytes-length encoded) 1)
           (== (types:bytes-ref-unsafe 0 encoded) #x0f))))

  (declare test-rlp-nested-list (Unit -> Boolean))
  (define (test-rlp-nested-list _)
    "RLP encode [ [], [[]], [ [], [[]] ] ] per Ethereum wiki"
    ;; This is the standard test vector from the Ethereum wiki
    (let ((encoded (rlp:rlp-encode
                    (rlp:RlpList
                     (Cons (rlp:RlpList Nil)
                           (Cons (rlp:RlpList (Cons (rlp:RlpList Nil) Nil))
                                 (Cons (rlp:RlpList
                                        (Cons (rlp:RlpList Nil)
                                              (Cons (rlp:RlpList (Cons (rlp:RlpList Nil) Nil))
                                                    Nil)))
                                       Nil)))))))
      ;; Expected: c7 c0 c1c0 c3c0c1c0
      (and (== (types:bytes-length encoded) 8)
           (== (types:bytes-ref-unsafe 0 encoded) #xc7))))

  (declare test-rlp-decode-roundtrip (Unit -> Boolean))
  (define (test-rlp-decode-roundtrip _)
    "RLP encode then decode should roundtrip for a list"
    (let ((item (rlp:RlpList
                 (Cons (rlp:RlpBytes (types:bytes-from-list (Cons 1 (Cons 2 (Cons 3 Nil)))))
                       (Cons (rlp:RlpBytes (types:bytes-from-list (Cons 4 (Cons 5 Nil))))
                             Nil)))))
      (let ((encoded (rlp:rlp-encode item)))
        (match (rlp:rlp-decode encoded)
          ((Err _) False)
          ((Ok (Tuple decoded _))
           (match decoded
             ((rlp:RlpList items) (== (list:length items) 2))
             (_ False)))))))

  ;;; =========================================================================
  ;;; Crypto Tests
  ;;; =========================================================================

  (declare test-keccak256-empty (Unit -> Boolean))
  (define (test-keccak256-empty _)
    "keccak256 of empty bytes should match known hash"
    ;; keccak256("") = c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470
    (let ((hash (crypto:keccak256 (types:bytes-empty))))
      (and (== (types:bytes-length hash) 32)
           (== (types:bytes-ref-unsafe 0 hash) #xc5)
           (== (types:bytes-ref-unsafe 1 hash) #xd2)
           (== (types:bytes-ref-unsafe 31 hash) #x70))))

  (declare test-keccak256-hello (Unit -> Boolean))
  (define (test-keccak256-hello _)
    "keccak256 of 'hello' should match known hash"
    ;; keccak256("hello") = 1c8aff950685c2ed4bc3174f3472287b56d9517b9c948127319a09a7a36deac8
    (let ((hello-bytes (types:bytes-from-list
                        (Cons #x68 (Cons #x65 (Cons #x6c (Cons #x6c (Cons #x6f Nil))))))))
      (let ((hash (crypto:keccak256 hello-bytes)))
        (and (== (types:bytes-ref-unsafe 0 hash) #x1c)
             (== (types:bytes-ref-unsafe 1 hash) #x8a)
             (== (types:bytes-ref-unsafe 31 hash) #xc8)))))

  ;;; =========================================================================
  ;;; Address Tests
  ;;; =========================================================================

  (declare test-address-from-hex (Unit -> Boolean))
  (define (test-address-from-hex _)
    "Should parse an address from hex"
    (match (addr:address-from-hex "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045")
      ((Err _) False)
      ((Ok address) (== (types:bytes-length (addr:address-bytes address)) 20))))

  (declare test-address-checksum (Unit -> Boolean))
  (define (test-address-checksum _)
    "EIP-55 checksum should produce correct mixed-case address"
    ;; Using Vitalik's address as test vector
    (match (addr:address-from-hex "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045")
      ((Err _) False)
      ((Ok address)
       ;; The checksum should produce the correctly-cased version
       (let ((checksummed (addr:address-to-checksum-hex address)))
         (== checksummed "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045")))))

  ;;; =========================================================================
  ;;; ABI Tests
  ;;; =========================================================================

  (declare test-function-selector-transfer (Unit -> Boolean))
  (define (test-function-selector-transfer _)
    "function selector for 'transfer(address,uint256)' should be 0xa9059cbb"
    (let ((selector (abi:function-selector "transfer(address,uint256)")))
      (and (== (types:bytes-length selector) 4)
           (== (types:bytes-ref-unsafe 0 selector) #xa9)
           (== (types:bytes-ref-unsafe 1 selector) #x05)
           (== (types:bytes-ref-unsafe 2 selector) #x9c)
           (== (types:bytes-ref-unsafe 3 selector) #xbb))))

  (declare test-abi-encode-uint256 (Unit -> Boolean))
  (define (test-abi-encode-uint256 _)
    "ABI encode uint256(1) should be 32 bytes with value 1 at end"
    (let ((encoded (abi:abi-encode
                    (Cons (abi:AbiUintVal (types:u256-from-integer 1)) Nil))))
      (and (== (types:bytes-length encoded) 32)
           (== (types:bytes-ref-unsafe 31 encoded) 1)
           (== (types:bytes-ref-unsafe 30 encoded) 0))))

  (declare test-abi-encode-bool (Unit -> Boolean))
  (define (test-abi-encode-bool _)
    "ABI encode bool(true) should have 1 at byte 31"
    (let ((encoded (abi:abi-encode (Cons (abi:AbiBoolVal True) Nil))))
      (and (== (types:bytes-length encoded) 32)
           (== (types:bytes-ref-unsafe 31 encoded) 1))))

  (declare test-abi-encode-address (Unit -> Boolean))
  (define (test-abi-encode-address _)
    "ABI encode address should be right-aligned in 32 bytes"
    (let ((addr-bytes (types:bytes-from-list
                       (Cons #xd8 (Cons #xda (Cons #x6b (Cons #xf2 (Cons #x69
                        (Cons #x64 (Cons #xaf (Cons #x9d (Cons #x7e (Cons #xed
                         (Cons #x9e (Cons #x03 (Cons #xe5 (Cons #x34 (Cons #x15
                          (Cons #xd3 (Cons #x7a (Cons #xa9 (Cons #x60 (Cons #x45
                           Nil)))))))))))))))))))))))
      (let ((encoded (abi:abi-encode (Cons (abi:AbiAddressVal addr-bytes) Nil))))
        (and (== (types:bytes-length encoded) 32)
             ;; First 12 bytes should be zero
             (== (types:bytes-ref-unsafe 0 encoded) 0)
             (== (types:bytes-ref-unsafe 11 encoded) 0)
             ;; Address starts at byte 12
             (== (types:bytes-ref-unsafe 12 encoded) #xd8)))))
)
