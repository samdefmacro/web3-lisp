(in-package #:web3/abi)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; Solidity abi.encodePacked - tight packing without 32-byte alignment.
  ;;; Used for keccak256(abi.encodePacked(...)) patterns: Merkle trees,
  ;;; CREATE2 salts, Uniswap V3 path encoding, signed message digests.
  ;;;
  ;;; Packing rules (top level):
  ;;;   uint<N>     -> N/8 bytes, big-endian
  ;;;   int<N>      -> N/8 bytes, two's complement big-endian
  ;;;   address     -> 20 bytes
  ;;;   bool        -> 1 byte (0x00 or 0x01)
  ;;;   bytes<N>    -> N bytes (must match)
  ;;;   bytes       -> as-is, no length prefix (even inside arrays)
  ;;;   string      -> UTF-8 bytes, no length prefix
  ;;;   T[] / T[N]  -> elements packed, statically-sized elements padded to
  ;;;                  32 bytes; no length prefix
  ;;;   tuple       -> NOT supported (per Solidity)

  (declare abi-encode-packed ((List (Tuple AbiType AbiValue))
                              -> (types:Web3Result types:Bytes)))
  (define (abi-encode-packed pairs)
    "Solidity abi.encodePacked(types, values) - tight packing without padding."
    (match (%encode-each pairs False Nil)
      ((Err e) (Err e))
      ((Ok parts) (Ok (types:bytes-concat-many (list:reverse parts))))))

  (declare %encode-each ((List (Tuple AbiType AbiValue))
                         -> Boolean
                         -> (List types:Bytes)
                         -> (types:Web3Result (List types:Bytes))))
  (define (%encode-each pairs in-array? acc)
    (match pairs
      ((Nil) (Ok acc))
      ((Cons (Tuple typ val) rest)
       (match (%encode-packed-one typ val in-array?)
         ((Err e) (Err e))
         ((Ok part) (%encode-each rest in-array? (Cons part acc)))))))

  (declare %encode-array (AbiType -> (List AbiValue) -> (types:Web3Result types:Bytes)))
  (define (%encode-array inner items)
    (match (%encode-array-loop inner items Nil)
      ((Err e) (Err e))
      ((Ok parts) (Ok (types:bytes-concat-many (list:reverse parts))))))

  (declare %encode-array-loop (AbiType
                               -> (List AbiValue)
                               -> (List types:Bytes)
                               -> (types:Web3Result (List types:Bytes))))
  (define (%encode-array-loop inner items acc)
    (match items
      ((Nil) (Ok acc))
      ((Cons v rest)
       (match (%encode-packed-one inner v True)
         ((Err e) (Err e))
         ((Ok part) (%encode-array-loop inner rest (Cons part acc)))))))

  (declare %encode-packed-one (AbiType -> AbiValue -> Boolean
                                       -> (types:Web3Result types:Bytes)))
  (define (%encode-packed-one typ val in-array?)
    (match (Tuple typ val)
      ((Tuple (AbiUint bits) (AbiUintVal u))
       (match (%check-int-bits bits)
         ((Err e) (Err e))
         ((Ok size)
          (let ((packed (%u256-to-packed-bytes u size)))
            (Ok (if in-array? (types:bytes-pad-left 32 packed) packed))))))

      ((Tuple (AbiInt bits) (AbiIntVal i))
       (match (%check-int-bits bits)
         ((Err e) (Err e))
         ((Ok size)
          (let ((packed (%int-to-packed-bytes i size)))
            (Ok (if in-array? (%sign-extend-32 packed i) packed))))))

      ((Tuple (AbiAddress) (AbiAddressVal bs))
       (if (/= (types:bytes-length bs) 20)
           (Err (types:AbiError "abi-encode-packed: address must be 20 bytes"))
           (Ok (if in-array? (types:bytes-pad-left 32 bs) bs))))

      ((Tuple (AbiBool) (AbiBoolVal b))
       (let ((one-byte (types:make-bytes 1)))
         (when b (types:bytes-set! 0 1 one-byte))
         (Ok (if in-array? (types:bytes-pad-left 32 one-byte) one-byte))))

      ((Tuple (AbiBytesFixed n) (AbiBytesFixedVal bs))
       (let ((actual (types:bytes-length bs)))
         (if (/= actual n)
             (Err (types:AbiError
                   (lisp String (n actual)
                     (cl:format cl:nil
                                "abi-encode-packed: bytes~D requires ~D bytes, got ~D"
                                n n actual))))
             (Ok (if in-array? (types:bytes-pad-right 32 bs) bs)))))

      ((Tuple (AbiBytes) (AbiBytesVal bs))
       (Ok bs))

      ((Tuple (AbiString) (AbiStringVal s))
       (Ok (types:string-to-bytes s)))

      ((Tuple (AbiArray inner) (AbiArrayVal items))
       (%encode-array inner items))

      ((Tuple (AbiFixedArray inner _) (AbiFixedArrayVal items))
       (%encode-array inner items))

      ((Tuple (AbiTuple _) _)
       (Err (types:AbiError "abi-encode-packed: tuples are not supported")))

      (_
       (Err (types:AbiError "abi-encode-packed: type/value mismatch")))))

  (declare %check-int-bits (UFix -> (types:Web3Result UFix)))
  (define (%check-int-bits bits)
    "Validate bits is 8..256 and divisible by 8; return byte size."
    (if (or (< bits 8) (> bits 256))
        (Err (types:AbiError "abi-encode-packed: int/uint bit width must be 8..256"))
        (let ((rem (lisp UFix (bits) (cl:mod bits 8))))
          (if (/= rem 0)
              (Err (types:AbiError "abi-encode-packed: int/uint bit width must be a multiple of 8"))
              (Ok (lisp UFix (bits) (cl:floor bits 8)))))))

  (declare %u256-to-packed-bytes (types:U256 -> UFix -> types:Bytes))
  (define (%u256-to-packed-bytes u size)
    "Encode U256 as `size` big-endian bytes (low bytes if size < 32)."
    (if (== size 32)
        (types:u256-to-bytes u)
        (let ((n (types:u256-to-integer u)))
          (lisp types:Bytes (n size)
            (cl:let ((result (cl:make-array size :fill-pointer size
                                                  :adjustable cl:t
                                                  :initial-element 0)))
              (cl:loop :for i :from 0 :below size
                       :do (cl:setf (cl:aref result (cl:- size 1 i))
                                    (cl:ldb (cl:byte 8 (cl:* i 8)) n)))
              result)))))

  (declare %int-to-packed-bytes (Integer -> UFix -> types:Bytes))
  (define (%int-to-packed-bytes n size)
    "Encode signed Integer as `size` big-endian bytes (two's complement)."
    (lisp types:Bytes (n size)
      (cl:let* ((bits (cl:* size 8))
                (mask (cl:1- (cl:ash 1 bits)))
                (val (cl:logand n mask))
                (result (cl:make-array size :fill-pointer size
                                             :adjustable cl:t
                                             :initial-element 0)))
        (cl:loop :for i :from 0 :below size
                 :do (cl:setf (cl:aref result (cl:- size 1 i))
                              (cl:ldb (cl:byte 8 (cl:* i 8)) val)))
        result)))

  (declare %sign-extend-32 (types:Bytes -> Integer -> types:Bytes))
  (define (%sign-extend-32 packed n)
    "Pad packed signed integer to 32 bytes, sign-extending if negative."
    (let ((cur (types:bytes-length packed)))
      (if (>= cur 32)
          packed
          (let ((pad-len (- 32 cur))
                (fill (if (lisp Boolean (n) (cl:minusp n)) #xff 0)))
            (let ((pad (lisp types:Bytes (pad-len fill)
                         (cl:make-array pad-len :fill-pointer pad-len
                                                 :adjustable cl:t
                                                 :initial-element fill))))
              (types:bytes-append pad packed)))))))
