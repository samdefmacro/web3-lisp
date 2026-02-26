(in-package #:web3/types)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; U256 type - 256-bit unsigned integer
  ;;; Backed by CL bignum for arithmetic, stored as 4 U64 words (little-endian)

  (repr :native (cl:simple-array cl:t (4)))
  (define-type U256
    "256-bit unsigned integer")

  (declare u256-make (U64 -> U64 -> U64 -> U64 -> U256))
  (define (u256-make w0 w1 w2 w3)
    "Create a U256 from 4 U64 words (w0 is lowest, w3 is highest)"
    (lisp U256 (w0 w1 w2 w3)
      (cl:let ((arr (cl:make-array 4)))
        (cl:setf (cl:aref arr 0) w0)
        (cl:setf (cl:aref arr 1) w1)
        (cl:setf (cl:aref arr 2) w2)
        (cl:setf (cl:aref arr 3) w3)
        arr)))

  (declare u256-word (UFix -> U256 -> U64))
  (define (u256-word idx u)
    "Get the idx-th word (U64) from U256 (0 = lowest)"
    (lisp U64 (idx u)
      (cl:aref u idx)))

  (declare u256-zero (Unit -> U256))
  (define (u256-zero)
    "Create a zero U256"
    (u256-make 0 0 0 0))

  (declare u256-one (Unit -> U256))
  (define (u256-one)
    "Create a U256 with value 1"
    (u256-make 1 0 0 0))

  (declare u256-max (Unit -> U256))
  (define (u256-max)
    "Maximum U256 value (2^256 - 1)"
    (u256-make 18446744073709551615
               18446744073709551615
               18446744073709551615
               18446744073709551615))

  ;;; Bignum interop helpers (internal)

  (declare %u256-to-bignum (U256 -> Integer))
  (define (%u256-to-bignum u)
    "Convert U256 to CL bignum"
    (lisp Integer (u)
      (cl:+ (cl:aref u 0)
            (cl:ash (cl:aref u 1) 64)
            (cl:ash (cl:aref u 2) 128)
            (cl:ash (cl:aref u 3) 192))))

  (declare %bignum-to-u256 (Integer -> U256))
  (define (%bignum-to-u256 n)
    "Convert CL bignum to U256 (truncates to 256 bits)"
    (lisp U256 (n)
      (cl:let* ((mask (cl:1- (cl:ash 1 64)))
                (w0 (cl:logand n mask))
                (w1 (cl:logand (cl:ash n -64) mask))
                (w2 (cl:logand (cl:ash n -128) mask))
                (w3 (cl:logand (cl:ash n -192) mask))
                (arr (cl:make-array 4)))
        (cl:setf (cl:aref arr 0) w0)
        (cl:setf (cl:aref arr 1) w1)
        (cl:setf (cl:aref arr 2) w2)
        (cl:setf (cl:aref arr 3) w3)
        arr)))

  (declare u256-from-integer (Integer -> U256))
  (define (u256-from-integer n)
    "Create a U256 from an Integer (truncates to 256 bits)"
    (%bignum-to-u256 n))

  (declare u256-to-integer (U256 -> Integer))
  (define (u256-to-integer u)
    "Convert U256 to Integer"
    (%u256-to-bignum u))

  ;;; Arithmetic (via bignum)

  (declare u256-add (U256 -> U256 -> U256))
  (define (u256-add a b)
    "Add two U256 values (wraps on overflow)"
    (%bignum-to-u256 (+ (%u256-to-bignum a) (%u256-to-bignum b))))

  (declare u256-sub (U256 -> U256 -> U256))
  (define (u256-sub a b)
    "Subtract two U256 values (wraps on underflow)"
    (let ((result (- (%u256-to-bignum a) (%u256-to-bignum b))))
      (if (< result 0)
          ;; Wrap around: add 2^256
          (%bignum-to-u256
           (lisp Integer (result)
             (cl:+ result (cl:ash 1 256))))
          (%bignum-to-u256 result))))

  (declare u256-mul (U256 -> U256 -> U256))
  (define (u256-mul a b)
    "Multiply two U256 values (wraps on overflow)"
    (%bignum-to-u256 (* (%u256-to-bignum a) (%u256-to-bignum b))))

  (declare u256-div (U256 -> U256 -> (Web3Result U256)))
  (define (u256-div a b)
    "Divide two U256 values"
    (if (u256-zero? b)
        (Err (AbiError "Division by zero"))
        (let ((an (%u256-to-bignum a))
              (bn (%u256-to-bignum b)))
          (Ok (%bignum-to-u256
               (lisp Integer (an bn) (cl:floor an bn)))))))

  (declare u256-mod (U256 -> U256 -> (Web3Result U256)))
  (define (u256-mod a b)
    "Modulo of two U256 values"
    (if (u256-zero? b)
        (Err (AbiError "Modulo by zero"))
        (let ((an (%u256-to-bignum a))
              (bn (%u256-to-bignum b)))
          (Ok (%bignum-to-u256
               (lisp Integer (an bn) (cl:mod an bn)))))))

  ;;; Comparison

  (declare u256-zero? (U256 -> Boolean))
  (define (u256-zero? u)
    "Check if U256 is zero"
    (and (== (u256-word 0 u) 0)
         (== (u256-word 1 u) 0)
         (== (u256-word 2 u) 0)
         (== (u256-word 3 u) 0)))

  (declare u256-equal? (U256 -> U256 -> Boolean))
  (define (u256-equal? a b)
    "Check if two U256 values are equal"
    (and (== (u256-word 0 a) (u256-word 0 b))
         (== (u256-word 1 a) (u256-word 1 b))
         (== (u256-word 2 a) (u256-word 2 b))
         (== (u256-word 3 a) (u256-word 3 b))))

  (declare u256-less-than? (U256 -> U256 -> Boolean))
  (define (u256-less-than? a b)
    "Check if a < b"
    (let ((an (%u256-to-bignum a))
          (bn (%u256-to-bignum b)))
      (lisp Boolean (an bn) (cl:< an bn))))

  (declare u256-greater-than? (U256 -> U256 -> Boolean))
  (define (u256-greater-than? a b)
    "Check if a > b"
    (u256-less-than? b a))

  ;;; Bytes conversion

  (declare u256-to-bytes (U256 -> Bytes))
  (define (u256-to-bytes u)
    "Convert U256 to 32 bytes (big-endian)"
    (let ((n (%u256-to-bignum u)))
      (lisp Bytes (n)
        (cl:let* ((result (cl:make-array 32 :fill-pointer 32 :adjustable cl:t
                                            :initial-element 0)))
          (cl:loop :for i :from 0 :below 32
                   :do (cl:setf (cl:aref result (cl:- 31 i))
                                (cl:ldb (cl:byte 8 (cl:* i 8)) n)))
          result))))

  (declare u256-from-bytes (Bytes -> (Web3Result U256)))
  (define (u256-from-bytes bytes)
    "Convert 32 bytes (big-endian) to U256"
    (if (/= (bytes-length bytes) 32)
        (Err (HexError "U256 requires exactly 32 bytes"))
        (Ok (lisp U256 (bytes)
              (cl:let ((n 0))
                (cl:loop :for i :from 0 :below 32
                         :do (cl:setf n (cl:+ (cl:ash n 8) (cl:aref bytes i))))
                (%bignum-to-u256 n))))))

  ;;; Unit conversions (wei/gwei/ether)

  (declare wei-to-gwei (U256 -> U256))
  (define (wei-to-gwei wei)
    "Convert wei to gwei (divide by 10^9)"
    (let ((gwei-unit (u256-from-integer 1000000000)))
      (match (u256-div wei gwei-unit)
        ((Ok result) result)
        ((Err _) (u256-zero)))))

  (declare gwei-to-wei (U256 -> U256))
  (define (gwei-to-wei gwei)
    "Convert gwei to wei (multiply by 10^9)"
    (u256-mul gwei (u256-from-integer 1000000000)))

  (declare wei-to-ether-string (U256 -> String))
  (define (wei-to-ether-string wei)
    "Convert wei to ether as a decimal string"
    (let ((wei-int (%u256-to-bignum wei)))
      (lisp String (wei-int)
        (cl:let* ((n wei-int)
                  (ether-unit (cl:expt 10 18))
                  (whole (cl:floor n ether-unit))
                  (frac (cl:mod n ether-unit)))
          (cl:if (cl:zerop frac)
               (cl:format cl:nil "~D.0" whole)
               (cl:let ((frac-str (cl:format cl:nil "~18,'0D" frac)))
                 ;; Trim trailing zeros
                 (cl:format cl:nil "~D.~A" whole
                            (cl:string-right-trim "0" frac-str))))))))

  (declare ether-to-wei (String -> (Web3Result U256)))
  (define (ether-to-wei ether-str)
    "Convert ether string (e.g. \"1.5\") to wei"
    (lisp (Web3Result U256) (ether-str)
      (cl:handler-case
          (cl:let* ((dot-pos (cl:position #\. ether-str))
                    (whole-str (cl:if dot-pos
                                     (cl:subseq ether-str 0 dot-pos)
                                     ether-str))
                    (frac-str (cl:if dot-pos
                                    (cl:subseq ether-str (cl:1+ dot-pos))
                                    ""))
                    ;; Pad or truncate fractional part to 18 digits
                    (frac-padded (cl:if (cl:> (cl:length frac-str) 18)
                                        (cl:subseq frac-str 0 18)
                                        (cl:concatenate 'cl:string frac-str
                                                        (cl:make-string
                                                         (cl:- 18 (cl:length frac-str))
                                                         :initial-element #\0))))
                    (whole (cl:parse-integer whole-str))
                    (frac (cl:if (cl:string= frac-padded "")
                                 0
                                 (cl:parse-integer frac-padded)))
                    (wei (cl:+ (cl:* whole (cl:expt 10 18)) frac)))
            (coalton-prelude:Ok (%bignum-to-u256 wei)))
        (cl:error (e)
          (coalton-prelude:Err
           (HexError (cl:format cl:nil "Invalid ether string: ~A" e))))))))
