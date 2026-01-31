(in-package #:web3/types)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; Bytes type - represents a byte vector
  (define-type-alias Bytes (vec:Vector U8))

  (declare bytes-length (Bytes -> UFix))
  (define (bytes-length bytes)
    "Get the length of a byte vector"
    (vec:length bytes))

  (declare bytes-ref (UFix -> Bytes -> (Optional U8)))
  (define (bytes-ref idx bytes)
    "Get a byte at a specific index (safe)"
    (vec:index idx bytes))

  (declare bytes-ref-unsafe (UFix -> Bytes -> U8))
  (define (bytes-ref-unsafe idx bytes)
    "Get a byte at a specific index (unsafe - assumes valid index)"
    (vec:index-unsafe idx bytes))

  (declare make-bytes (UFix -> Bytes))
  (define (make-bytes len)
    "Create a byte vector of given length filled with zeros"
    (vec:with-initial-element len 0))

  (declare bytes-from-list ((List U8) -> Bytes))
  (define (bytes-from-list lst)
    "Create a byte vector from a list of bytes"
    (iter:collect! (iter:into-iter lst)))

  (declare bytes-empty (Unit -> Bytes))
  (define (bytes-empty)
    "Create an empty byte vector"
    (make-bytes 0))

  (declare bytes-slice (UFix -> UFix -> Bytes -> Bytes))
  (define (bytes-slice start len bytes)
    "Extract a slice from bytes starting at start with given length"
    (lisp Bytes (start len bytes)
      (cl:let ((result (cl:make-array len :fill-pointer 0 :adjustable cl:t)))
        (cl:loop :for i :from 0 :below len
                 :for idx := (cl:+ start i)
                 :when (cl:< idx (cl:length bytes))
                 :do (cl:vector-push-extend (cl:aref bytes idx) result))
        result)))

  (declare bytes-take (UFix -> Bytes -> Bytes))
  (define (bytes-take n bytes)
    "Take the first n bytes"
    (bytes-slice 0 n bytes))

  (declare bytes-drop (UFix -> Bytes -> Bytes))
  (define (bytes-drop n bytes)
    "Drop the first n bytes"
    (let ((total (bytes-length bytes)))
      (if (>= n total)
          (bytes-empty)
          (bytes-slice n (- total n) bytes))))

  (declare bytes-append (Bytes -> Bytes -> Bytes))
  (define (bytes-append a b)
    "Concatenate two byte vectors"
    (let ((result (vec:copy a)))
      (for elem in (iter:into-iter b)
        (vec:push! elem result))
      result))

  (declare bytes-concat-many ((List Bytes) -> Bytes))
  (define (bytes-concat-many parts)
    "Concatenate a list of byte vectors"
    (let ((result (make-bytes 0)))
      (for part in (iter:into-iter parts)
        (for elem in (iter:into-iter part)
          (vec:push! elem result)))
      result))

  (declare bytes-set! (UFix -> U8 -> Bytes -> Unit))
  (define (bytes-set! idx val bytes)
    "Set a byte at a specific index"
    (vec:set! idx val bytes))

  (declare bytes-copy (Bytes -> Bytes))
  (define (bytes-copy bytes)
    "Create a copy of a byte vector"
    (vec:copy bytes))

  (declare bytes-equal? (Bytes -> Bytes -> Boolean))
  (define (bytes-equal? a b)
    "Check if two byte vectors are equal"
    (lisp Boolean (a b)
      (cl:and (cl:= (cl:length a) (cl:length b))
              (cl:loop :for i :from 0 :below (cl:length a)
                       :always (cl:= (cl:aref a i) (cl:aref b i))))))

  (declare bytes-reverse (Bytes -> Bytes))
  (define (bytes-reverse bytes)
    "Reverse a byte vector"
    (lisp Bytes (bytes)
      (cl:let* ((len (cl:length bytes))
                (result (cl:make-array len :fill-pointer len :adjustable cl:t)))
        (cl:loop :for i :from 0 :below len
                 :do (cl:setf (cl:aref result i)
                              (cl:aref bytes (cl:- len 1 i))))
        result)))

  (declare bytes-pad-left (UFix -> Bytes -> Bytes))
  (define (bytes-pad-left target-len bytes)
    "Pad bytes with zeros on the left to reach target length"
    (let ((cur-len (bytes-length bytes)))
      (if (>= cur-len target-len)
          bytes
          (let ((pad (make-bytes (- target-len cur-len))))
            (bytes-append pad bytes)))))

  (declare bytes-pad-right (UFix -> Bytes -> Bytes))
  (define (bytes-pad-right target-len bytes)
    "Pad bytes with zeros on the right to reach target length"
    (let ((cur-len (bytes-length bytes)))
      (if (>= cur-len target-len)
          bytes
          (let ((pad (make-bytes (- target-len cur-len))))
            (bytes-append bytes pad)))))

  ;;; Hex encoding/decoding

  (declare hex-encode (Bytes -> String))
  (define (hex-encode bytes)
    "Encode bytes as a lowercase hex string (no 0x prefix)"
    (lisp String (bytes)
      (cl:with-output-to-string (s)
        (cl:loop :for b :across bytes
                 :do (cl:format s "~(~2,'0x~)" b)))))

  (declare hex-encode-prefixed (Bytes -> String))
  (define (hex-encode-prefixed bytes)
    "Encode bytes as a 0x-prefixed lowercase hex string"
    (lisp String (bytes)
      (cl:with-output-to-string (s)
        (cl:write-string "0x" s)
        (cl:loop :for b :across bytes
                 :do (cl:format s "~(~2,'0x~)" b)))))

  (declare hex-decode (String -> (Web3Result Bytes)))
  (define (hex-decode hex-str)
    "Decode a hex string (with or without 0x prefix) to bytes"
    (lisp (Web3Result Bytes) (hex-str)
      (cl:handler-case
          (cl:let* ((str (cl:if (cl:and (cl:>= (cl:length hex-str) 2)
                                        (cl:string= hex-str "0x" :end1 2))
                                (cl:subseq hex-str 2)
                                hex-str))
                    ;; Handle odd-length hex strings by prepending a 0
                    (str (cl:if (cl:oddp (cl:length str))
                                (cl:concatenate 'cl:string "0" str)
                                str))
                    (len (cl:floor (cl:length str) 2))
                    (result (cl:make-array len :fill-pointer len :adjustable cl:t)))
            (cl:loop :for i :from 0 :below len
                     :for pos := (cl:* i 2)
                     :do (cl:setf (cl:aref result i)
                                  (cl:parse-integer str :start pos :end (cl:+ pos 2)
                                                        :radix 16)))
            (coalton-prelude:Ok result))
        (cl:error (e)
          (coalton-prelude:Err
           (HexError (cl:format cl:nil "Invalid hex string: ~A" e)))))))

  (declare hex-decode-prefixed (String -> (Web3Result Bytes)))
  (define (hex-decode-prefixed hex-str)
    "Decode a 0x-prefixed hex string to bytes (requires prefix)"
    (lisp (Web3Result Bytes) (hex-str)
      (cl:if (cl:and (cl:>= (cl:length hex-str) 2)
                     (cl:string= hex-str "0x" :end1 2))
             ;; Call hex-decode on the full string (it handles the prefix)
             (coalton (hex-decode (lisp String () hex-str)))
             (coalton-prelude:Err
              (HexError "Missing 0x prefix"))))))
