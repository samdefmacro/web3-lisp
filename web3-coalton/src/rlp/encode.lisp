(in-package #:web3/rlp)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; RLP Item ADT
  (define-type RlpItem
    "An RLP-encoded item: either raw bytes or a list of items"
    (RlpBytes types:Bytes)
    (RlpList (List RlpItem)))

  ;;; Internal helpers

  (declare %encode-length (UFix -> U8 -> types:Bytes))
  (define (%encode-length len offset)
    "Encode an RLP length prefix"
    (cond
      ((< len 56)
       ;; Short length: single byte (offset + length)
       (types:bytes-from-list
        (Cons (lisp U8 (len offset) (cl:+ len offset)) Nil)))
      (True
       ;; Long length: offset+55+len-bytes followed by length bytes
       (let ((len-bytes (%integer-to-bytes-be len)))
         (let ((len-of-len (types:bytes-length len-bytes)))
           (types:bytes-append
            (types:bytes-from-list
             (Cons (lisp U8 (len-of-len offset) (cl:+ offset 55 len-of-len)) Nil))
            len-bytes))))))

  (declare %integer-to-bytes-be (UFix -> types:Bytes))
  (define (%integer-to-bytes-be n)
    "Encode a positive integer as big-endian bytes (no leading zeros)"
    (lisp types:Bytes (n)
      (cl:if (cl:zerop n)
             (cl:make-array 0 :fill-pointer 0 :adjustable cl:t)
             (cl:let* ((byte-count (cl:ceiling (cl:integer-length n) 8))
                       (result (cl:make-array byte-count :fill-pointer byte-count
                                                         :adjustable cl:t)))
               (cl:loop :for i :from 0 :below byte-count
                        :do (cl:setf (cl:aref result (cl:- byte-count 1 i))
                                     (cl:ldb (cl:byte 8 (cl:* i 8)) n)))
               result))))

  ;;; Public encoding functions

  (declare rlp-encode (RlpItem -> types:Bytes))
  (define (rlp-encode item)
    "Encode an RLP item to bytes"
    (match item
      ((RlpBytes data)
       (let ((len (types:bytes-length data)))
         (cond
           ;; Single byte in [0x00, 0x7f]: encode as itself
           ((and (== len 1)
                 (lisp Boolean (data) (cl:<= (cl:aref data 0) #x7f)))
            data)
           ;; Otherwise: length-prefixed string
           (True
            (types:bytes-append (%encode-length len #x80) data)))))
      ((RlpList items)
       (let ((encoded-items (map rlp-encode items)))
         (let ((payload (types:bytes-concat-many encoded-items)))
           (types:bytes-append
            (%encode-length (types:bytes-length payload) #xc0)
            payload))))))

  (declare rlp-encode-bytes (types:Bytes -> types:Bytes))
  (define (rlp-encode-bytes data)
    "Convenience: RLP-encode raw bytes"
    (rlp-encode (RlpBytes data)))

  (declare rlp-encode-string (String -> types:Bytes))
  (define (rlp-encode-string s)
    "RLP-encode a UTF-8 string"
    (rlp-encode (RlpBytes
                 (lisp types:Bytes (s)
                   (cl:let* ((octets (cl:map 'cl:vector #'cl:char-code s))
                             (result (cl:make-array (cl:length octets)
                                                    :fill-pointer (cl:length octets)
                                                    :adjustable cl:t
                                                    :initial-contents octets)))
                     result)))))

  (declare rlp-encode-integer (Integer -> types:Bytes))
  (define (rlp-encode-integer n)
    "RLP-encode a non-negative integer"
    (if (== n 0)
        (rlp-encode (RlpBytes (types:bytes-from-list Nil)))
        (rlp-encode (RlpBytes (%integer-to-bytes-be
                               (lisp UFix (n) n)))))))
