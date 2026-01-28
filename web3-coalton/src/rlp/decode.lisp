(in-package #:web3/rlp)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; RLP Decoding per Ethereum Yellow Paper Appendix B

  (declare %decode-be-length (UFix -> UFix -> types:Bytes -> UFix))
  (define (%decode-be-length start len bytes)
    "Decode a big-endian length from bytes"
    (lisp UFix (start len bytes)
      (cl:let ((result 0))
        (cl:loop :for i :from 0 :below len
                 :do (cl:setf result
                              (cl:+ (cl:ash result 8)
                                    (cl:aref bytes (cl:+ start i)))))
        result)))

  (declare rlp-decode (types:Bytes -> (types:Web3Result (Tuple RlpItem types:Bytes))))
  (define (rlp-decode input)
    "Decode an RLP-encoded byte sequence. Returns the decoded item and remaining bytes."
    (let ((input-len (types:bytes-length input)))
      (if (== input-len 0)
          (Err (types:RlpError "Empty input"))
          (let ((prefix (types:bytes-ref-unsafe 0 input)))
            (cond
              ;; Single byte [0x00, 0x7f]
              ((<= prefix 127)
               (Ok (Tuple (RlpBytes (types:bytes-from-list (Cons prefix Nil)))
                          (types:bytes-drop 1 input))))

              ;; Short string [0x80, 0xb7]: length = prefix - 0x80
              ((<= prefix 183)
               (%decode-short-string prefix input input-len))

              ;; Long string [0xb8, 0xbf]: length-of-length = prefix - 0xb7
              ((<= prefix 191)
               (%decode-long-string prefix input input-len))

              ;; Short list [0xc0, 0xf7]: length = prefix - 0xc0
              ((<= prefix 247)
               (%decode-short-list prefix input input-len))

              ;; Long list [0xf8, 0xff]: length-of-length = prefix - 0xf7
              (True
               (%decode-long-list prefix input input-len)))))))

  (declare %decode-short-string (U8 -> types:Bytes -> UFix -> (types:Web3Result (Tuple RlpItem types:Bytes))))
  (define (%decode-short-string prefix input input-len)
    (let ((str-len (lisp UFix (prefix) (cl:- prefix #x80))))
      (if (> (+ str-len 1) input-len)
          (Err (types:RlpError "Short string truncated"))
          (Ok (Tuple (RlpBytes (types:bytes-slice 1 str-len input))
                     (types:bytes-drop (+ 1 str-len) input))))))

  (declare %decode-long-string (U8 -> types:Bytes -> UFix -> (types:Web3Result (Tuple RlpItem types:Bytes))))
  (define (%decode-long-string prefix input input-len)
    (let ((len-of-len (lisp UFix (prefix) (cl:- prefix #xb7))))
      (if (> (+ len-of-len 1) input-len)
          (Err (types:RlpError "Long string length truncated"))
          (let ((str-len (%decode-be-length 1 len-of-len input))
                (data-start (+ 1 len-of-len)))
            (if (> (+ data-start str-len) input-len)
                (Err (types:RlpError "Long string data truncated"))
                (Ok (Tuple (RlpBytes (types:bytes-slice data-start str-len input))
                           (types:bytes-drop (+ data-start str-len) input))))))))

  (declare %decode-short-list (U8 -> types:Bytes -> UFix -> (types:Web3Result (Tuple RlpItem types:Bytes))))
  (define (%decode-short-list prefix input input-len)
    (let ((list-len (lisp UFix (prefix) (cl:- prefix #xc0))))
      (if (> (+ list-len 1) input-len)
          (Err (types:RlpError "Short list truncated"))
          (let ((list-data (types:bytes-slice 1 list-len input)))
            (match (%decode-list-items list-data)
              ((Err e) (Err e))
              ((Ok items)
               (Ok (Tuple (RlpList items)
                          (types:bytes-drop (+ 1 list-len) input)))))))))

  (declare %decode-long-list (U8 -> types:Bytes -> UFix -> (types:Web3Result (Tuple RlpItem types:Bytes))))
  (define (%decode-long-list prefix input input-len)
    (let ((len-of-len (lisp UFix (prefix) (cl:- prefix #xf7))))
      (if (> (+ len-of-len 1) input-len)
          (Err (types:RlpError "Long list length truncated"))
          (let ((list-len (%decode-be-length 1 len-of-len input))
                (data-start (+ 1 len-of-len)))
            (if (> (+ data-start list-len) input-len)
                (Err (types:RlpError "Long list data truncated"))
                (let ((list-data (types:bytes-slice data-start list-len input)))
                  (match (%decode-list-items list-data)
                    ((Err e) (Err e))
                    ((Ok items)
                     (Ok (Tuple (RlpList items)
                                (types:bytes-drop (+ data-start list-len) input)))))))))))

  (declare %decode-list-items (types:Bytes -> (types:Web3Result (List RlpItem))))
  (define (%decode-list-items data)
    "Decode all items from list payload bytes"
    (if (== (types:bytes-length data) 0)
        (Ok Nil)
        (match (rlp-decode data)
          ((Err e) (Err e))
          ((Ok (Tuple item rest))
           (match (%decode-list-items rest)
             ((Err e) (Err e))
             ((Ok more-items)
              (Ok (Cons item more-items)))))))))
