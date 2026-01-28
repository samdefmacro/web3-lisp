(in-package #:web3/crypto)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; secp256k1 elliptic curve operations via ironclad

  (declare private-key-to-public-key (types:Bytes -> (types:Web3Result types:Bytes)))
  (define (private-key-to-public-key private-key)
    "Derive the 65-byte uncompressed public key from a 32-byte private key.
     Returns 04 || x (32 bytes) || y (32 bytes)."
    (if (/= (types:bytes-length private-key) 32)
        (Err (types:CryptoError "Private key must be 32 bytes"))
        (lisp (types:Web3Result types:Bytes) (private-key)
          (cl:handler-case
              (cl:let* ((pk-vec (cl:make-array 32 :element-type '(cl:unsigned-byte 8)
                                                  :initial-contents private-key))
                        (sk (ironclad:make-private-key :secp256k1 :x pk-vec))
                        ;; secp256k1-key-y on a private key returns 65-byte uncompressed pubkey
                        (pub-uncompressed (ironclad:secp256k1-key-y sk))
                        (result (cl:make-array 65 :fill-pointer 65 :adjustable cl:t)))
                (cl:loop :for i :from 0 :below 65
                         :do (cl:setf (cl:aref result i) (cl:aref pub-uncompressed i)))
                (coalton-prelude:Ok result))
            (cl:error (e)
              (coalton-prelude:Err
               (types:CryptoError (cl:format cl:nil "Key derivation error: ~A" e))))))))

  (declare public-key-to-uncompressed (types:Bytes -> (types:Web3Result types:Bytes)))
  (define (public-key-to-uncompressed pub-key)
    "Convert a public key to 65-byte uncompressed format.
     Accepts uncompressed (65 bytes, starts with 04),
     compressed (33 bytes, starts with 02/03),
     or raw (64 bytes, no prefix)."
    (let ((len (types:bytes-length pub-key)))
      (cond
        ;; Already uncompressed (65 bytes, 04 prefix)
        ((and (== len 65) (== (types:bytes-ref-unsafe 0 pub-key) 4))
         (Ok (types:bytes-copy pub-key)))

        ;; Raw 64 bytes (no prefix) - add 04 prefix
        ((== len 64)
         (Ok (types:bytes-append
              (types:bytes-from-list (Cons 4 Nil))
              pub-key)))

        ;; Compressed (33 bytes, 02 or 03 prefix)
        ((and (== len 33)
              (or (== (types:bytes-ref-unsafe 0 pub-key) 2)
                  (== (types:bytes-ref-unsafe 0 pub-key) 3)))
         ;; Decompress using ironclad
         (lisp (types:Web3Result types:Bytes) (pub-key)
           (cl:handler-case
               (cl:let* ((compressed (cl:make-array 33 :element-type '(cl:unsigned-byte 8)
                                                       :initial-contents pub-key))
                         (point (ironclad:ec-decode-point :secp256k1 compressed))
                         ;; ec-encode-point takes 1 arg in this ironclad version
                         (uncompressed (ironclad:ec-encode-point point))
                         (result (cl:make-array 65 :fill-pointer 65 :adjustable cl:t)))
                 (cl:loop :for i :from 0 :below 65
                          :do (cl:setf (cl:aref result i) (cl:aref uncompressed i)))
                 (coalton-prelude:Ok result))
             (cl:error (e)
               (coalton-prelude:Err
                (types:CryptoError (cl:format cl:nil "Decompression error: ~A" e)))))))

        (True
         (Err (types:CryptoError
               (lisp String (len)
                 (cl:format cl:nil "Invalid public key length: ~A" len)))))))))
