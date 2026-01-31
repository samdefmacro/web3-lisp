(in-package #:web3/crypto)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; ECDSA Signature type

  (define-type Signature
    "ECDSA signature with r, s, and recovery id v"
    (%Signature types:Bytes   ; r (32 bytes)
                types:Bytes   ; s (32 bytes)
                U8))          ; v (recovery id: 0 or 1, or 27/28 for legacy)

  (declare make-signature (types:Bytes -> types:Bytes -> U8 -> Signature))
  (define (make-signature r s v)
    "Create a Signature from r, s, v components"
    (%Signature r s v))

  (declare signature-r (Signature -> types:Bytes))
  (define (signature-r sig)
    (match sig ((%Signature r _ _) r)))

  (declare signature-s (Signature -> types:Bytes))
  (define (signature-s sig)
    (match sig ((%Signature _ s _) s)))

  (declare signature-v (Signature -> U8))
  (define (signature-v sig)
    (match sig ((%Signature _ _ v) v)))

  (declare signature-to-bytes (Signature -> types:Bytes))
  (define (signature-to-bytes sig)
    "Convert signature to 65-byte encoding (r || s || v)"
    (types:bytes-concat-many
     (Cons (types:bytes-pad-left 32 (signature-r sig))
           (Cons (types:bytes-pad-left 32 (signature-s sig))
                 (Cons (types:bytes-from-list (Cons (signature-v sig) Nil))
                       Nil)))))

  (declare signature-from-bytes (types:Bytes -> (types:Web3Result Signature)))
  (define (signature-from-bytes bytes)
    "Parse a 65-byte signature (r || s || v)"
    (if (/= (types:bytes-length bytes) 65)
        (Err (types:CryptoError "Signature must be 65 bytes"))
        (let ((r (types:bytes-slice 0 32 bytes))
              (s (types:bytes-slice 32 32 bytes))
              (v (types:bytes-ref-unsafe 64 bytes)))
          (Ok (make-signature r s v))))))

;;; CL helper for ecrecover via brute-force
;;; We try both recovery IDs (0 and 1) and return the one that matches
(cl:defun web3/crypto::%ec-recover-public-key (hash-bytes r-bytes s-bytes recovery-id)
  "Recover the uncompressed public key from an ECDSA signature.
   This is a simplified implementation using secp256k1 curve math."
  ;; secp256k1 curve parameters
  (cl:let* ((p #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F)
            (n #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141)
            ;; Convert r, s, hash to integers
            (r-int (cl:let ((val 0))
                     (cl:loop :for i :from 0 :below 32
                              :do (cl:setf val (cl:+ (cl:ash val 8) (cl:aref r-bytes i))))
                     val))
            (s-int (cl:let ((val 0))
                     (cl:loop :for i :from 0 :below 32
                              :do (cl:setf val (cl:+ (cl:ash val 8) (cl:aref s-bytes i))))
                     val))
            (e-int (cl:let ((val 0))
                     (cl:loop :for i :from 0 :below 32
                              :do (cl:setf val (cl:+ (cl:ash val 8) (cl:aref hash-bytes i))))
                     val))
            ;; Compute r^-1 mod n
            (r-inv (%mod-inverse r-int n))
            ;; Recover the point R from r
            (x r-int)
            ;; y^2 = x^3 + 7 (mod p)
            (y-sq (cl:mod (cl:+ (cl:mod (cl:expt x 3) p) 7) p))
            (y (%mod-sqrt y-sq p)))
    ;; Select the correct y based on recovery-id parity
    (cl:when (cl:/= (cl:logand y 1) recovery-id)
      (cl:setf y (cl:- p y)))
    ;; R = (x, y) point on curve
    ;; Public key Q = r^-1 * (s*R - e*G)
    (cl:let* ((r-point (ironclad:ec-decode-point :secp256k1
                         (%encode-uncompressed-point x y)))
              (g-point (%get-secp256k1-generator))
              ;; s*R
              (sr (ironclad:ec-scalar-mult r-point s-int))
              ;; (n-e)*G  (negating e*G by computing (n-e)*G)
              (neg-eg (ironclad:ec-scalar-mult g-point (cl:- n e-int)))
              ;; s*R + (n-e)*G = s*R - e*G
              (sum-point (ironclad:ec-add sr neg-eg))
              ;; r^-1 * (s*R - e*G)
              (q-point (ironclad:ec-scalar-mult sum-point r-inv))
              (encoded (ironclad:ec-encode-point q-point)))
      encoded)))

(cl:defun web3/crypto::%mod-inverse (a n)
  "Compute modular inverse of a mod n using extended Euclidean algorithm"
  (cl:let ((old-r n) (r (cl:mod a n))
           (old-s 0) (s 1))
    (cl:loop :while (cl:plusp r)
             :do (cl:let ((quotient (cl:floor old-r r)))
                   (cl:psetf old-r r r (cl:- old-r (cl:* quotient r)))
                   (cl:psetf old-s s s (cl:- old-s (cl:* quotient s)))))
    (cl:mod old-s n)))

(cl:defun web3/crypto::%mod-expt (base exp modulus)
  "Compute (base^exp) mod modulus using fast modular exponentiation"
  (cl:let ((result 1)
           (b (cl:mod base modulus))
           (e exp))
    (cl:loop :while (cl:plusp e)
             :do (cl:when (cl:oddp e)
                   (cl:setf result (cl:mod (cl:* result b) modulus)))
                 (cl:setf e (cl:ash e -1))
                 (cl:setf b (cl:mod (cl:* b b) modulus)))
    result))

(cl:defun web3/crypto::%mod-sqrt (a p)
  "Compute modular square root (for p ≡ 3 mod 4, use a^((p+1)/4) mod p)"
  (%mod-expt a (cl:/ (cl:+ p 1) 4) p))

(cl:defun web3/crypto::%encode-uncompressed-point (x y)
  "Encode x,y coordinates as 65-byte uncompressed point (04 || x || y)"
  (cl:let ((result (cl:make-array 65 :element-type '(cl:unsigned-byte 8))))
    (cl:setf (cl:aref result 0) 4)
    (cl:loop :for i :from 0 :below 32
             :do (cl:setf (cl:aref result (cl:+ 1 i))
                          (cl:ldb (cl:byte 8 (cl:* (cl:- 31 i) 8)) x)))
    (cl:loop :for i :from 0 :below 32
             :do (cl:setf (cl:aref result (cl:+ 33 i))
                          (cl:ldb (cl:byte 8 (cl:* (cl:- 31 i) 8)) y)))
    result))

(cl:defun web3/crypto::%get-secp256k1-generator ()
  "Get the secp256k1 generator point G"
  ;; Derive G by computing pubkey for scalar 1
  (cl:let* ((one-bytes (cl:make-array 32 :element-type '(cl:unsigned-byte 8) :initial-element 0)))
    (cl:setf (cl:aref one-bytes 31) 1)
    (cl:let* ((sk (ironclad:make-private-key :secp256k1 :x one-bytes))
              (pub-bytes (ironclad:secp256k1-key-y sk)))
      (ironclad:ec-decode-point :secp256k1 pub-bytes))))

;;; Back to Coalton for sign-hash and recover
(coalton-toplevel

  (declare sign-hash (types:Bytes -> types:Bytes -> (types:Web3Result Signature)))
  (define (sign-hash hash-bytes private-key)
    "Sign a 32-byte hash with a 32-byte private key using secp256k1.
     Returns a Signature with r, s, and recovery id v (0 or 1)."
    (if (/= (types:bytes-length hash-bytes) 32)
        (Err (types:CryptoError "Hash must be 32 bytes"))
        (if (/= (types:bytes-length private-key) 32)
            (Err (types:CryptoError "Private key must be 32 bytes"))
            (lisp (types:Web3Result Signature) (hash-bytes private-key)
              (cl:handler-case
                  (cl:let* ((hash-vec (cl:make-array 32 :element-type '(cl:unsigned-byte 8)
                                                        :initial-contents hash-bytes))
                            (pk-vec (cl:make-array 32 :element-type '(cl:unsigned-byte 8)
                                                      :initial-contents private-key))
                            (sk (ironclad:make-private-key :secp256k1 :x pk-vec))
                            ;; ironclad returns 64-byte signature: r (32) || s (32)
                            (sig-bytes (ironclad:sign-message sk hash-vec))
                            (r-bytes (cl:make-array 32 :fill-pointer 32 :adjustable cl:t))
                            (s-bytes (cl:make-array 32 :fill-pointer 32 :adjustable cl:t)))
                    (cl:loop :for i :from 0 :below 32
                             :do (cl:setf (cl:aref r-bytes i) (cl:aref sig-bytes i))
                                 (cl:setf (cl:aref s-bytes i) (cl:aref sig-bytes (cl:+ 32 i))))
                    ;; Determine recovery id by trying both v=0 and v=1
                    (cl:let* ((pub-uncompressed (ironclad:secp256k1-key-y sk))
                              (recovery-id
                                (cl:block find-v
                                  (cl:loop :for v :from 0 :to 1
                                           :do (cl:handler-case
                                                   (cl:let* ((recovered (%ec-recover-public-key
                                                                         hash-vec r-bytes s-bytes v)))
                                                     (cl:when (cl:equalp recovered pub-uncompressed)
                                                       (cl:return-from find-v v)))
                                                 (cl:error () cl:nil)))
                                  0)))
                      (coalton-prelude:Ok
                       (%Signature r-bytes s-bytes recovery-id))))
                (cl:error (e)
                  (coalton-prelude:Err
                   (types:CryptoError (cl:format cl:nil "Signing error: ~A" e)))))))))

  (declare recover-public-key (types:Bytes -> Signature -> (types:Web3Result types:Bytes)))
  (define (recover-public-key hash-bytes sig)
    "Recover the uncompressed public key from a hash and signature"
    (if (/= (types:bytes-length hash-bytes) 32)
        (Err (types:CryptoError "Hash must be 32 bytes"))
        (let ((r-bytes (signature-r sig))
              (s-bytes (signature-s sig))
              (v (signature-v sig)))
          (lisp (types:Web3Result types:Bytes) (hash-bytes r-bytes s-bytes v)
            (cl:handler-case
                (cl:let* ((hash-vec (cl:make-array 32 :element-type '(cl:unsigned-byte 8)
                                                      :initial-contents hash-bytes))
                          (r-vec (cl:make-array 32 :element-type '(cl:unsigned-byte 8)
                                                   :initial-contents r-bytes))
                          (s-vec (cl:make-array 32 :element-type '(cl:unsigned-byte 8)
                                                   :initial-contents s-bytes))
                          ;; Normalize v: 27/28 -> 0/1
                          (recovery-id (cl:if (cl:>= v 27) (cl:- v 27) v))
                          (recovered (%ec-recover-public-key hash-vec r-vec s-vec recovery-id))
                          (result (cl:make-array 65 :fill-pointer 65 :adjustable cl:t)))
                  (cl:loop :for i :from 0 :below 65
                           :do (cl:setf (cl:aref result i) (cl:aref recovered i)))
                  (coalton-prelude:Ok result))
              (cl:error (e)
                (coalton-prelude:Err
                 (types:CryptoError (cl:format cl:nil "Recovery error: ~A" e))))))))))
