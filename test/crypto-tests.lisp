;;; Crypto module tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Keccak256 Tests
;;; =========================================================================

(defun run-crypto-tests ()
  (format t "~%=== Crypto Tests ===~%")

  (test-case "keccak256 empty input"
    (assert (eq (web3-tests:test-keccak256-empty coalton:Unit) coalton:True)))

  (test-case "keccak256 'hello'"
    (assert (eq (web3-tests:test-keccak256-hello coalton:Unit) coalton:True)))

  (test-case "keccak256 produces 32 bytes"
    (let ((result (coalton:coalton
                   (web3/crypto:keccak256 (web3/types:make-bytes 100)))))
      (assert (= (length result) 32))))

  (test-case "keccak256 empty - full hash (direct CL)"
    ;; keccak256("") = c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470
    (let ((result (coalton:coalton
                   (web3/crypto:keccak256 (web3/types:bytes-empty coalton:Unit)))))
      (assert (= (aref result 0) #xc5))
      (assert (= (aref result 1) #xd2))
      (assert (= (aref result 2) #x46))
      (assert (= (aref result 3) #x01))
      (assert (= (aref result 31) #x70))))

  (test-case "keccak256 deterministic"
    (let ((h1 (coalton:coalton
               (web3/crypto:keccak256
                (web3/types:bytes-from-list (coalton:Cons 1 (coalton:Cons 2 coalton:Nil))))))
          (h2 (coalton:coalton
               (web3/crypto:keccak256
                (web3/types:bytes-from-list (coalton:Cons 1 (coalton:Cons 2 coalton:Nil)))))))
      (assert (bytes-equal h1 h2))))

  (test-case "keccak256 different inputs produce different hashes"
    (let ((h1 (coalton:coalton
               (web3/crypto:keccak256
                (web3/types:bytes-from-list (coalton:Cons 1 coalton:Nil)))))
          (h2 (coalton:coalton
               (web3/crypto:keccak256
                (web3/types:bytes-from-list (coalton:Cons 2 coalton:Nil))))))
      (assert (not (bytes-equal h1 h2)))))

  ;;; =========================================================================
  ;;; secp256k1 Key Derivation Tests
  ;;; =========================================================================

  (test-case "private-key-to-public-key produces 65-byte uncompressed key"
    ;; Use a well-known test private key (all 1s)
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (result (coalton:coalton
                    (web3/crypto:private-key-to-public-key
                     (coalton:lisp web3/types:Bytes () pk)))))
      (assert (result-ok-p result))
      (let ((pubkey (result-value result)))
        ;; Uncompressed pubkey is 65 bytes starting with 0x04
        (assert (= (length pubkey) 65))
        (assert (= (aref pubkey 0) 4)))))

  (test-case "private-key-to-public-key rejects invalid length"
    ;; 31 bytes is invalid
    (let* ((pk (make-array 31 :fill-pointer 31 :adjustable t :initial-element 1))
           (result (coalton:coalton
                    (web3/crypto:private-key-to-public-key
                     (coalton:lisp web3/types:Bytes () pk)))))
      (assert (result-err-p result))))

  (test-case "private-key-to-public-key deterministic"
    ;; Same private key should produce same public key
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element #x42))
           (result1 (coalton:coalton
                     (web3/crypto:private-key-to-public-key
                      (coalton:lisp web3/types:Bytes () pk))))
           (result2 (coalton:coalton
                     (web3/crypto:private-key-to-public-key
                      (coalton:lisp web3/types:Bytes () pk)))))
      (assert (result-ok-p result1))
      (assert (result-ok-p result2))
      (assert (bytes-equal (result-value result1) (result-value result2)))))

  (test-case "different private keys produce different public keys"
    (let* ((pk1 (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (pk2 (make-array 32 :fill-pointer 32 :adjustable t :initial-element 2))
           (result1 (coalton:coalton
                     (web3/crypto:private-key-to-public-key
                      (coalton:lisp web3/types:Bytes () pk1))))
           (result2 (coalton:coalton
                     (web3/crypto:private-key-to-public-key
                      (coalton:lisp web3/types:Bytes () pk2)))))
      (assert (result-ok-p result1))
      (assert (result-ok-p result2))
      (assert (not (bytes-equal (result-value result1) (result-value result2))))))

  ;;; =========================================================================
  ;;; Public Key Format Conversion Tests
  ;;; =========================================================================

  (test-case "public-key-to-uncompressed with 65-byte key (already uncompressed)"
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (pubkey-result (coalton:coalton
                           (web3/crypto:private-key-to-public-key
                            (coalton:lisp web3/types:Bytes () pk))))
           (pubkey (result-value pubkey-result))
           (result (coalton:coalton
                    (web3/crypto:public-key-to-uncompressed
                     (coalton:lisp web3/types:Bytes () pubkey)))))
      (assert (result-ok-p result))
      (let ((converted (result-value result)))
        (assert (= (length converted) 65))
        (assert (bytes-equal pubkey converted)))))

  (test-case "public-key-to-uncompressed with 64-byte raw key"
    ;; 64 bytes = raw x,y coordinates without 0x04 prefix
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (pubkey-result (coalton:coalton
                           (web3/crypto:private-key-to-public-key
                            (coalton:lisp web3/types:Bytes () pk))))
           (pubkey (result-value pubkey-result))
           ;; Extract just the 64 bytes (skip the 04 prefix)
           (raw-64 (make-array 64 :fill-pointer 64 :adjustable t)))
      (loop :for i :from 0 :below 64
            :do (setf (aref raw-64 i) (aref pubkey (1+ i))))
      (let ((result (coalton:coalton
                     (web3/crypto:public-key-to-uncompressed
                      (coalton:lisp web3/types:Bytes () raw-64)))))
        (assert (result-ok-p result))
        (let ((converted (result-value result)))
          (assert (= (length converted) 65))
          (assert (= (aref converted 0) 4))
          ;; Rest should match original pubkey
          (assert (bytes-equal pubkey converted))))))

  (test-case "public-key-to-uncompressed rejects invalid length"
    (let* ((invalid (make-array 50 :fill-pointer 50 :adjustable t :initial-element 0))
           (result (coalton:coalton
                    (web3/crypto:public-key-to-uncompressed
                     (coalton:lisp web3/types:Bytes () invalid)))))
      (assert (result-err-p result))))

  ;;; =========================================================================
  ;;; Signature Creation and Serialization Tests
  ;;; =========================================================================

  (test-case "signature-to-bytes produces 65 bytes"
    (let* ((r (make-array 32 :fill-pointer 32 :adjustable t :initial-element #xaa))
           (s (make-array 32 :fill-pointer 32 :adjustable t :initial-element #xbb))
           (sig-bytes (coalton:coalton
                       (web3/crypto:signature-to-bytes
                        (web3/crypto:make-signature
                         (coalton:lisp web3/types:Bytes () r)
                         (coalton:lisp web3/types:Bytes () s)
                         27)))))
      (assert (= (length sig-bytes) 65))
      ;; First 32 bytes should be r
      (assert (= (aref sig-bytes 0) #xaa))
      (assert (= (aref sig-bytes 31) #xaa))
      ;; Next 32 bytes should be s
      (assert (= (aref sig-bytes 32) #xbb))
      (assert (= (aref sig-bytes 63) #xbb))
      ;; Last byte is v
      (assert (= (aref sig-bytes 64) 27))))

  (test-case "signature-from-bytes roundtrip"
    (let* ((r (make-array 32 :fill-pointer 32 :adjustable t :initial-element #x11))
           (s (make-array 32 :fill-pointer 32 :adjustable t :initial-element #x22))
           (original (coalton:coalton
                      (web3/crypto:make-signature
                       (coalton:lisp web3/types:Bytes () r)
                       (coalton:lisp web3/types:Bytes () s)
                       1)))
           (serialized (coalton:coalton
                        (web3/crypto:signature-to-bytes
                         (coalton:lisp web3/crypto:Signature () original))))
           (parsed (coalton:coalton
                    (web3/crypto:signature-from-bytes
                     (coalton:lisp web3/types:Bytes () serialized)))))
      (assert (result-ok-p parsed))
      (let* ((sig (result-value parsed))
             (r2 (coalton:coalton
                  (web3/crypto:signature-r
                   (coalton:lisp web3/crypto:Signature () sig))))
             (s2 (coalton:coalton
                  (web3/crypto:signature-s
                   (coalton:lisp web3/crypto:Signature () sig))))
             (v2 (coalton:coalton
                  (web3/crypto:signature-v
                   (coalton:lisp web3/crypto:Signature () sig)))))
        (assert (bytes-equal r r2))
        (assert (bytes-equal s s2))
        (assert (= v2 1)))))

  (test-case "signature-from-bytes rejects invalid length"
    (let* ((invalid (make-array 64 :fill-pointer 64 :adjustable t :initial-element 0))
           (result (coalton:coalton
                    (web3/crypto:signature-from-bytes
                     (coalton:lisp web3/types:Bytes () invalid)))))
      (assert (result-err-p result))))

  ;;; =========================================================================
  ;;; Sign and Recover Tests
  ;;; =========================================================================

  (test-case "sign-hash produces valid signature"
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           ;; Create a hash to sign (use keccak256 of some data)
           (hash (coalton:coalton
                  (web3/crypto:keccak256
                   (web3/types:bytes-from-list
                    (coalton:Cons #x68 (coalton:Cons #x65 (coalton:Cons #x6c
                     (coalton:Cons #x6c (coalton:Cons #x6f coalton:Nil)))))))))
           (sig-result (coalton:coalton
                        (web3/crypto:sign-hash
                         (coalton:lisp web3/types:Bytes () hash)
                         (coalton:lisp web3/types:Bytes () pk)))))
      (assert (result-ok-p sig-result))
      (let* ((sig (result-value sig-result))
             (r (coalton:coalton
                 (web3/crypto:signature-r
                  (coalton:lisp web3/crypto:Signature () sig))))
             (s (coalton:coalton
                 (web3/crypto:signature-s
                  (coalton:lisp web3/crypto:Signature () sig))))
             (v (coalton:coalton
                 (web3/crypto:signature-v
                  (coalton:lisp web3/crypto:Signature () sig)))))
        ;; r and s should be 32 bytes each
        (assert (= (length r) 32))
        (assert (= (length s) 32))
        ;; v should be 0 or 1 (recovery id)
        (assert (or (= v 0) (= v 1))))))

  (test-case "sign-hash rejects invalid hash length"
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (bad-hash (make-array 31 :fill-pointer 31 :adjustable t :initial-element 0))
           (result (coalton:coalton
                    (web3/crypto:sign-hash
                     (coalton:lisp web3/types:Bytes () bad-hash)
                     (coalton:lisp web3/types:Bytes () pk)))))
      (assert (result-err-p result))))

  (test-case "sign-hash rejects invalid private key length"
    (let* ((bad-pk (make-array 31 :fill-pointer 31 :adjustable t :initial-element 1))
           (hash (make-array 32 :fill-pointer 32 :adjustable t :initial-element 0))
           (result (coalton:coalton
                    (web3/crypto:sign-hash
                     (coalton:lisp web3/types:Bytes () hash)
                     (coalton:lisp web3/types:Bytes () bad-pk)))))
      (assert (result-err-p result))))

  (test-case "sign-hash multiple signatures both valid"
    ;; ECDSA may use random nonces, so different signatures can be valid
    ;; Test that both signatures are valid by recovering the same public key
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element #x42))
           (pubkey (result-value (coalton:coalton
                                  (web3/crypto:private-key-to-public-key
                                   (coalton:lisp web3/types:Bytes () pk)))))
           (hash (make-array 32 :fill-pointer 32 :adjustable t :initial-element #x99))
           (sig1 (result-value (coalton:coalton
                                (web3/crypto:sign-hash
                                 (coalton:lisp web3/types:Bytes () hash)
                                 (coalton:lisp web3/types:Bytes () pk)))))
           (sig2 (result-value (coalton:coalton
                                (web3/crypto:sign-hash
                                 (coalton:lisp web3/types:Bytes () hash)
                                 (coalton:lisp web3/types:Bytes () pk)))))
           ;; Both signatures should recover to the same public key
           (recovered1 (result-value
                        (coalton:coalton
                         (web3/crypto:recover-public-key
                          (coalton:lisp web3/types:Bytes () hash)
                          (coalton:lisp web3/crypto:Signature () sig1)))))
           (recovered2 (result-value
                        (coalton:coalton
                         (web3/crypto:recover-public-key
                          (coalton:lisp web3/types:Bytes () hash)
                          (coalton:lisp web3/crypto:Signature () sig2))))))
      ;; Both should recover to the original public key
      (assert (bytes-equal pubkey recovered1))
      (assert (bytes-equal pubkey recovered2))))

  (test-case "recover-public-key recovers correct key"
    ;; Sign a message and recover the public key - it should match the original
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           ;; Get the public key for this private key
           (pubkey-result (coalton:coalton
                           (web3/crypto:private-key-to-public-key
                            (coalton:lisp web3/types:Bytes () pk))))
           (expected-pubkey (result-value pubkey-result))
           ;; Create a hash and sign it
           (hash (coalton:coalton
                  (web3/crypto:keccak256
                   (web3/types:bytes-from-list
                    (coalton:Cons 1 (coalton:Cons 2 (coalton:Cons 3 coalton:Nil)))))))
           (sig-result (coalton:coalton
                        (web3/crypto:sign-hash
                         (coalton:lisp web3/types:Bytes () hash)
                         (coalton:lisp web3/types:Bytes () pk))))
           (sig (result-value sig-result))
           ;; Recover the public key from the signature
           (recovered-result (coalton:coalton
                              (web3/crypto:recover-public-key
                               (coalton:lisp web3/types:Bytes () hash)
                               (coalton:lisp web3/crypto:Signature () sig)))))
      (assert (result-ok-p recovered-result))
      (let ((recovered-pubkey (result-value recovered-result)))
        (assert (= (length recovered-pubkey) 65))
        (assert (bytes-equal expected-pubkey recovered-pubkey)))))

  (test-case "recover-public-key with different private keys"
    ;; Different keys produce different recovered public keys
    (let* ((pk1 (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (pk2 (make-array 32 :fill-pointer 32 :adjustable t :initial-element 2))
           (hash (make-array 32 :fill-pointer 32 :adjustable t :initial-element #x55))
           (sig1 (result-value (coalton:coalton
                                (web3/crypto:sign-hash
                                 (coalton:lisp web3/types:Bytes () hash)
                                 (coalton:lisp web3/types:Bytes () pk1)))))
           (sig2 (result-value (coalton:coalton
                                (web3/crypto:sign-hash
                                 (coalton:lisp web3/types:Bytes () hash)
                                 (coalton:lisp web3/types:Bytes () pk2)))))
           (recovered1 (result-value
                        (coalton:coalton
                         (web3/crypto:recover-public-key
                          (coalton:lisp web3/types:Bytes () hash)
                          (coalton:lisp web3/crypto:Signature () sig1)))))
           (recovered2 (result-value
                        (coalton:coalton
                         (web3/crypto:recover-public-key
                          (coalton:lisp web3/types:Bytes () hash)
                          (coalton:lisp web3/crypto:Signature () sig2))))))
      (assert (not (bytes-equal recovered1 recovered2)))))

  (test-case "recover-public-key rejects invalid hash length"
    (let* ((bad-hash (make-array 31 :fill-pointer 31 :adjustable t :initial-element 0))
           (r (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (s (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (sig (coalton:coalton
                 (web3/crypto:make-signature
                  (coalton:lisp web3/types:Bytes () r)
                  (coalton:lisp web3/types:Bytes () s)
                  0)))
           (result (coalton:coalton
                    (web3/crypto:recover-public-key
                     (coalton:lisp web3/types:Bytes () bad-hash)
                     (coalton:lisp web3/crypto:Signature () sig)))))
      (assert (result-err-p result))))

  (test-case "sign and recover roundtrip with v=27/28 normalization"
    ;; Test that recovery handles both v=0/1 and v=27/28 formats
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element #xab))
           (pubkey (result-value (coalton:coalton
                                  (web3/crypto:private-key-to-public-key
                                   (coalton:lisp web3/types:Bytes () pk)))))
           (hash (make-array 32 :fill-pointer 32 :adjustable t :initial-element #xcd))
           (sig-result (coalton:coalton
                        (web3/crypto:sign-hash
                         (coalton:lisp web3/types:Bytes () hash)
                         (coalton:lisp web3/types:Bytes () pk))))
           (sig (result-value sig-result))
           ;; Get signature components
           (r (coalton:coalton
               (web3/crypto:signature-r
                (coalton:lisp web3/crypto:Signature () sig))))
           (s (coalton:coalton
               (web3/crypto:signature-s
                (coalton:lisp web3/crypto:Signature () sig))))
           (v (coalton:coalton
               (web3/crypto:signature-v
                (coalton:lisp web3/crypto:Signature () sig))))
           ;; Create signature with v+27 (legacy Ethereum format)
           (sig-legacy (coalton:coalton
                        (web3/crypto:make-signature
                         (coalton:lisp web3/types:Bytes () r)
                         (coalton:lisp web3/types:Bytes () s)
                         (coalton:lisp coalton:U8 () (+ v 27)))))
           ;; Recover from legacy signature
           (recovered (result-value
                       (coalton:coalton
                        (web3/crypto:recover-public-key
                         (coalton:lisp web3/types:Bytes () hash)
                         (coalton:lisp web3/crypto:Signature () sig-legacy))))))
      (assert (bytes-equal pubkey recovered)))))
