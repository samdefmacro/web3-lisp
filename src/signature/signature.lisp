;;;; Signature Utils implementation
;;;; EIP-191 personal sign, signature recovery, and verification

(in-package #:web3/signature)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Re-export Signature type from crypto module
  ;;; =========================================================================

  ;; Use crypto module's Signature type directly
  (define-type-alias Signature crypto:Signature)

  (declare make-signature (types:Bytes -> types:Bytes -> U8 -> Signature))
  (define (make-signature r s v)
    "Create a Signature from r, s, v components"
    (crypto:make-signature r s v))

  (declare sig-r (Signature -> types:Bytes))
  (define (sig-r sig)
    "Get the r component of the signature"
    (crypto:.signature-r sig))

  (declare sig-s (Signature -> types:Bytes))
  (define (sig-s sig)
    "Get the s component of the signature"
    (crypto:.signature-s sig))

  (declare sig-v (Signature -> U8))
  (define (sig-v sig)
    "Get the v component (recovery id) of the signature"
    (crypto:.signature-v sig))

  ;;; =========================================================================
  ;;; Signature Encoding
  ;;; =========================================================================

  (declare signature-to-bytes (Signature -> types:Bytes))
  (define (signature-to-bytes sig)
    "Convert signature to 65-byte encoding (r || s || v)"
    (crypto:signature-to-bytes sig))

  (declare signature-from-bytes (types:Bytes -> (types:Web3Result Signature)))
  (define (signature-from-bytes bytes)
    "Parse a 65-byte signature (r || s || v)"
    (crypto:signature-from-bytes bytes))

  (declare signature-to-hex (Signature -> String))
  (define (signature-to-hex sig)
    "Convert signature to hex string with 0x prefix"
    (types:hex-encode-prefixed (signature-to-bytes sig)))

  (declare signature-from-hex (String -> (types:Web3Result Signature)))
  (define (signature-from-hex hex-str)
    "Parse signature from hex string"
    (match (types:hex-decode hex-str)
      ((Err e) (Err e))
      ((Ok bytes) (signature-from-bytes bytes))))

  ;;; =========================================================================
  ;;; V value normalization
  ;;; =========================================================================

  (declare normalize-v (UFix -> U8))
  (define (normalize-v v)
    "Normalize v to 0 or 1 (from 27/28 or EIP-155 format)"
    (cond
      ;; Already normalized
      ((< v 2) (lisp U8 (v) v))
      ;; Legacy format (27/28)
      ((< v 35) (lisp U8 (v) (cl:- v 27)))
      ;; EIP-155 format (chain-id * 2 + 35 + recovery-id)
      (True (if (== (mod v 2) 0) 0 1))))

  (declare to-eip155-v (U8 -> UFix -> UFix))
  (define (to-eip155-v recovery-id chain-id)
    "Convert recovery-id (0/1) to EIP-155 v value"
    (lisp UFix (recovery-id chain-id)
      (cl:+ (cl:* chain-id 2) 35 recovery-id)))

  (declare from-eip155-v (UFix -> UFix -> U8))
  (define (from-eip155-v v chain-id)
    "Extract recovery-id (0/1) from EIP-155 v value"
    (lisp U8 (v chain-id)
      (cl:let ((base (cl:+ (cl:* chain-id 2) 35)))
        (cl:- v base))))

  ;;; =========================================================================
  ;;; Split and Join utilities
  ;;; =========================================================================

  (declare split-signature (Signature -> (Tuple types:Bytes (Tuple types:Bytes U8))))
  (define (split-signature sig)
    "Split signature into (r, (s, v)) nested tuple"
    (Tuple (sig-r sig) (Tuple (sig-s sig) (sig-v sig))))

  (declare join-signature (types:Bytes -> types:Bytes -> U8 -> Signature))
  (define (join-signature r s v)
    "Join r, s, v into a signature"
    (make-signature r s v))

  ;;; =========================================================================
  ;;; EIP-191 Personal Sign
  ;;; =========================================================================

  (declare personal-sign-prefix String)
  (define personal-sign-prefix
    "The Ethereum signed message prefix"
    "\x19Ethereum Signed Message:\n")

  (declare hash-personal-message (types:Bytes -> types:Bytes))
  (define (hash-personal-message message)
    "Hash a message with the EIP-191 personal sign prefix.
     Returns keccak256(prefix || length || message)"
    (let ((prefix-str personal-sign-prefix)
          (msg-len (types:bytes-length message)))
      ;; Build: prefix || decimal_length || message
      (let ((len-str (ufix-to-string msg-len)))
        (let ((prefix-bytes (types:string-to-bytes prefix-str))
              (len-bytes (types:string-to-bytes len-str)))
          (crypto:keccak256
           (types:bytes-concat-many
            (Cons prefix-bytes
                  (Cons len-bytes
                        (Cons message Nil)))))))))

  (declare personal-sign (types:Bytes -> types:Bytes -> (types:Web3Result Signature)))
  (define (personal-sign message private-key)
    "Sign a message using EIP-191 personal sign.
     Prepends the Ethereum signed message prefix before hashing and signing."
    (let ((hash (hash-personal-message message)))
      (match (crypto:sign-hash hash private-key)
        ((Err e) (Err e))
        ((Ok sig)
         ;; Convert v from 0/1 to 27/28 for legacy compatibility
         (Ok (make-signature (sig-r sig)
                             (sig-s sig)
                             (+ (sig-v sig) 27)))))))

  (declare personal-recover (types:Bytes -> Signature -> (types:Web3Result addr:Address)))
  (define (personal-recover message sig)
    "Recover the signer's address from a personal-signed message.
     The message should be the original message (without prefix)."
    (let ((hash (hash-personal-message message)))
      (recover-from-hash hash sig)))

  ;;; =========================================================================
  ;;; Raw Signing (no prefix)
  ;;; =========================================================================

  (declare sign-hash (types:Bytes -> types:Bytes -> (types:Web3Result Signature)))
  (define (sign-hash hash private-key)
    "Sign a 32-byte hash directly (no prefix).
     Returns signature with v = 0 or 1."
    (crypto:sign-hash hash private-key))

  (declare recover-from-hash (types:Bytes -> Signature -> (types:Web3Result addr:Address)))
  (define (recover-from-hash hash sig)
    "Recover the signer's address from a hash and signature"
    (match (crypto:recover-public-key hash sig)
      ((Err e) (Err e))
      ((Ok pubkey)
       ;; Derive address from public key
       (addr:address-from-public-key pubkey))))

  ;;; =========================================================================
  ;;; Verification
  ;;; =========================================================================

  (declare verify-signature (types:Bytes -> Signature -> addr:Address -> Boolean))
  (define (verify-signature hash sig expected-signer)
    "Verify that a hash was signed by the expected address"
    (match (recover-from-hash hash sig)
      ((Err _) False)
      ((Ok recovered-addr)
       (types:bytes-equal? (addr:address-bytes recovered-addr)
                           (addr:address-bytes expected-signer)))))

  (declare verify-personal-signature (types:Bytes -> Signature -> addr:Address -> Boolean))
  (define (verify-personal-signature message sig expected-signer)
    "Verify that a personal-signed message was signed by the expected address"
    (match (personal-recover message sig)
      ((Err _) False)
      ((Ok recovered-addr)
       (types:bytes-equal? (addr:address-bytes recovered-addr)
                           (addr:address-bytes expected-signer)))))

  ;;; =========================================================================
  ;;; Helper Functions
  ;;; =========================================================================

  (declare ufix-to-string (UFix -> String))
  (define (ufix-to-string n)
    "Convert UFix to decimal string"
    (lisp String (n)
      (cl:format cl:nil "~D" n)))

)


