;;;; Signature Utils package definition
;;;; EIP-191 personal sign, signature recovery, and verification

(defpackage #:web3/signature
  (:documentation "Ethereum signature utilities - EIP-191 personal sign and recovery")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:crypto #:web3/crypto)
   (#:addr #:web3/address))
  (:export
   ;; Signature type
   #:Signature
   #:make-signature
   #:sig-v
   #:sig-r
   #:sig-s
   #:signature-to-bytes
   #:signature-from-bytes
   #:signature-to-hex
   #:signature-from-hex

   ;; EIP-191 Personal Sign
   #:personal-sign-prefix
   #:hash-personal-message
   #:personal-sign
   #:personal-recover

   ;; Raw signing (no prefix)
   #:sign-hash
   #:recover-from-hash

   ;; Verification
   #:verify-signature
   #:verify-personal-signature

   ;; Utilities
   #:split-signature
   #:join-signature
   #:normalize-v
   #:to-eip155-v
   #:from-eip155-v))

(in-package #:web3/signature)
(named-readtables:in-readtable coalton:coalton)
