;;;; KZG package definition
;;;; KZG commitments for EIP-4844 blobs

(defpackage #:web3/kzg
  (:documentation "KZG commitments for EIP-4844 blob transactions")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:blob #:web3/blob))
  (:export
   ;; Types
   #:KZGContext
   #:KZGCommitment
   #:KZGProof

   ;; Context management
   #:make-kzg-context
   #:kzg-context-loaded?

   ;; Core functions
   #:blob-to-commitment
   #:compute-blob-proof
   #:verify-blob-proof
   #:compute-kzg-proof

   ;; Batch operations
   #:blobs-to-commitments
   #:verify-blob-proofs))

(in-package #:web3/kzg)
(named-readtables:in-readtable coalton:coalton)
