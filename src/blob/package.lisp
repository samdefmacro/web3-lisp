;;;; Blob package definition
;;;; EIP-4844 blob data encoding

(defpackage #:web3/blob
  (:documentation "EIP-4844 blob data encoding and manipulation")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:crypto #:web3/crypto)
   (#:list #:coalton-library/list))
  (:export
   ;; Constants
   #:+bytes-per-field-element+
   #:+field-elements-per-blob+
   #:+bytes-per-blob+
   #:+max-blobs-per-block+
   #:+usable-bytes-per-field-element+

   ;; Core functions
   #:to-blobs
   #:from-blobs
   #:commitment-to-versioned-hash
   #:empty-blob

   ;; Helpers
   #:blob-count-for-data
   #:valid-blob?))

(in-package #:web3/blob)
(named-readtables:in-readtable coalton:coalton)
