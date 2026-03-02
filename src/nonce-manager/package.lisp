;;;; Nonce Manager package definition
;;;; Multi-address/chain nonce tracking

(defpackage #:web3/nonce-manager
  (:documentation "Nonce management for multi-address/chain transaction tracking")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:addr #:web3/address)
   (#:provider #:web3/provider)
   (#:cell #:coalton-library/cell))
  (:export
   ;; Types
   #:NonceManager
   #:NonceKey

   ;; Constructor
   #:make-nonce-manager

   ;; Core operations
   #:nonce-get
   #:nonce-consume
   #:nonce-reset
   #:nonce-sync
   #:nonce-peek

   ;; Utilities
   #:nonce-manager-provider))

(in-package #:web3/nonce-manager)
(named-readtables:in-readtable coalton:coalton)
