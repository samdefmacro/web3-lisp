(defpackage #:web3/wallet
  (:documentation "Ethereum wallet - private key + provider")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:crypto #:web3/crypto)
   (#:addr #:web3/address)
   (#:tx #:web3/transaction)
   (#:provider #:web3/provider))
  (:export
   ;; Wallet type
   #:Wallet
   #:make-wallet
   #:wallet-with-provider

   ;; Wallet operations
   #:wallet-address
   #:wallet-sign-transaction
   #:wallet-send-transaction
   #:wallet-get-balance
   #:wallet-get-nonce))

(in-package #:web3/wallet)
(named-readtables:in-readtable coalton:coalton)
