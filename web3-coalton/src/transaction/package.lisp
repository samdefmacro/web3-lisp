(defpackage #:web3/transaction
  (:documentation "Ethereum transaction creation, encoding, and signing")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:vec #:coalton-library/vector)
   (#:iter #:coalton-library/iterator)
   (#:list #:coalton-library/list)
   (#:types #:web3/types)
   (#:rlp #:web3/rlp)
   (#:crypto #:web3/crypto)
   (#:addr #:web3/address))
  (:export
   ;; Transaction types
   #:TransactionType
   #:LegacyTx
   #:EIP2930Tx
   #:EIP1559Tx

   ;; Transaction struct
   #:Transaction
   #:make-transaction
   #:tx-type
   #:tx-chain-id
   #:tx-nonce
   #:tx-gas-price
   #:tx-max-priority-fee
   #:tx-max-fee
   #:tx-gas-limit
   #:tx-to
   #:tx-value
   #:tx-data
   #:tx-access-list

   ;; Encoding
   #:tx-encode-for-signing
   #:signed-tx-encode

   ;; Signing
   #:tx-sign

   ;; Decoding
   #:tx-decode))

(in-package #:web3/transaction)
(named-readtables:in-readtable coalton:coalton)
