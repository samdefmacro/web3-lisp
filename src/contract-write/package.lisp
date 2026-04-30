;;;; Contract write operations - high-level send-transaction for contracts

(defpackage #:web3/contract-write
  (:documentation "High-level contract write transactions: encode + sign + send")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:addr #:web3/address)
   (#:abi #:web3/abi)
   (#:tx #:web3/transaction)
   (#:provider #:web3/provider)
   (#:wallet #:web3/wallet)
   (#:simulate #:web3/simulate)
   (#:contract #:web3/contract))
  (:export
   #:send-function-call
   #:send-function-call-with-builder
   #:send-raw-call))

(in-package #:web3/contract-write)
(named-readtables:in-readtable coalton:coalton)
