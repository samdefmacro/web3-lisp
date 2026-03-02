(defpackage #:web3/provider
  (:documentation "Ethereum JSON-RPC provider")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:addr #:web3/address))
  (:export
   ;; Provider type
   #:HttpProvider
   #:make-http-provider

   ;; JSON-RPC methods
   #:eth-chain-id
   #:eth-block-number
   #:eth-get-balance
   #:eth-get-transaction-count
   #:eth-gas-price
   #:eth-estimate-gas
   #:eth-send-raw-transaction
   #:eth-call
   #:eth-get-transaction-receipt

   ;; Low-level
   #:json-rpc-call))

(in-package #:web3/provider)
(named-readtables:in-readtable coalton:coalton)
