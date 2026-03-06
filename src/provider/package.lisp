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
   #:eth-get-code
   #:eth-get-storage-at
   #:eth-max-priority-fee-per-gas
   #:eth-get-block-by-number
   #:eth-get-block-by-hash
   #:eth-get-transaction-by-hash
   #:eth-fee-history
   #:eth-syncing
   #:wait-for-transaction-receipt

   ;; Low-level
   #:json-rpc-call
   #:json-rpc-call-nullable))

(in-package #:web3/provider)
(named-readtables:in-readtable coalton:coalton)
