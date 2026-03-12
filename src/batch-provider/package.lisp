(defpackage #:web3/batch-provider
  (:documentation "JSON-RPC batch request support for efficient multi-call RPC")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:addr #:web3/address)
   (#:provider #:web3/provider))
  (:export
   ;; Batch request types
   #:RpcRequest
   #:make-rpc-request
   #:BatchResult
   #:BatchOk
   #:BatchErr
   #:batch-result-id
   #:batch-result-value

   ;; Batch execution
   #:batch-call
   #:batch-call-raw

   ;; Convenience builders
   #:batch-eth-chain-id
   #:batch-eth-block-number
   #:batch-eth-get-balance
   #:batch-eth-get-transaction-count
   #:batch-eth-gas-price
   #:batch-eth-call
   #:batch-eth-get-code
   #:batch-eth-get-storage-at
   #:batch-eth-max-priority-fee-per-gas))

(in-package #:web3/batch-provider)
(named-readtables:in-readtable coalton:coalton)
