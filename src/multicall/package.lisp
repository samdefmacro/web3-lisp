;;;; Multicall package definition
;;;; Batch multiple contract calls into a single RPC request

(defpackage #:web3/multicall
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:addr #:web3/address)
   (#:abi #:web3/abi))
  (:export
   ;; Multicall3 contract address
   #:multicall3-address

   ;; Call types
   #:Call
   #:make-Call
   #:.target
   #:.calldata

   #:Call3
   #:make-Call3
   #:.allow-failure

   #:CallResult
   #:make-CallResult
   #:.success
   #:.return-data

   ;; Selectors
   #:aggregate-selector
   #:try-aggregate-selector
   #:aggregate3-selector

   ;; Calldata builders
   #:aggregate-calldata
   #:try-aggregate-calldata
   #:aggregate3-calldata

   ;; High-level helpers
   #:batch-calls
   #:batch-calls-allow-failure))
