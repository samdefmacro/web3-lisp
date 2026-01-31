;;;; Contract Abstraction package definition
;;;; High-level interface for interacting with smart contracts

(defpackage #:web3/contract
  (:documentation "High-level contract abstraction combining ABI parser with provider")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:vec #:coalton-library/vector)
   (#:iter #:coalton-library/iterator)
   (#:list #:coalton-library/list)
   (#:str #:coalton-library/string)
   (#:types #:web3/types)
   (#:abi #:web3/abi)
   (#:abi-parser #:web3/abi-parser)
   (#:addr #:web3/address)
   (#:provider #:web3/provider))
  (:export
   ;; Contract type
   #:Contract
   #:make-contract
   #:contract-from-abi-json
   #:.contract-address
   #:.contract-abi

   ;; Function lookup
   #:get-function
   #:get-event
   #:get-function-by-selector
   #:get-event-by-topic
   #:list-functions
   #:list-events

   ;; Encoding
   #:encode-function-call
   #:encode-function-call-by-name

   ;; Decoding
   #:decode-function-output
   #:decode-function-output-by-name
   #:decode-event
   #:decode-event-by-topic

   ;; Call building helpers
   #:CallBuilder
   #:call-builder
   #:with-arg
   #:build-calldata
   #:build-call-request

   ;; High-level read call
   #:CallRequest
   #:.call-to
   #:.call-data))

(in-package #:web3/contract)
(named-readtables:in-readtable coalton:coalton)
