;;;; ABI Parser package definition
;;;; Parse Solidity ABI JSON and generate typed calldata builders

(defpackage #:web3/abi-parser
  (:documentation "Parse Solidity ABI JSON and generate typed calldata builders")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:vec #:coalton-library/vector)
   (#:iter #:coalton-library/iterator)
   (#:list #:coalton-library/list)
   (#:str #:coalton-library/string)
   (#:types #:web3/types)
   (#:abi #:web3/abi)
   (#:crypto #:web3/crypto)
   (#:addr #:web3/address))
  (:export
   ;; ABI item types
   #:AbiItem
   #:AbiFunction
   #:AbiEvent
   #:AbiConstructor
   #:AbiFallback
   #:AbiReceive
   #:AbiError

   ;; ABI parameter
   #:AbiParam
   #:make-abi-param
   #:.param-name
   #:.param-type
   #:.param-indexed

   ;; Parsed function
   #:ParsedFunction
   #:.fn-name
   #:.fn-inputs
   #:.fn-outputs
   #:.fn-selector
   #:.fn-state-mutability

   ;; Parsed event
   #:ParsedEvent
   #:.event-name
   #:.event-inputs
   #:.event-topic
   #:.event-anonymous

   ;; Type parsing
   #:parse-abi-type
   #:type-string-to-abi-type

   ;; ABI JSON parsing
   #:parse-abi-json
   #:parse-function-json
   #:parse-event-json

   ;; Calldata building from parsed ABI
   #:encode-function-call
   #:decode-function-output
   #:decode-event-log

   ;; Function signature building
   #:build-function-signature
   #:build-event-signature))

(in-package #:web3/abi-parser)
(named-readtables:in-readtable coalton:coalton)
