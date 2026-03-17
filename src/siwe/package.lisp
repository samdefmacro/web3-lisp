;;;; SIWE package definition
;;;; Sign-In with Ethereum (ERC-4361)

(defpackage #:web3/siwe
  (:documentation "Sign-In with Ethereum (ERC-4361) implementation")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:addr #:web3/address)
   (#:sig #:web3/signature)
   (#:str #:coalton-library/string)
   (#:list #:coalton-library/list))
  (:export
   ;; Types
   #:SiweMessage
   #:make-siwe-message
   #:.siwe-domain
   #:.siwe-address
   #:.siwe-statement
   #:.siwe-uri
   #:.siwe-version
   #:.siwe-chain-id
   #:.siwe-nonce
   #:.siwe-issued-at
   #:.siwe-expiration-time
   #:.siwe-not-before
   #:.siwe-request-id
   #:.siwe-resources

   ;; Message creation/parsing
   #:create-siwe-message
   #:parse-siwe-message

   ;; Validation
   #:validate-siwe-signature
   #:verify-siwe-message
   #:siwe-message-expired?
   #:siwe-message-not-yet-valid?

   ;; Nonce generation
   #:generate-siwe-nonce))

(in-package #:web3/siwe)
(named-readtables:in-readtable coalton:coalton)
