;;;; Shared eth_call helpers
;;;; Common (eth_call -> abi-decode -> singleton extraction) wrappers used by
;;;; ERC-20, ERC-721, ERC-1155 (and other view-call surfaces).

(defpackage #:web3/abi-call
  (:documentation "Shared helpers for eth_call -> abi-decode of singleton return values")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:addr #:web3/address)
   (#:abi #:web3/abi)
   (#:provider #:web3/provider))
  (:export
   #:call-decode-string
   #:call-decode-u256
   #:call-decode-address
   #:call-decode-bool))

(in-package #:web3/abi-call)
(named-readtables:in-readtable coalton:coalton)
