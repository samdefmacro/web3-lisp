(defpackage #:web3/erc20
  (:documentation "ERC-20 token standard support")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:addr #:web3/address)
   (#:abi #:web3/abi)
   (#:provider #:web3/provider))
  (:export
   ;; Read functions (view calls via eth_call)
   #:erc20-name
   #:erc20-symbol
   #:erc20-decimals
   #:erc20-total-supply
   #:erc20-balance-of
   #:erc20-allowance

   ;; Write function calldata builders
   #:erc20-transfer-data
   #:erc20-approve-data
   #:erc20-transfer-from-data

   ;; Function selectors (for testing/advanced use)
   #:selector-name
   #:selector-symbol
   #:selector-decimals
   #:selector-total-supply
   #:selector-balance-of
   #:selector-allowance
   #:selector-transfer
   #:selector-approve
   #:selector-transfer-from))

(in-package #:web3/erc20)
(named-readtables:in-readtable coalton:coalton)
