(defpackage #:web3/erc1155
  (:documentation "ERC-1155 Multi-Token standard support")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:addr #:web3/address)
   (#:abi #:web3/abi)
   (#:provider #:web3/provider))
  (:export
   ;; Read functions (view calls via eth_call)
   #:erc1155-uri
   #:erc1155-balance-of
   #:erc1155-balance-of-batch
   #:erc1155-is-approved-for-all

   ;; Write function calldata builders
   #:erc1155-safe-transfer-from-data
   #:erc1155-safe-batch-transfer-from-data
   #:erc1155-set-approval-for-all-data

   ;; Function selectors (for testing/advanced use)
   #:selector-uri
   #:selector-balance-of
   #:selector-balance-of-batch
   #:selector-is-approved-for-all
   #:selector-safe-transfer-from
   #:selector-safe-batch-transfer-from
   #:selector-set-approval-for-all))

(in-package #:web3/erc1155)
(named-readtables:in-readtable coalton:coalton)
