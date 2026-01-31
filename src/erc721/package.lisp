(defpackage #:web3/erc721
  (:documentation "ERC-721 NFT standard support")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:addr #:web3/address)
   (#:abi #:web3/abi)
   (#:provider #:web3/provider))
  (:export
   ;; Read functions (view calls via eth_call)
   #:erc721-name
   #:erc721-symbol
   #:erc721-token-uri
   #:erc721-balance-of
   #:erc721-owner-of
   #:erc721-get-approved
   #:erc721-is-approved-for-all

   ;; Write function calldata builders
   #:erc721-transfer-from-data
   #:erc721-safe-transfer-from-data
   #:erc721-safe-transfer-from-with-data
   #:erc721-approve-data
   #:erc721-set-approval-for-all-data

   ;; Function selectors (for testing/advanced use)
   #:selector-name
   #:selector-symbol
   #:selector-token-uri
   #:selector-balance-of
   #:selector-owner-of
   #:selector-get-approved
   #:selector-is-approved-for-all
   #:selector-transfer-from
   #:selector-safe-transfer-from
   #:selector-safe-transfer-from-with-data
   #:selector-approve
   #:selector-set-approval-for-all))

(in-package #:web3/erc721)
(named-readtables:in-readtable coalton:coalton)
