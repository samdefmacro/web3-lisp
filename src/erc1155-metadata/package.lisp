;;;; ERC-1155 Metadata - Fetch and parse multi-token metadata JSON

(defpackage #:web3/erc1155-metadata
  (:documentation "ERC-1155 metadata fetching with URI template {id} substitution")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:addr #:web3/address)
   (#:erc1155 #:web3/erc1155)
   (#:nft #:web3/erc721-metadata)
   (#:provider #:web3/provider))
  (:export
   ;; URI template substitution
   #:substitute-token-id

   ;; Fetching
   #:fetch-erc1155-metadata))

(in-package #:web3/erc1155-metadata)
(named-readtables:in-readtable coalton:coalton)
