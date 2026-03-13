;;;; ERC-721 Metadata - Fetch and parse NFT metadata JSON from tokenURI

(defpackage #:web3/erc721-metadata
  (:documentation "ERC-721 metadata fetching and parsing from tokenURI responses")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:addr #:web3/address)
   (#:erc721 #:web3/erc721)
   (#:provider #:web3/provider))
  (:export
   ;; Types
   #:NftAttribute
   #:NftAttribute
   #:nft-attribute-trait-type
   #:nft-attribute-value

   #:NftMetadata
   #:NftMetadata
   #:nft-metadata-name
   #:nft-metadata-description
   #:nft-metadata-image
   #:nft-metadata-external-url
   #:nft-metadata-animation-url
   #:nft-metadata-attributes

   ;; Fetching
   #:fetch-metadata-json
   #:fetch-token-metadata

   ;; URI helpers
   #:ipfs-to-gateway-url
   #:parse-data-uri))

(in-package #:web3/erc721-metadata)
(named-readtables:in-readtable coalton:coalton)
