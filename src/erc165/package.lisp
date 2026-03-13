;;;; ERC-165 package definition
;;;; Standard interface detection

(defpackage #:web3/erc165
  (:documentation "ERC-165 Standard Interface Detection - supportsInterface")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:addr #:web3/address)
   (#:abi #:web3/abi)
   (#:provider #:web3/provider))
  (:export
   ;; Core function
   #:supports-interface

   ;; Function selector
   #:selector-supports-interface

   ;; Well-known interface IDs
   #:interface-erc165
   #:interface-erc721
   #:interface-erc721-metadata
   #:interface-erc721-enumerable
   #:interface-erc1155
   #:interface-erc1155-metadata-uri
   #:interface-erc20))
