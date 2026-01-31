;;;; ENS (Ethereum Name Service) package definition

(defpackage #:web3/ens
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:addr #:web3/address)
   (#:abi #:web3/abi)
   (#:crypto #:web3/crypto))
  (:export
   ;; Namehash computation
   #:namehash
   #:namehash-hex
   #:labelhash

   ;; ENS contract addresses (mainnet)
   #:ens-registry-address
   #:ens-public-resolver-address

   ;; Registry selectors and calldata
   #:ens-resolver-selector
   #:ens-owner-selector
   #:ens-resolver-calldata
   #:ens-owner-calldata

   ;; Resolver selectors and calldata
   #:resolver-addr-selector
   #:resolver-name-selector
   #:resolver-text-selector
   #:resolver-contenthash-selector
   #:resolver-addr-calldata
   #:resolver-name-calldata
   #:resolver-text-calldata

   ;; Reverse resolution
   #:reverse-node
   #:address-to-reverse-name

   ;; Decoding
   #:decode-address-result
   #:decode-name-result
   #:decode-text-result))
