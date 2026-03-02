(defpackage #:web3/address
  (:documentation "Ethereum address handling with EIP-55 checksum")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:crypto #:web3/crypto))
  (:export
   ;; Address type
   #:Address
   #:address-bytes
   #:address-from-bytes
   #:address-from-hex
   #:address-to-hex
   #:address-to-checksum-hex
   #:address-from-public-key
   #:address-zero

   ;; Contract address computation
   #:compute-contract-address
   #:compute-create2-address))

(in-package #:web3/address)
(named-readtables:in-readtable coalton:coalton)
