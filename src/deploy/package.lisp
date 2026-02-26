;;;; Contract deployment package definition

(defpackage #:web3/deploy
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:addr #:web3/address)
   (#:abi #:web3/abi)
   (#:crypto #:web3/crypto)
   (#:rlp #:web3/rlp))
  (:export
   ;; Deployment data construction
   #:deployment-data
   #:deployment-data-with-constructor

   ;; CREATE address computation
   #:compute-create-address
   #:compute-create-address-from-hex

   ;; CREATE2 address computation
   #:compute-create2-address
   #:compute-create2-address-from-hex

   ;; Constructor encoding helpers
   #:encode-constructor))
