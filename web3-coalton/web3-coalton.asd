;;;; web3-coalton - Ethereum library implemented in Coalton
;;;; Modeled after ethers.js and viem

;;; Core types module
(asdf:defsystem "web3/types"
  :description "Web3 core types - Bytes, U256, hex encoding"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("coalton"
               "named-readtables")
  :pathname "src/types/"
  :serial t
  :components ((:file "package")
               (:file "errors")
               (:file "bytes")
               (:file "numeric")))

;;; RLP encoding/decoding
(asdf:defsystem "web3/rlp"
  :description "RLP (Recursive Length Prefix) encoding/decoding"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types")
  :pathname "src/rlp/"
  :serial t
  :components ((:file "package")
               (:file "encode")
               (:file "decode")))

;;; Cryptographic primitives
(asdf:defsystem "web3/crypto"
  :description "Cryptographic primitives - keccak256, secp256k1"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "ironclad")
  :pathname "src/crypto/"
  :serial t
  :components ((:file "package")
               (:file "keccak")
               (:file "secp256k1")
               (:file "signature")))

;;; Ethereum addresses
(asdf:defsystem "web3/address"
  :description "Ethereum address handling with EIP-55 checksum"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/crypto")
  :pathname "src/address/"
  :serial t
  :components ((:file "package")
               (:file "address")))

;;; ABI encoding/decoding
(asdf:defsystem "web3/abi"
  :description "Ethereum ABI encoding/decoding"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/crypto")
  :pathname "src/abi/"
  :serial t
  :components ((:file "package")
               (:file "types")
               (:file "encode")
               (:file "decode")
               (:file "selector")))

;;; Transaction types and encoding
(asdf:defsystem "web3/transaction"
  :description "Ethereum transaction creation, encoding, and signing"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/rlp"
               "web3/crypto"
               "web3/address")
  :pathname "src/transaction/"
  :serial t
  :components ((:file "package")
               (:file "types")
               (:file "encode")
               (:file "decode")
               (:file "sign")))

;;; JSON-RPC provider
(asdf:defsystem "web3/provider"
  :description "Ethereum JSON-RPC provider"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/address"
               "web3/transaction"
               "dexador"
               "cl-json")
  :pathname "src/provider/"
  :serial t
  :components ((:file "package")
               (:file "json-rpc")
               (:file "http")))

;;; Wallet
(asdf:defsystem "web3/wallet"
  :description "Ethereum wallet - private key + provider"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/crypto"
               "web3/address"
               "web3/transaction"
               "web3/provider")
  :pathname "src/wallet/"
  :serial t
  :components ((:file "package")
               (:file "wallet")))

;;; Meta-system that loads everything
(asdf:defsystem "web3"
  :description "Complete Ethereum library in Coalton"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/rlp"
               "web3/crypto"
               "web3/address"
               "web3/abi"
               "web3/transaction"
               "web3/provider"
               "web3/wallet"))

;;; Test system
(asdf:defsystem "web3/tests"
  :description "Tests for web3-coalton"
  :depends-on ("web3")
  :pathname "test/"
  :serial t
  :components ((:file "package")
               (:file "helpers")
               (:file "types-tests")
               (:file "rlp-tests")
               (:file "crypto-tests")
               (:file "address-tests")
               (:file "abi-tests")
               (:file "transaction-tests")
               (:file "provider-tests")
               (:file "wallet-tests"))
  :perform (asdf:test-op (o s)
             (uiop:symbol-call '#:web3-tests/runner '#:run-all-tests)))
