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

;;; ERC-20 token standard
(asdf:defsystem "web3/erc20"
  :description "ERC-20 token standard support"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/address"
               "web3/abi"
               "web3/provider")
  :pathname "src/erc20/"
  :serial t
  :components ((:file "package")
               (:file "erc20")))

;;; ERC-721 NFT standard
(asdf:defsystem "web3/erc721"
  :description "ERC-721 NFT standard support"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/address"
               "web3/abi"
               "web3/provider")
  :pathname "src/erc721/"
  :serial t
  :components ((:file "package")
               (:file "erc721")))

;;; ERC-1155 Multi-Token standard
(asdf:defsystem "web3/erc1155"
  :description "ERC-1155 Multi-Token standard support"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/address"
               "web3/abi"
               "web3/provider")
  :pathname "src/erc1155/"
  :serial t
  :components ((:file "package")
               (:file "erc1155")))

;;; Event log parsing
(asdf:defsystem "web3/events"
  :description "Ethereum event log parsing and decoding"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/address"
               "web3/crypto"
               "web3/abi")
  :pathname "src/events/"
  :serial t
  :components ((:file "package")
               (:file "events")))

;;; Contract deployment
(asdf:defsystem "web3/deploy"
  :description "Contract deployment utilities - CREATE/CREATE2 address computation"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/address"
               "web3/abi"
               "web3/crypto"
               "web3/rlp")
  :pathname "src/deploy/"
  :serial t
  :components ((:file "package")
               (:file "deploy")))

;;; ENS (Ethereum Name Service)
(asdf:defsystem "web3/ens"
  :description "ENS name resolution and namehash computation"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/address"
               "web3/abi"
               "web3/crypto"
               "uiop")
  :pathname "src/ens/"
  :serial t
  :components ((:file "package")
               (:file "ens")))

;;; Multicall batching
(asdf:defsystem "web3/multicall"
  :description "Multicall3 batching support for efficient RPC calls"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/address"
               "web3/abi")
  :pathname "src/multicall/"
  :serial t
  :components ((:file "package")
               (:file "multicall")))

;;; EIP-712 Typed Data
(asdf:defsystem "web3/eip712"
  :description "EIP-712 typed structured data hashing and signing"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/crypto"
               "web3/address"
               "web3/abi")
  :pathname "src/eip712/"
  :serial t
  :components ((:file "package")
               (:file "eip712")))

;;; HD Wallet (BIP-39/BIP-32)
(asdf:defsystem "web3/hdwallet"
  :description "HD Wallet with BIP-39 mnemonics and BIP-32 key derivation"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/crypto"
               "web3/address"
               "ironclad"
               "dexador")
  :pathname "src/hdwallet/"
  :serial t
  :components ((:file "package")
               (:file "wordlist")
               (:file "hdwallet")))

;;; ABI Parser (JSON ABI to typed builders)
(asdf:defsystem "web3/abi-parser"
  :description "Parse Solidity ABI JSON and generate typed calldata builders"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/crypto"
               "web3/address"
               "web3/abi"
               "cl-json")
  :pathname "src/abi-parser/"
  :serial t
  :components ((:file "package")
               (:file "abi-parser")))

;;; Contract Abstraction Layer
(asdf:defsystem "web3/contract"
  :description "High-level contract abstraction combining ABI parser with provider"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/address"
               "web3/abi"
               "web3/abi-parser"
               "web3/provider")
  :pathname "src/contract/"
  :serial t
  :components ((:file "package")
               (:file "contract")))

;;; WebSocket Provider
(asdf:defsystem "web3/ws-provider"
  :description "WebSocket provider for real-time Ethereum subscriptions"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/address"
               "cl-json")
  :pathname "src/ws-provider/"
  :serial t
  :components ((:file "package")
               (:file "ws-provider")))

;;; Gas Utilities
(asdf:defsystem "web3/gas"
  :description "Gas utilities for EIP-1559 fee calculation and estimation"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "cl-json")
  :pathname "src/gas/"
  :serial t
  :components ((:file "package")
               (:file "gas")))

;;; Receipt Parsing
(asdf:defsystem "web3/receipt"
  :description "Transaction receipt parsing for Ethereum"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/address"
               "cl-json")
  :pathname "src/receipt/"
  :serial t
  :components ((:file "package")
               (:file "receipt")))

;;; Signature Utilities
(asdf:defsystem "web3/signature"
  :description "Signature utilities - EIP-191 personal sign and recovery"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/crypto"
               "web3/address")
  :pathname "src/signature/"
  :serial t
  :components ((:file "package")
               (:file "signature")))

;;; Chain Configs
(asdf:defsystem "web3/chain"
  :description "Pre-configured blockchain network settings"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types")
  :pathname "src/chain/"
  :serial t
  :components ((:file "package")
               (:file "chain")))

;;; Block Parsing
(asdf:defsystem "web3/block"
  :description "Ethereum block and header parsing"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/address"
               "cl-json")
  :pathname "src/block/"
  :serial t
  :components ((:file "package")
               (:file "block")))

;;; Units - parseUnits/formatUnits with custom decimals
(asdf:defsystem "web3/units"
  :description "Ethereum unit conversions - parseUnits/formatUnits with custom decimals"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types")
  :pathname "src/units/"
  :serial t
  :components ((:file "package")
               (:file "units")))

;;; Blob - EIP-4844 blob data encoding
(asdf:defsystem "web3/blob"
  :description "EIP-4844 blob data encoding and manipulation"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/crypto"
               "ironclad")
  :pathname "src/blob/"
  :serial t
  :components ((:file "package")
               (:file "constants")
               (:file "blob")))

;;; KZG - KZG commitments for EIP-4844 blobs
(asdf:defsystem "web3/kzg"
  :description "KZG commitments for EIP-4844 blob transactions"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/blob"
               "cffi")
  :pathname "src/kzg/"
  :serial t
  :components ((:file "package")
               (:file "types")
               (:file "ffi")
               (:file "kzg")))

;;; SIWE - Sign-In with Ethereum (ERC-4361)
(asdf:defsystem "web3/siwe"
  :description "Sign-In with Ethereum (ERC-4361) implementation"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/address"
               "web3/signature"
               "split-sequence")
  :pathname "src/siwe/"
  :serial t
  :components ((:file "package")
               (:file "types")
               (:file "message")
               (:file "verify")))

;;; Nonce Manager - Multi-address/chain nonce tracking
(asdf:defsystem "web3/nonce-manager"
  :description "Nonce management for multi-address/chain transaction tracking"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/address"
               "web3/provider")
  :pathname "src/nonce-manager/"
  :serial t
  :components ((:file "package")
               (:file "types")
               (:file "manager")))

;;; Transaction Simulation and Gas Estimation
(asdf:defsystem "web3/simulate"
  :description "Transaction simulation and gas estimation helpers"
  :author "Web3-Coalton Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("web3/types"
               "web3/address"
               "web3/transaction"
               "web3/provider"
               "web3/gas")
  :pathname "src/simulate/"
  :serial t
  :components ((:file "package")
               (:file "simulate")))

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
               "web3/wallet"
               "web3/erc20"
               "web3/erc721"
               "web3/erc1155"
               "web3/events"
               "web3/deploy"
               "web3/ens"
               "web3/multicall"
               "web3/eip712"
               "web3/hdwallet"
               "web3/abi-parser"
               "web3/contract"
               "web3/ws-provider"
               "web3/gas"
               "web3/receipt"
               "web3/signature"
               "web3/chain"
               "web3/block"
               "web3/units"
               "web3/blob"
               "web3/kzg"
               "web3/siwe"
               "web3/nonce-manager"
               "web3/simulate"))

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
               (:file "wallet-tests")
               (:file "erc20-tests")
               (:file "erc721-tests")
               (:file "erc1155-tests")
               (:file "events-tests")
               (:file "deploy-tests")
               (:file "ens-tests")
               (:file "multicall-tests")
               (:file "eip712-tests")
               (:file "hdwallet-tests")
               (:file "abi-parser-tests")
               (:file "contract-tests")
               (:file "ws-provider-tests")
               (:file "gas-tests")
               (:file "receipt-tests")
               (:file "signature-tests")
               (:file "chain-tests")
               (:file "block-tests")
               (:file "units-tests")
               (:file "blob-tests")
               (:file "kzg-tests")
               (:file "siwe-tests")
               (:file "nonce-manager-tests")
               (:file "simulate-tests")
               (:file "integration-tests"))
  :perform (asdf:test-op (o s)
             (uiop:symbol-call '#:web3-tests/runner '#:run-all-tests)))
