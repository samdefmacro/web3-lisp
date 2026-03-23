;;;; Chain Configs package definition
;;;; Pre-configured blockchain network settings

(defpackage #:web3/chain
  (:documentation "Pre-configured blockchain network settings - chain IDs, names, explorers")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types))
  (:export
   ;; Chain type
   #:Chain
   #:NativeCurrency

   ;; Chain accessors
   #:.chain-id
   #:.chain-name
   #:.chain-short-name
   #:.chain-native-currency
   #:.chain-block-explorer
   #:.chain-is-testnet

   ;; Native currency accessors
   #:.currency-name
   #:.currency-symbol
   #:.currency-decimals

   ;; Chain ID constants
   #:chain-id-mainnet
   #:chain-id-sepolia
   #:chain-id-holesky
   #:chain-id-polygon
   #:chain-id-polygon-amoy
   #:chain-id-arbitrum
   #:chain-id-arbitrum-sepolia
   #:chain-id-optimism
   #:chain-id-optimism-sepolia
   #:chain-id-base
   #:chain-id-base-sepolia
   #:chain-id-bsc
   #:chain-id-bsc-testnet
   #:chain-id-avalanche
   #:chain-id-avalanche-fuji
   #:chain-id-gnosis
   #:chain-id-fantom
   #:chain-id-celo
   #:chain-id-zksync
   #:chain-id-linea
   #:chain-id-scroll
   #:chain-id-mantle
   #:chain-id-blast
   #:chain-id-local

   ;; Pre-configured chains
   #:ethereum-mainnet
   #:sepolia
   #:holesky
   #:polygon
   #:polygon-amoy
   #:arbitrum-one
   #:arbitrum-sepolia
   #:optimism
   #:optimism-sepolia
   #:base
   #:base-sepolia
   #:bsc
   #:bsc-testnet
   #:avalanche
   #:avalanche-fuji
   #:gnosis
   #:fantom
   #:celo
   #:zksync-era
   #:linea
   #:scroll
   #:mantle
   #:blast
   #:localhost
   #:hardhat
   #:anvil

   ;; Lookup functions
   #:get-chain-by-id
   #:get-chain-by-name
   #:all-chains
   #:mainnet-chains
   #:testnet-chains

   ;; Utilities
   #:is-eip1559-chain
   #:default-block-time
   #:explorer-tx-url
   #:explorer-address-url
   #:explorer-block-url))

(in-package #:web3/chain)
(named-readtables:in-readtable coalton:coalton)
