;;;; web3/prelude - One-stop import for the high-traffic public surface
;;;;
;;;; Re-exports the most commonly used types and functions across web3-lisp so
;;;; that consumers can write `(use-package :web3/prelude)` and get the 80%
;;;; surface (types, addresses, units, provider, wallet, transactions, ERC-20,
;;;; contracts, ABI values, common chains).
;;;;
;;;; For specialised modules (ENS, EIP-712, blob/KZG, hdwallet, ws-provider,
;;;; ERC-721/1155, batch-provider, etc.) keep importing the dedicated package
;;;; directly — they intentionally stay out of the prelude.

(defpackage #:web3/prelude
  (:documentation
   "Curated re-exports from across web3-lisp. One import for the common surface.")
  (:use #:coalton
        #:coalton-prelude)

  ;; Core types
  (:import-from #:web3/types
                #:Bytes #:U256 #:Web3Result
                #:bytes-empty #:bytes-length #:bytes-equal? #:bytes-from-list
                #:bytes-append #:bytes-concat-many #:bytes-slice
                #:bytes-pad-left #:bytes-pad-right
                #:hex-encode #:hex-decode
                #:hex-encode-prefixed #:hex-decode-prefixed
                #:u256-zero #:u256-one #:u256-from-integer #:u256-to-integer
                #:u256-equal? #:u256-less-than? #:u256-greater-than?
                #:u256-add #:u256-sub #:u256-mul #:u256-div #:u256-mod
                #:u256-zero? #:u256-max
                #:wei-to-gwei #:gwei-to-wei #:ether-to-wei)

  ;; Addresses
  (:import-from #:web3/address
                #:Address
                #:address-from-hex #:address-from-bytes
                #:address-to-hex #:address-to-checksum-hex
                #:address-bytes #:address-zero
                #:compute-contract-address #:compute-create2-address)

  ;; Units
  (:import-from #:web3/units
                #:parse-units #:format-units
                #:parse-ether  #:format-ether
                #:parse-gwei   #:format-gwei)

  ;; Chains (objects + lookup; the 26 chain-id constants stay in web3/chain)
  (:import-from #:web3/chain
                #:Chain #:NativeCurrency
                #:.chain-id #:.chain-name #:.chain-short-name
                #:.chain-native-currency #:.chain-block-explorer #:.chain-is-testnet
                #:ethereum-mainnet #:sepolia #:holesky
                #:polygon #:polygon-amoy
                #:arbitrum-one #:arbitrum-sepolia
                #:optimism #:optimism-sepolia
                #:base #:base-sepolia
                #:bsc #:avalanche #:gnosis
                #:zksync-era #:linea #:scroll #:mantle #:blast
                #:localhost #:hardhat #:anvil
                #:get-chain-by-id #:get-chain-by-name
                #:explorer-tx-url #:explorer-address-url #:explorer-block-url)

  ;; Provider (JSON-RPC)
  (:import-from #:web3/provider
                #:HttpProvider #:make-http-provider
                #:eth-chain-id #:eth-block-number
                #:eth-get-balance #:eth-get-transaction-count
                #:eth-gas-price #:eth-max-priority-fee-per-gas
                #:eth-estimate-gas
                #:eth-call #:eth-send-raw-transaction
                #:eth-get-transaction-receipt #:wait-for-transaction-receipt
                #:eth-get-code #:eth-get-storage-at
                #:eth-get-block-by-number #:eth-get-block-by-hash
                #:eth-get-transaction-by-hash
                #:eth-fee-history #:eth-syncing)

  ;; Transactions
  (:import-from #:web3/transaction
                #:Transaction #:make-transaction #:make-blob-transaction
                #:TransactionType #:LegacyTx #:EIP2930Tx #:EIP1559Tx #:EIP4844Tx
                #:AccessListEntry #:AccessList
                #:.tx-type #:.tx-chain-id #:.tx-nonce
                #:.tx-gas-price #:.tx-max-priority-fee #:.tx-max-fee
                #:.tx-gas-limit #:.tx-to #:.tx-value #:.tx-data
                #:.tx-access-list
                #:tx-encode-for-signing #:signed-tx-encode #:tx-sign #:tx-decode)

  ;; Wallet
  (:import-from #:web3/wallet
                #:Wallet #:make-wallet #:wallet-with-provider
                #:wallet-address #:wallet-sign-transaction #:wallet-send-transaction
                #:wallet-get-balance #:wallet-get-nonce)

  ;; ERC-20 (the only token standard re-exported by default; opt-in to 721/1155)
  (:import-from #:web3/erc20
                #:erc20-name #:erc20-symbol #:erc20-decimals
                #:erc20-total-supply #:erc20-balance-of #:erc20-allowance
                #:erc20-transfer-data #:erc20-approve-data
                #:erc20-transfer-from-data)

  ;; ABI values (so users can build custom calls)
  (:import-from #:web3/abi
                #:AbiValue
                #:AbiUintVal #:AbiIntVal #:AbiAddressVal #:AbiBoolVal
                #:AbiBytesVal #:AbiBytesFixedVal #:AbiStringVal
                #:AbiArrayVal #:AbiFixedArrayVal #:AbiTupleVal
                #:AbiType
                #:AbiUint #:AbiInt #:AbiAddress #:AbiBool
                #:AbiBytes #:AbiBytesFixed #:AbiString
                #:AbiArray #:AbiFixedArray #:AbiTuple
                #:abi-encode #:abi-decode #:abi-encode-with-selector
                #:function-selector)

  ;; Contracts (high-level) — uses the by-name encoders/decoders to avoid
  ;; symbol clashes with the lower-level abi-parser package.
  (:import-from #:web3/contract
                #:Contract #:make-contract #:contract-from-abi-json
                #:.contract-address #:.contract-abi
                #:get-function #:get-event
                #:encode-function-call-by-name
                #:decode-function-output-by-name
                #:decode-event-by-topic
                #:CallRequest #:.call-to #:.call-data)

  ;; Contract write (sign + send in one step)
  (:import-from #:web3/contract-write
                #:send-function-call)

  (:export
   ;; Core types
   #:Bytes #:U256 #:Web3Result
   #:bytes-empty #:bytes-length #:bytes-equal? #:bytes-from-list
   #:bytes-append #:bytes-concat-many #:bytes-slice
   #:bytes-pad-left #:bytes-pad-right
   #:hex-encode #:hex-decode
   #:hex-encode-prefixed #:hex-decode-prefixed
   #:u256-zero #:u256-one #:u256-from-integer #:u256-to-integer
   #:u256-equal? #:u256-less-than? #:u256-greater-than?
   #:u256-add #:u256-sub #:u256-mul #:u256-div #:u256-mod
   #:u256-zero? #:u256-max
   #:wei-to-gwei #:gwei-to-wei #:ether-to-wei

   ;; Addresses
   #:Address
   #:address-from-hex #:address-from-bytes
   #:address-to-hex #:address-to-checksum-hex
   #:address-bytes #:address-zero
   #:compute-contract-address #:compute-create2-address

   ;; Units
   #:parse-units #:format-units
   #:parse-ether  #:format-ether
   #:parse-gwei   #:format-gwei

   ;; Chains
   #:Chain #:NativeCurrency
   #:.chain-id #:.chain-name #:.chain-short-name
   #:.chain-native-currency #:.chain-block-explorer #:.chain-is-testnet
   #:ethereum-mainnet #:sepolia #:holesky
   #:polygon #:polygon-amoy
   #:arbitrum-one #:arbitrum-sepolia
   #:optimism #:optimism-sepolia
   #:base #:base-sepolia
   #:bsc #:avalanche #:gnosis
   #:zksync-era #:linea #:scroll #:mantle #:blast
   #:localhost #:hardhat #:anvil
   #:get-chain-by-id #:get-chain-by-name
   #:explorer-tx-url #:explorer-address-url #:explorer-block-url

   ;; Provider
   #:HttpProvider #:make-http-provider
   #:eth-chain-id #:eth-block-number
   #:eth-get-balance #:eth-get-transaction-count
   #:eth-gas-price #:eth-max-priority-fee-per-gas
   #:eth-estimate-gas
   #:eth-call #:eth-send-raw-transaction
   #:eth-get-transaction-receipt #:wait-for-transaction-receipt
   #:eth-get-code #:eth-get-storage-at
   #:eth-get-block-by-number #:eth-get-block-by-hash
   #:eth-get-transaction-by-hash
   #:eth-fee-history #:eth-syncing

   ;; Transactions
   #:Transaction #:make-transaction #:make-blob-transaction
   #:TransactionType #:LegacyTx #:EIP2930Tx #:EIP1559Tx #:EIP4844Tx
   #:AccessListEntry #:AccessList
   #:.tx-type #:.tx-chain-id #:.tx-nonce
   #:.tx-gas-price #:.tx-max-priority-fee #:.tx-max-fee
   #:.tx-gas-limit #:.tx-to #:.tx-value #:.tx-data
   #:.tx-access-list
   #:tx-encode-for-signing #:signed-tx-encode #:tx-sign #:tx-decode

   ;; Wallet
   #:Wallet #:make-wallet #:wallet-with-provider
   #:wallet-address #:wallet-sign-transaction #:wallet-send-transaction
   #:wallet-get-balance #:wallet-get-nonce

   ;; ERC-20
   #:erc20-name #:erc20-symbol #:erc20-decimals
   #:erc20-total-supply #:erc20-balance-of #:erc20-allowance
   #:erc20-transfer-data #:erc20-approve-data
   #:erc20-transfer-from-data

   ;; ABI values
   #:AbiValue
   #:AbiUintVal #:AbiIntVal #:AbiAddressVal #:AbiBoolVal
   #:AbiBytesVal #:AbiBytesFixedVal #:AbiStringVal
   #:AbiArrayVal #:AbiFixedArrayVal #:AbiTupleVal
   #:AbiType
   #:AbiUint #:AbiInt #:AbiAddress #:AbiBool
   #:AbiBytes #:AbiBytesFixed #:AbiString
   #:AbiArray #:AbiFixedArray #:AbiTuple
   #:abi-encode #:abi-decode #:abi-encode-with-selector
   #:function-selector

   ;; Contracts
   #:Contract #:make-contract #:contract-from-abi-json
   #:.contract-address #:.contract-abi
   #:get-function #:get-event
   #:encode-function-call-by-name
   #:decode-function-output-by-name
   #:decode-event-by-topic
   #:CallRequest #:.call-to #:.call-data

   ;; Contract write
   #:send-function-call))

(in-package #:web3/prelude)
(named-readtables:in-readtable coalton:coalton)
