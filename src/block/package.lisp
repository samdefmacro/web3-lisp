;;;; Block Parsing package definition
;;;; Parse Ethereum blocks and block headers from JSON-RPC

(defpackage #:web3/block
  (:documentation "Ethereum block and header parsing from JSON-RPC responses")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:addr #:web3/address))
  (:export
   ;; Block Header type
   #:BlockHeader
   #:make-block-header
   #:.header-number
   #:.header-hash
   #:.header-parent-hash
   #:.header-nonce
   #:.header-sha3-uncles
   #:.header-logs-bloom
   #:.header-transactions-root
   #:.header-state-root
   #:.header-receipts-root
   #:.header-miner
   #:.header-difficulty
   #:.header-total-difficulty
   #:.header-extra-data
   #:.header-size
   #:.header-gas-limit
   #:.header-gas-used
   #:.header-timestamp
   #:.header-base-fee
   #:.header-withdrawals-root
   #:.header-blob-gas-used
   #:.header-excess-blob-gas

   ;; Full Block type
   #:Block
   #:make-block
   #:.block-header
   #:.block-transactions
   #:.block-uncles
   #:.block-withdrawals

   ;; Transaction in block (can be hash or full)
   #:BlockTx
   #:TxHash
   #:TxFull

   ;; Withdrawal type
   #:Withdrawal
   #:make-withdrawal
   #:.withdrawal-index
   #:.withdrawal-validator-index
   #:.withdrawal-address
   #:.withdrawal-amount

   ;; Parsing functions
   #:parse-block-header
   #:parse-block
   #:parse-withdrawal

   ;; JSON-RPC request encoding
   #:encode-get-block-by-number-request
   #:encode-get-block-by-hash-request
   #:parse-get-block-response

   ;; Block tag types
   #:BlockTag
   #:TagLatest
   #:TagPending
   #:TagFinalized
   #:TagSafe
   #:TagEarliest
   #:TagNumber
   #:block-tag-to-string

   ;; Utility functions
   #:is-post-merge
   #:is-post-shanghai
   #:is-post-cancun
   #:block-age
   #:gas-utilization))

(in-package #:web3/block)
(named-readtables:in-readtable coalton:coalton)
