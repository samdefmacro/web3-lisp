;;;; Gas Utilities package definition
;;;; EIP-1559 fee calculation, gas estimation, and fee suggestions

(defpackage #:web3/gas
  (:documentation "Gas utilities for EIP-1559 fee calculation and estimation")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:list #:coalton-library/list)
   (#:math #:coalton-library/math))
  (:export
   ;; Fee types
   #:GasFees
   #:make-gas-fees
   #:.max-fee-per-gas
   #:.max-priority-fee-per-gas

   #:LegacyGasPrice
   #:make-legacy-gas-price
   #:.gas-price

   #:FeeEstimate
   #:Eip1559Fees
   #:LegacyFees

   ;; Fee history
   #:FeeHistoryBlock
   #:make-fee-history-block
   #:.block-base-fee
   #:.block-gas-used-ratio
   #:.block-priority-fees

   #:FeeHistory
   #:make-fee-history
   #:.history-oldest-block
   #:.history-blocks
   #:.history-base-fee-next

   ;; Base fee calculation
   #:calculate-next-base-fee
   #:estimate-base-fee-trend

   ;; Priority fee suggestions
   #:suggest-priority-fee
   #:priority-fee-percentile

   ;; Complete fee suggestions
   #:suggest-fees
   #:suggest-fees-from-history

   ;; Fee presets
   #:FeePreset
   #:FeeSlow
   #:FeeNormal
   #:FeeFast
   #:FeeInstant
   #:preset-multiplier

   ;; Gas limit helpers
   #:add-gas-buffer
   #:standard-gas-limits

   ;; Unit conversions (re-export convenience)
   #:gwei-to-wei
   #:wei-to-gwei

   ;; Fee comparison
   #:fees-sufficient?
   #:compare-fees

   ;; JSON encoding for RPC
   #:encode-fee-history-request
   #:parse-fee-history-response
   #:parse-gas-price-response))

(in-package #:web3/gas)
(named-readtables:in-readtable coalton:coalton)
