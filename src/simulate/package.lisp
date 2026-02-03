(defpackage #:web3/simulate
  (:documentation "Transaction simulation and gas estimation helpers")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:addr #:web3/address)
   (#:tx #:web3/transaction)
   (#:provider #:web3/provider)
   (#:gas #:web3/gas))
  (:export
   ;; Block specifier
   #:BlockSpec
   #:BlockLatest
   #:BlockPending
   #:BlockEarliest
   #:BlockNumber
   #:block-spec-to-string

   ;; Call options
   #:CallOptions
   #:make-call-options
   #:default-call-options
   #:.call-options-block
   #:.call-options-from
   #:.call-options-gas-limit
   #:.call-options-gas-price
   #:.call-options-value

   ;; Simulation result
   #:SimulationResult
   #:make-simulation-result
   #:.simulation-return-data
   #:.simulation-gas-used
   #:.simulation-success

   ;; Gas estimate result
   #:GasEstimate
   #:make-gas-estimate
   #:.gas-estimate-gas-limit
   #:.gas-estimate-gas-price
   #:.gas-estimate-max-fee
   #:.gas-estimate-max-priority-fee
   #:.gas-estimate-total-cost

   ;; Access list result
   #:AccessListResult
   #:make-access-list-result
   #:.access-list-result-list
   #:.access-list-result-gas-used

   ;; Core functions
   #:simulate-call
   #:simulate-transaction
   #:estimate-gas
   #:estimate-transaction-gas
   #:estimate-transaction-cost
   #:create-access-list
   #:populate-transaction))

(in-package #:web3/simulate)
(named-readtables:in-readtable coalton:coalton)
