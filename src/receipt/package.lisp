;;;; Receipt Parsing package definition
;;;; Parse Ethereum transaction receipts from JSON-RPC responses

(defpackage #:web3/receipt
  (:documentation "Transaction receipt parsing for Ethereum")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:addr #:web3/address)
   (#:list #:coalton-library/list))
  (:export
   ;; Log entry type
   #:LogEntry
   #:make-log-entry
   #:.log-address
   #:.log-topics
   #:.log-data
   #:.log-block-number
   #:.log-transaction-hash
   #:.log-transaction-index
   #:.log-block-hash
   #:.log-log-index
   #:.log-removed

   ;; Receipt type
   #:Receipt
   #:make-receipt
   #:.receipt-transaction-hash
   #:.receipt-transaction-index
   #:.receipt-block-hash
   #:.receipt-block-number
   #:.receipt-from
   #:.receipt-to
   #:.receipt-cumulative-gas-used
   #:.receipt-gas-used
   #:.receipt-effective-gas-price
   #:.receipt-contract-address
   #:.receipt-logs
   #:.receipt-logs-bloom
   #:.receipt-status
   #:.receipt-type

   ;; Receipt status
   #:ReceiptStatus
   #:StatusSuccess
   #:StatusFailed
   #:StatusUnknown

   ;; Transaction type
   #:TxType
   #:TxTypeLegacy
   #:TxTypeAccessList
   #:TxTypeEip1559
   #:TxTypeBlob

   ;; Parsing functions
   #:parse-receipt
   #:parse-log-entry

   ;; Query helpers
   #:receipt-success?
   #:receipt-failed?
   #:receipt-contract-created?
   #:receipt-total-cost
   #:filter-logs-by-address
   #:filter-logs-by-topic

   ;; JSON-RPC helpers
   #:encode-get-receipt-request
   #:parse-get-receipt-response))

(in-package #:web3/receipt)
(named-readtables:in-readtable coalton:coalton)
