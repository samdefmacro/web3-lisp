(defpackage #:web3/events
  (:documentation "Ethereum event log parsing and decoding")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:addr #:web3/address)
   (#:crypto #:web3/crypto)
   (#:abi #:web3/abi))
  (:export
   ;; Event log type
   #:EventLog
   #:make-EventLog
   #:.address
   #:.topics
   #:.data
   #:.block-number
   #:.tx-hash
   #:.log-index

   ;; Event signature
   #:event-signature
   #:event-topic

   ;; Decoding functions
   #:decode-event
   #:decode-indexed-address
   #:decode-indexed-uint256
   #:decode-non-indexed-params
   #:matches-event-signature?

   ;; Common event signatures
   #:erc20-transfer-topic
   #:erc20-approval-topic
   #:erc721-transfer-topic
   #:erc721-approval-topic
   #:erc721-approval-for-all-topic
   #:erc1155-transfer-single-topic
   #:erc1155-transfer-batch-topic
   #:erc1155-approval-for-all-topic

   ;; Decoded event types
   #:DecodedEvent
   #:Erc20Transfer
   #:Erc20Approval
   #:Erc721Transfer
   #:Erc721Approval
   #:Erc1155TransferSingle
   #:UnknownEvent

   ;; High-level decoders
   #:decode-erc20-transfer
   #:decode-erc20-approval
   #:decode-erc721-transfer
   #:decode-erc1155-transfer-single))

(in-package #:web3/events)
(named-readtables:in-readtable coalton:coalton)
