;;;; WebSocket Provider package definition
;;;; Real-time Ethereum subscriptions over WebSocket

(defpackage #:web3/ws-provider
  (:documentation "WebSocket provider for real-time Ethereum subscriptions")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:addr #:web3/address))
  (:export
   ;; Subscription types
   #:SubscriptionType
   #:SubNewHeads
   #:SubNewPendingTransactions
   #:SubLogs
   #:SubSyncing

   ;; Log filter for subscriptions
   #:LogFilter
   #:make-log-filter
   #:.filter-address
   #:.filter-topics

   ;; Subscription result types
   #:BlockHeader
   #:.header-number
   #:.header-hash
   #:.header-parent-hash
   #:.header-timestamp
   #:.header-gas-limit
   #:.header-gas-used
   #:.header-base-fee

   #:LogEntry
   #:.log-address
   #:.log-topics
   #:.log-data
   #:.log-block-number
   #:.log-tx-hash
   #:.log-tx-index
   #:.log-block-hash
   #:.log-log-index

   #:SyncStatus
   #:Syncing
   #:NotSyncing
   #:.sync-starting-block
   #:.sync-current-block
   #:.sync-highest-block
   #:sync-starting-block
   #:sync-current-block
   #:sync-highest-block

   ;; Subscription
   #:Subscription
   #:.subscription-id
   #:.subscription-type

   ;; Message types for handling
   #:WsMessage
   #:WsSubscriptionData
   #:WsUnsubscribed
   #:WsError

   ;; Encoding subscription requests
   #:encode-subscribe-request
   #:encode-unsubscribe-request
   #:parse-subscription-response
   #:parse-subscription-notification

   ;; Parsing helpers
   #:parse-block-header
   #:parse-log-entry
   #:parse-sync-status

   ;; Connection state (for CL-level use)
   #:ws-connection-state
   #:make-ws-connection-state
   #:ws-connection-state-url
   #:ws-connection-state-subscriptions
   #:ws-connection-state-next-id
   #:ws-state-add-subscription
   #:ws-state-remove-subscription
   #:ws-state-get-subscription
   #:ws-state-next-request-id

   ;; WebSocket I/O (CL-level)
   #:ws-provider
   #:make-ws-provider
   #:ws-provider-connected-p
   #:ws-connect
   #:ws-close
   #:ws-subscribe
   #:ws-unsubscribe))

(in-package #:web3/ws-provider)
(named-readtables:in-readtable coalton:coalton)
