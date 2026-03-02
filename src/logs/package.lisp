;;;; Event Log Querying package definition
;;;; Query historical event logs via eth_getLogs

(defpackage #:web3/logs
  (:documentation "Event log querying via eth_getLogs JSON-RPC method")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:addr #:web3/address)
   (#:receipt #:web3/receipt)
   (#:block #:web3/block)
   (#:provider #:web3/provider))
  (:export
   ;; Log filter type
   #:LogFilter
   #:.filter-from-block
   #:.filter-to-block
   #:.filter-address
   #:.filter-topics

   ;; Constructor
   #:make-log-filter

   ;; Query functions
   #:eth-get-logs
   #:get-logs-by-event
   #:get-logs-by-address))

(in-package #:web3/logs)
(named-readtables:in-readtable coalton:coalton)
