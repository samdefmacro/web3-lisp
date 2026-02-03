;;;; Units package definition
;;;; Ethereum unit conversions with custom decimals

(defpackage #:web3/units
  (:documentation "Ethereum unit conversions - parseUnits/formatUnits with custom decimals")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:str #:coalton-library/string))
  (:export
   ;; Core unit functions
   #:parse-units
   #:format-units

   ;; Ether convenience (18 decimals)
   #:parse-ether
   #:format-ether

   ;; Gwei convenience (9 decimals)
   #:parse-gwei
   #:format-gwei))

(in-package #:web3/units)
(named-readtables:in-readtable coalton:coalton)
