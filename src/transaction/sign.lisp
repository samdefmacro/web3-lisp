(in-package #:web3/transaction)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare tx-sign (Transaction -> types:Bytes -> (types:Web3Result types:Bytes)))
  (define (tx-sign tx private-key)
    "Sign a transaction with a private key and return the raw signed transaction bytes"
    (let ((unsigned (tx-encode-for-signing tx)))
      (let ((hash (crypto:keccak256 unsigned)))
        (match (crypto:sign-hash hash private-key)
          ((Err e) (Err e))
          ((Ok sig) (Ok (signed-tx-encode tx sig))))))))
