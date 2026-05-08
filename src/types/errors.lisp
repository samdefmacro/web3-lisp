(in-package #:web3/types)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-type Web3Error
    "Errors that can occur during Web3 operations"
    (HexError String)
    (RlpError String)
    (CryptoError String)
    (AddressError String)
    (AbiError String)
    (TransactionError String)
    (ProviderError String)
    (WalletError String))

  (define-type-alias (Web3Result :a) (Result Web3Error :a))

  ;;; Result combinators

  (declare traverse-result-list ((List (Result :e :a)) -> (Result :e (List :a))))
  (define (traverse-result-list xs)
    "Sequence a list of Results: Ok of all values, or the first Err."
    (match xs
      ((Nil) (Ok Nil))
      ((Cons head tail)
       (do (h <- head)
           (rest <- (traverse-result-list tail))
           (Ok (Cons h rest)))))))
