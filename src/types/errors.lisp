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

  (define-type-alias (Web3Result :a) (Result Web3Error :a)))
