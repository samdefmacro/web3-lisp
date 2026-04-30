(in-package #:web3/contract-write)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; High-level "encode calldata, populate tx, sign, send" flow for contract
  ;;; writes. Uses EIP-1559 transactions populated from on-chain values
  ;;; (nonce, gas estimate, fee history) via web3/simulate:populate-transaction.

  (declare send-function-call (wallet:Wallet
                               -> contract:Contract
                               -> String
                               -> (List abi:AbiValue)
                               -> types:U256
                               -> (types:Web3Result String)))
  (define (send-function-call w contract-inst fn-name args value)
    "Encode a contract function call, populate an EIP-1559 transaction,
     sign with the wallet, and broadcast. Returns the transaction hash.
     Wallet must have a provider configured."
    (match (contract:encode-function-call-by-name contract-inst fn-name args)
      ((Err e) (Err e))
      ((Ok data)
       (send-raw-call w (contract:.contract-address contract-inst) value data))))

  (declare send-function-call-with-builder (wallet:Wallet
                                            -> contract:CallBuilder
                                            -> types:U256
                                            -> (types:Web3Result String)))
  (define (send-function-call-with-builder w builder value)
    "Send a transaction built with a CallBuilder. Returns transaction hash."
    (match (contract:build-call-request builder)
      ((contract:CallRequest to data)
       (send-raw-call w to value data))))

  (declare send-raw-call (wallet:Wallet
                          -> addr:Address
                          -> types:U256
                          -> types:Bytes
                          -> (types:Web3Result String)))
  (define (send-raw-call w to value data)
    "Send a transaction with the given calldata to an address.
     Lower-level helper used by send-function-call. Returns transaction hash."
    (match (.wallet-provider w)
      ((None) (Err (types:WalletError "No provider configured")))
      ((Some prov)
       (match (wallet:wallet-address w)
         ((Err e) (Err e))
         ((Ok from)
          (match (simulate:populate-transaction prov from tx:EIP1559Tx
                                                (Some to) value data)
            ((Err e) (Err e))
            ((Ok transaction)
             (wallet:wallet-send-transaction w transaction)))))))))
