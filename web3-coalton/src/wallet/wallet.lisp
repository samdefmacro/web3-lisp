(in-package #:web3/wallet)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; Wallet type

  (define-type Wallet
    "Ethereum wallet: private key + optional provider"
    (%Wallet types:Bytes                    ; private key (32 bytes)
             (Optional provider:HttpProvider))) ; optional provider

  (declare make-wallet (types:Bytes -> Wallet))
  (define (make-wallet private-key)
    "Create a wallet from a private key (no provider)"
    (%Wallet private-key None))

  (declare wallet-with-provider (types:Bytes -> provider:HttpProvider -> Wallet))
  (define (wallet-with-provider private-key prov)
    "Create a wallet from a private key and provider"
    (%Wallet private-key (Some prov)))

  ;;; Wallet operations

  (declare %wallet-private-key (Wallet -> types:Bytes))
  (define (%wallet-private-key w)
    (match w ((%Wallet pk _) pk)))

  (declare %wallet-provider (Wallet -> (Optional provider:HttpProvider)))
  (define (%wallet-provider w)
    (match w ((%Wallet _ p) p)))

  (declare wallet-address (Wallet -> (types:Web3Result addr:Address)))
  (define (wallet-address w)
    "Get the Ethereum address of the wallet"
    (match (crypto:private-key-to-public-key (%wallet-private-key w))
      ((Err e) (Err e))
      ((Ok pub-key) (addr:address-from-public-key pub-key))))

  (declare wallet-sign-transaction (Wallet -> tx:Transaction -> (types:Web3Result types:Bytes)))
  (define (wallet-sign-transaction w transaction)
    "Sign a transaction with the wallet's private key"
    (tx:tx-sign transaction (%wallet-private-key w)))

  (declare wallet-send-transaction (Wallet -> tx:Transaction -> (types:Web3Result String)))
  (define (wallet-send-transaction w transaction)
    "Sign and send a transaction, returns transaction hash"
    (match (%wallet-provider w)
      ((None) (Err (types:WalletError "No provider configured")))
      ((Some prov)
       (match (wallet-sign-transaction w transaction)
         ((Err e) (Err e))
         ((Ok signed-tx)
          (provider:eth-send-raw-transaction prov signed-tx))))))

  (declare wallet-get-balance (Wallet -> (types:Web3Result types:U256)))
  (define (wallet-get-balance w)
    "Get the wallet's balance"
    (match (%wallet-provider w)
      ((None) (Err (types:WalletError "No provider configured")))
      ((Some prov)
       (match (wallet-address w)
         ((Err e) (Err e))
         ((Ok address)
          (provider:eth-get-balance prov address))))))

  (declare wallet-get-nonce (Wallet -> (types:Web3Result U64)))
  (define (wallet-get-nonce w)
    "Get the wallet's transaction count (nonce)"
    (match (%wallet-provider w)
      ((None) (Err (types:WalletError "No provider configured")))
      ((Some prov)
       (match (wallet-address w)
         ((Err e) (Err e))
         ((Ok address)
          (provider:eth-get-transaction-count prov address)))))))
