(in-package #:web3/wallet)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-struct Wallet
    "Ethereum wallet: private key + optional provider"
    (wallet-private-key types:Bytes)                     ; private key (32 bytes)
    (wallet-provider (Optional provider:HttpProvider)))  ; optional provider

  (declare make-wallet (types:Bytes -> Wallet))
  (define (make-wallet private-key)
    "Create a wallet from a private key (no provider)"
    (Wallet private-key None))

  (declare wallet-with-provider (types:Bytes -> provider:HttpProvider -> Wallet))
  (define (wallet-with-provider private-key prov)
    "Create a wallet from a private key and provider"
    (Wallet private-key (Some prov)))

  (declare wallet-address (Wallet -> (types:Web3Result addr:Address)))
  (define (wallet-address w)
    "Get the Ethereum address of the wallet"
    (do (pub-key <- (crypto:private-key-to-public-key (.wallet-private-key w)))
        (addr:address-from-public-key pub-key)))

  (declare wallet-sign-transaction (Wallet -> tx:Transaction -> (types:Web3Result types:Bytes)))
  (define (wallet-sign-transaction w transaction)
    "Sign a transaction with the wallet's private key"
    (tx:tx-sign transaction (.wallet-private-key w)))

  (declare %require-provider (Wallet -> (types:Web3Result provider:HttpProvider)))
  (define (%require-provider w)
    "Return the wallet's provider or a uniform missing-provider error."
    (match (.wallet-provider w)
      ((Some p) (Ok p))
      ((None)   (Err (types:WalletError "wallet: no provider configured")))))

  (declare wallet-send-transaction (Wallet -> tx:Transaction -> (types:Web3Result String)))
  (define (wallet-send-transaction w transaction)
    "Sign and send a transaction, returns transaction hash"
    (do (prov      <- (%require-provider w))
        (signed-tx <- (wallet-sign-transaction w transaction))
        (provider:eth-send-raw-transaction prov signed-tx)))

  (declare wallet-get-balance (Wallet -> (types:Web3Result types:U256)))
  (define (wallet-get-balance w)
    "Get the wallet's balance"
    (do (prov    <- (%require-provider w))
        (address <- (wallet-address w))
        (provider:eth-get-balance prov address)))

  (declare wallet-get-nonce (Wallet -> (types:Web3Result U64)))
  (define (wallet-get-nonce w)
    "Get the wallet's transaction count (nonce)"
    (do (prov    <- (%require-provider w))
        (address <- (wallet-address w))
        (provider:eth-get-transaction-count prov address))))
