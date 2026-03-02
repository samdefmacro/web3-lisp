;;;; Chain Configs implementation
;;;; Pre-configured blockchain network settings

(in-package #:web3/chain)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Native Currency Type
  ;;; =========================================================================

  (define-type NativeCurrency
    (NativeCurrency String   ; name (e.g., "Ether")
                    String   ; symbol (e.g., "ETH")
                    UFix))   ; decimals (e.g., 18)

  (declare make-native-currency (String -> String -> UFix -> NativeCurrency))
  (define (make-native-currency name symbol decimals)
    "Create a native currency configuration"
    (NativeCurrency name symbol decimals))

  (declare currency-name (NativeCurrency -> String))
  (define (currency-name curr)
    "Get the currency name"
    (match curr
      ((NativeCurrency name _ _) name)))

  (declare currency-symbol (NativeCurrency -> String))
  (define (currency-symbol curr)
    "Get the currency symbol"
    (match curr
      ((NativeCurrency _ sym _) sym)))

  (declare currency-decimals (NativeCurrency -> UFix))
  (define (currency-decimals curr)
    "Get the currency decimals"
    (match curr
      ((NativeCurrency _ _ dec) dec)))

  ;;; =========================================================================
  ;;; Chain Type
  ;;; =========================================================================

  (define-type Chain
    (Chain UFix            ; chain ID
           String          ; name
           String          ; short name
           NativeCurrency  ; native currency
           String          ; block explorer URL
           Boolean))       ; is testnet

  (declare make-chain (UFix -> String -> String -> NativeCurrency -> String -> Boolean -> Chain))
  (define (make-chain id name short-name currency explorer is-testnet)
    "Create a chain configuration"
    (Chain id name short-name currency explorer is-testnet))

  (declare chain-id (Chain -> UFix))
  (define (chain-id chain)
    "Get the chain ID"
    (match chain
      ((Chain id _ _ _ _ _) id)))

  (declare chain-name (Chain -> String))
  (define (chain-name chain)
    "Get the chain name"
    (match chain
      ((Chain _ name _ _ _ _) name)))

  (declare chain-short-name (Chain -> String))
  (define (chain-short-name chain)
    "Get the chain short name"
    (match chain
      ((Chain _ _ short _ _ _) short)))

  (declare chain-native-currency (Chain -> NativeCurrency))
  (define (chain-native-currency chain)
    "Get the native currency"
    (match chain
      ((Chain _ _ _ curr _ _) curr)))

  (declare chain-block-explorer (Chain -> String))
  (define (chain-block-explorer chain)
    "Get the block explorer URL"
    (match chain
      ((Chain _ _ _ _ explorer _) explorer)))

  (declare chain-is-testnet (Chain -> Boolean))
  (define (chain-is-testnet chain)
    "Check if chain is a testnet"
    (match chain
      ((Chain _ _ _ _ _ testnet) testnet)))

  ;;; =========================================================================
  ;;; Chain ID Constants
  ;;; =========================================================================

  (declare chain-id-mainnet UFix)
  (define chain-id-mainnet 1)

  (declare chain-id-sepolia UFix)
  (define chain-id-sepolia 11155111)

  (declare chain-id-holesky UFix)
  (define chain-id-holesky 17000)

  (declare chain-id-polygon UFix)
  (define chain-id-polygon 137)

  (declare chain-id-polygon-amoy UFix)
  (define chain-id-polygon-amoy 80002)

  (declare chain-id-arbitrum UFix)
  (define chain-id-arbitrum 42161)

  (declare chain-id-arbitrum-sepolia UFix)
  (define chain-id-arbitrum-sepolia 421614)

  (declare chain-id-optimism UFix)
  (define chain-id-optimism 10)

  (declare chain-id-optimism-sepolia UFix)
  (define chain-id-optimism-sepolia 11155420)

  (declare chain-id-base UFix)
  (define chain-id-base 8453)

  (declare chain-id-base-sepolia UFix)
  (define chain-id-base-sepolia 84532)

  (declare chain-id-bsc UFix)
  (define chain-id-bsc 56)

  (declare chain-id-bsc-testnet UFix)
  (define chain-id-bsc-testnet 97)

  (declare chain-id-avalanche UFix)
  (define chain-id-avalanche 43114)

  (declare chain-id-avalanche-fuji UFix)
  (define chain-id-avalanche-fuji 43113)

  (declare chain-id-gnosis UFix)
  (define chain-id-gnosis 100)

  (declare chain-id-fantom UFix)
  (define chain-id-fantom 250)

  (declare chain-id-celo UFix)
  (define chain-id-celo 42220)

  (declare chain-id-zksync UFix)
  (define chain-id-zksync 324)

  (declare chain-id-linea UFix)
  (define chain-id-linea 59144)

  (declare chain-id-scroll UFix)
  (define chain-id-scroll 534352)

  (declare chain-id-mantle UFix)
  (define chain-id-mantle 5000)

  (declare chain-id-blast UFix)
  (define chain-id-blast 81457)

  (declare chain-id-local UFix)
  (define chain-id-local 31337)

  ;;; =========================================================================
  ;;; Common Native Currencies
  ;;; =========================================================================

  (declare ether NativeCurrency)
  (define ether (NativeCurrency "Ether" "ETH" 18))

  (declare matic NativeCurrency)
  (define matic (NativeCurrency "MATIC" "MATIC" 18))

  (declare pol NativeCurrency)
  (define pol (NativeCurrency "POL" "POL" 18))

  (declare bnb NativeCurrency)
  (define bnb (NativeCurrency "BNB" "BNB" 18))

  (declare avax NativeCurrency)
  (define avax (NativeCurrency "Avalanche" "AVAX" 18))

  (declare xdai NativeCurrency)
  (define xdai (NativeCurrency "xDAI" "xDAI" 18))

  (declare ftm NativeCurrency)
  (define ftm (NativeCurrency "Fantom" "FTM" 18))

  (declare celo-currency NativeCurrency)
  (define celo-currency (NativeCurrency "CELO" "CELO" 18))

  (declare mnt NativeCurrency)
  (define mnt (NativeCurrency "Mantle" "MNT" 18))

  ;;; =========================================================================
  ;;; Pre-configured Chains - Ethereum
  ;;; =========================================================================

  (declare ethereum-mainnet Chain)
  (define ethereum-mainnet
    (Chain chain-id-mainnet
           "Ethereum"
           "eth"
           ether
           "https://etherscan.io"
           False))

  (declare sepolia Chain)
  (define sepolia
    (Chain chain-id-sepolia
           "Sepolia"
           "sep"
           ether
           "https://sepolia.etherscan.io"
           True))

  (declare holesky Chain)
  (define holesky
    (Chain chain-id-holesky
           "Holesky"
           "holesky"
           ether
           "https://holesky.etherscan.io"
           True))

  ;;; =========================================================================
  ;;; Pre-configured Chains - Polygon
  ;;; =========================================================================

  (declare polygon Chain)
  (define polygon
    (Chain chain-id-polygon
           "Polygon"
           "matic"
           pol
           "https://polygonscan.com"
           False))

  (declare polygon-amoy Chain)
  (define polygon-amoy
    (Chain chain-id-polygon-amoy
           "Polygon Amoy"
           "amoy"
           pol
           "https://amoy.polygonscan.com"
           True))

  ;;; =========================================================================
  ;;; Pre-configured Chains - Arbitrum
  ;;; =========================================================================

  (declare arbitrum-one Chain)
  (define arbitrum-one
    (Chain chain-id-arbitrum
           "Arbitrum One"
           "arb1"
           ether
           "https://arbiscan.io"
           False))

  (declare arbitrum-sepolia Chain)
  (define arbitrum-sepolia
    (Chain chain-id-arbitrum-sepolia
           "Arbitrum Sepolia"
           "arb-sep"
           ether
           "https://sepolia.arbiscan.io"
           True))

  ;;; =========================================================================
  ;;; Pre-configured Chains - Optimism
  ;;; =========================================================================

  (declare optimism Chain)
  (define optimism
    (Chain chain-id-optimism
           "OP Mainnet"
           "oeth"
           ether
           "https://optimistic.etherscan.io"
           False))

  (declare optimism-sepolia Chain)
  (define optimism-sepolia
    (Chain chain-id-optimism-sepolia
           "OP Sepolia"
           "op-sep"
           ether
           "https://sepolia-optimism.etherscan.io"
           True))

  ;;; =========================================================================
  ;;; Pre-configured Chains - Base
  ;;; =========================================================================

  (declare base Chain)
  (define base
    (Chain chain-id-base
           "Base"
           "base"
           ether
           "https://basescan.org"
           False))

  (declare base-sepolia Chain)
  (define base-sepolia
    (Chain chain-id-base-sepolia
           "Base Sepolia"
           "base-sep"
           ether
           "https://sepolia.basescan.org"
           True))

  ;;; =========================================================================
  ;;; Pre-configured Chains - BSC
  ;;; =========================================================================

  (declare bsc Chain)
  (define bsc
    (Chain chain-id-bsc
           "BNB Smart Chain"
           "bnb"
           bnb
           "https://bscscan.com"
           False))

  (declare bsc-testnet Chain)
  (define bsc-testnet
    (Chain chain-id-bsc-testnet
           "BNB Smart Chain Testnet"
           "bnbt"
           bnb
           "https://testnet.bscscan.com"
           True))

  ;;; =========================================================================
  ;;; Pre-configured Chains - Avalanche
  ;;; =========================================================================

  (declare avalanche Chain)
  (define avalanche
    (Chain chain-id-avalanche
           "Avalanche C-Chain"
           "avax"
           avax
           "https://snowtrace.io"
           False))

  (declare avalanche-fuji Chain)
  (define avalanche-fuji
    (Chain chain-id-avalanche-fuji
           "Avalanche Fuji"
           "fuji"
           avax
           "https://testnet.snowtrace.io"
           True))

  ;;; =========================================================================
  ;;; Pre-configured Chains - Other L1s
  ;;; =========================================================================

  (declare gnosis Chain)
  (define gnosis
    (Chain chain-id-gnosis
           "Gnosis"
           "gno"
           xdai
           "https://gnosisscan.io"
           False))

  (declare fantom Chain)
  (define fantom
    (Chain chain-id-fantom
           "Fantom Opera"
           "ftm"
           ftm
           "https://ftmscan.com"
           False))

  (declare celo Chain)
  (define celo
    (Chain chain-id-celo
           "Celo"
           "celo"
           celo-currency
           "https://celoscan.io"
           False))

  ;;; =========================================================================
  ;;; Pre-configured Chains - Other L2s
  ;;; =========================================================================

  (declare zksync-era Chain)
  (define zksync-era
    (Chain chain-id-zksync
           "zkSync Era"
           "zksync"
           ether
           "https://explorer.zksync.io"
           False))

  (declare linea Chain)
  (define linea
    (Chain chain-id-linea
           "Linea"
           "linea"
           ether
           "https://lineascan.build"
           False))

  (declare scroll Chain)
  (define scroll
    (Chain chain-id-scroll
           "Scroll"
           "scroll"
           ether
           "https://scrollscan.com"
           False))

  (declare mantle Chain)
  (define mantle
    (Chain chain-id-mantle
           "Mantle"
           "mantle"
           mnt
           "https://explorer.mantle.xyz"
           False))

  (declare blast Chain)
  (define blast
    (Chain chain-id-blast
           "Blast"
           "blast"
           ether
           "https://blastscan.io"
           False))

  ;;; =========================================================================
  ;;; Pre-configured Chains - Local Development
  ;;; =========================================================================

  (declare localhost Chain)
  (define localhost
    (Chain chain-id-local
           "Localhost"
           "local"
           ether
           ""
           True))

  (declare hardhat Chain)
  (define hardhat
    (Chain chain-id-local
           "Hardhat"
           "hardhat"
           ether
           ""
           True))

  (declare anvil Chain)
  (define anvil
    (Chain chain-id-local
           "Anvil"
           "anvil"
           ether
           ""
           True))

  ;;; =========================================================================
  ;;; Chain Lists
  ;;; =========================================================================

  (declare all-chains (List Chain))
  (define all-chains
    "Get all pre-configured chains"
    (Cons ethereum-mainnet
          (Cons sepolia
                (Cons holesky
                      (Cons polygon
                            (Cons polygon-amoy
                                  (Cons arbitrum-one
                                        (Cons arbitrum-sepolia
                                              (Cons optimism
                                                    (Cons optimism-sepolia
                                                          (Cons base
                                                                (Cons base-sepolia
                                                                      (Cons bsc
                                                                            (Cons bsc-testnet
                                                                                  (Cons avalanche
                                                                                        (Cons avalanche-fuji
                                                                                              (Cons gnosis
                                                                                                    (Cons fantom
                                                                                                          (Cons celo
                                                                                                                (Cons zksync-era
                                                                                                                      (Cons linea
                                                                                                                            (Cons scroll
                                                                                                                                  (Cons mantle
                                                                                                                                        (Cons blast
                                                                                                                                              (Cons localhost
                                                                                                                                                    (Cons hardhat
                                                                                                                                                          (Cons anvil Nil)))))))))))))))))))))))))))

  (declare mainnet-chains (List Chain))
  (define mainnet-chains
    "Get all mainnet chains"
    (filter (fn (c) (not (chain-is-testnet c))) all-chains))

  (declare testnet-chains (List Chain))
  (define testnet-chains
    "Get all testnet chains"
    (filter chain-is-testnet all-chains))

  ;;; =========================================================================
  ;;; Lookup Functions
  ;;; =========================================================================

  (declare get-chain-by-id (UFix -> (Optional Chain)))
  (define (get-chain-by-id id)
    "Look up a chain by its chain ID"
    (find (fn (c) (== (chain-id c) id)) all-chains))

  (declare get-chain-by-name (String -> (Optional Chain)))
  (define (get-chain-by-name name)
    "Look up a chain by name (case-insensitive)"
    (let ((lower-name (types:string-downcase name)))
      (find (fn (c)
              (or (== (types:string-downcase (chain-name c)) lower-name)
                  (== (types:string-downcase (chain-short-name c)) lower-name)))
            all-chains)))

  ;;; =========================================================================
  ;;; Utility Functions
  ;;; =========================================================================

  (declare is-eip1559-chain (UFix -> Boolean))
  (define (is-eip1559-chain id)
    "Check if a chain supports EIP-1559 (most modern chains do)"
    ;; BSC doesn't use EIP-1559 style fees
    (not (or (== id chain-id-bsc)
             (== id chain-id-bsc-testnet))))

  (declare default-block-time (UFix -> UFix))
  (define (default-block-time id)
    "Get the approximate block time in seconds for a chain"
    (cond
      ((== id chain-id-mainnet) 12)
      ((== id chain-id-polygon) 2)
      ((== id chain-id-bsc) 3)
      ((== id chain-id-avalanche) 2)
      ((== id chain-id-fantom) 1)
      ((== id chain-id-arbitrum) 1)  ; L2, very fast
      ((== id chain-id-optimism) 2)
      ((== id chain-id-base) 2)
      ((== id chain-id-gnosis) 5)
      (True 12)))  ; Default to Ethereum-like

  (declare explorer-tx-url (Chain -> String -> String))
  (define (explorer-tx-url chain tx-hash)
    "Build a transaction URL for the block explorer"
    (let ((base (chain-block-explorer chain)))
      (if (== base "")
          ""
          (lisp String (base tx-hash)
            (cl:format cl:nil "~A/tx/~A" base tx-hash)))))

  (declare explorer-address-url (Chain -> String -> String))
  (define (explorer-address-url chain address)
    "Build an address URL for the block explorer"
    (let ((base (chain-block-explorer chain)))
      (if (== base "")
          ""
          (lisp String (base address)
            (cl:format cl:nil "~A/address/~A" base address)))))

  (declare explorer-block-url (Chain -> UFix -> String))
  (define (explorer-block-url chain block-number)
    "Build a block URL for the block explorer"
    (let ((base (chain-block-explorer chain)))
      (if (== base "")
          ""
          (lisp String (base block-number)
            (cl:format cl:nil "~A/block/~A" base block-number)))))

  ;;; =========================================================================
  ;;; Helper Functions
  ;;; =========================================================================

)


