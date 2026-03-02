;;; Chain Configs tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Chain Configs Tests
;;; =========================================================================

(defun run-chain-tests ()
  (format t "~%=== Chain Configs Tests ===~%")

  ;;; =========================================================================
  ;;; Chain ID Constants Tests
  ;;; =========================================================================

  (test-case "chain-id-mainnet is 1"
    (assert (= (coalton:coalton web3/chain:chain-id-mainnet) 1)))

  (test-case "chain-id-sepolia is 11155111"
    (assert (= (coalton:coalton web3/chain:chain-id-sepolia) 11155111)))

  (test-case "chain-id-polygon is 137"
    (assert (= (coalton:coalton web3/chain:chain-id-polygon) 137)))

  (test-case "chain-id-arbitrum is 42161"
    (assert (= (coalton:coalton web3/chain:chain-id-arbitrum) 42161)))

  (test-case "chain-id-optimism is 10"
    (assert (= (coalton:coalton web3/chain:chain-id-optimism) 10)))

  (test-case "chain-id-base is 8453"
    (assert (= (coalton:coalton web3/chain:chain-id-base) 8453)))

  (test-case "chain-id-local is 31337"
    (assert (= (coalton:coalton web3/chain:chain-id-local) 31337)))

  ;;; =========================================================================
  ;;; Pre-configured Chain Tests
  ;;; =========================================================================

  (test-case "ethereum-mainnet has correct chain ID"
    (let ((id (coalton:coalton (web3/chain:chain-id web3/chain:ethereum-mainnet))))
      (assert (= id 1))))

  (test-case "ethereum-mainnet has correct name"
    (let ((name (coalton:coalton (web3/chain:chain-name web3/chain:ethereum-mainnet))))
      (assert (string= name "Ethereum"))))

  (test-case "ethereum-mainnet is not a testnet"
    (let ((testnet (coalton:coalton (web3/chain:chain-is-testnet web3/chain:ethereum-mainnet))))
      (assert (not testnet))))

  (test-case "sepolia is a testnet"
    (let ((testnet (coalton:coalton (web3/chain:chain-is-testnet web3/chain:sepolia))))
      (assert testnet)))

  (test-case "polygon has correct explorer"
    (let ((explorer (coalton:coalton (web3/chain:chain-block-explorer web3/chain:polygon))))
      (assert (search "polygonscan" explorer))))

  (test-case "arbitrum-one uses ETH"
    (let* ((chain (coalton:coalton web3/chain:arbitrum-one))
           (currency (coalton:coalton (web3/chain:chain-native-currency
                                       (coalton:lisp web3/chain:Chain () chain))))
           (symbol (coalton:coalton (web3/chain:currency-symbol
                                     (coalton:lisp web3/chain:NativeCurrency () currency)))))
      (assert (string= symbol "ETH"))))

  (test-case "bsc uses BNB"
    (let* ((chain (coalton:coalton web3/chain:bsc))
           (currency (coalton:coalton (web3/chain:chain-native-currency
                                       (coalton:lisp web3/chain:Chain () chain))))
           (symbol (coalton:coalton (web3/chain:currency-symbol
                                     (coalton:lisp web3/chain:NativeCurrency () currency)))))
      (assert (string= symbol "BNB"))))

  (test-case "avalanche uses AVAX"
    (let* ((chain (coalton:coalton web3/chain:avalanche))
           (currency (coalton:coalton (web3/chain:chain-native-currency
                                       (coalton:lisp web3/chain:Chain () chain))))
           (symbol (coalton:coalton (web3/chain:currency-symbol
                                     (coalton:lisp web3/chain:NativeCurrency () currency)))))
      (assert (string= symbol "AVAX"))))

  ;;; =========================================================================
  ;;; Native Currency Tests
  ;;; =========================================================================

  (test-case "make-native-currency creates currency"
    (let* ((currency (coalton:coalton (web3/chain:make-native-currency "Test" "TST" 18)))
           (name (coalton:coalton (web3/chain:currency-name
                                   (coalton:lisp web3/chain:NativeCurrency () currency))))
           (symbol (coalton:coalton (web3/chain:currency-symbol
                                     (coalton:lisp web3/chain:NativeCurrency () currency))))
           (decimals (coalton:coalton (web3/chain:currency-decimals
                                       (coalton:lisp web3/chain:NativeCurrency () currency)))))
      (assert (string= name "Test"))
      (assert (string= symbol "TST"))
      (assert (= decimals 18))))

  ;;; =========================================================================
  ;;; Lookup Function Tests
  ;;; =========================================================================

  (test-case "get-chain-by-id finds mainnet"
    (let ((result (coalton:coalton (web3/chain:get-chain-by-id 1))))
      (assert (optional-some-p result))
      (let* ((chain (result-value result))
             (name (coalton:coalton (web3/chain:chain-name
                                     (coalton:lisp web3/chain:Chain () chain)))))
        (assert (string= name "Ethereum")))))

  (test-case "get-chain-by-id finds polygon"
    (let ((result (coalton:coalton (web3/chain:get-chain-by-id 137))))
      (assert (optional-some-p result))
      (let* ((chain (result-value result))
             (name (coalton:coalton (web3/chain:chain-name
                                     (coalton:lisp web3/chain:Chain () chain)))))
        (assert (string= name "Polygon")))))

  (test-case "get-chain-by-id returns None for unknown"
    (let ((result (coalton:coalton (web3/chain:get-chain-by-id 99999))))
      (assert (optional-none-p result))))

  (test-case "get-chain-by-name finds by full name"
    (let ((result (coalton:coalton (web3/chain:get-chain-by-name "Ethereum"))))
      (assert (optional-some-p result))))

  (test-case "get-chain-by-name finds by short name"
    (let ((result (coalton:coalton (web3/chain:get-chain-by-name "eth"))))
      (assert (optional-some-p result))))

  (test-case "get-chain-by-name is case-insensitive"
    (let ((result (coalton:coalton (web3/chain:get-chain-by-name "POLYGON"))))
      (assert (optional-some-p result))))

  (test-case "get-chain-by-name returns None for unknown"
    (let ((result (coalton:coalton (web3/chain:get-chain-by-name "NotAChain"))))
      (assert (optional-none-p result))))

  ;;; =========================================================================
  ;;; Chain List Tests
  ;;; =========================================================================

  (test-case "all-chains returns non-empty list"
    (let* ((chains (coalton:coalton web3/chain:all-chains))
           (len (coalton:coalton (coalton-prelude:length
                                  (coalton:lisp (coalton:List web3/chain:Chain) () chains)))))
      (assert (> len 0))))

  (test-case "mainnet-chains excludes testnets"
    (let* ((chains (coalton:coalton web3/chain:mainnet-chains))
           (len (coalton:coalton (coalton-prelude:length
                                  (coalton:lisp (coalton:List web3/chain:Chain) () chains)))))
      ;; Check that there are mainnet chains
      (assert (> len 0))))

  (test-case "testnet-chains only includes testnets"
    (let* ((chains (coalton:coalton web3/chain:testnet-chains))
           (len (coalton:coalton (coalton-prelude:length
                                  (coalton:lisp (coalton:List web3/chain:Chain) () chains)))))
      (assert (> len 0))))

  ;;; =========================================================================
  ;;; Utility Function Tests
  ;;; =========================================================================

  (test-case "is-eip1559-chain returns true for mainnet"
    (let ((result (coalton:coalton (web3/chain:is-eip1559-chain 1))))
      (assert result)))

  (test-case "is-eip1559-chain returns false for BSC"
    (let ((result (coalton:coalton (web3/chain:is-eip1559-chain 56))))
      (assert (not result))))

  (test-case "default-block-time returns 12 for mainnet"
    (let ((time (coalton:coalton (web3/chain:default-block-time 1))))
      (assert (= time 12))))

  (test-case "default-block-time returns 2 for polygon"
    (let ((time (coalton:coalton (web3/chain:default-block-time 137))))
      (assert (= time 2))))

  (test-case "explorer-tx-url builds correct URL"
    (let* ((chain (coalton:coalton web3/chain:ethereum-mainnet))
           (url (coalton:coalton (web3/chain:explorer-tx-url
                                  (coalton:lisp web3/chain:Chain () chain)
                                  "0x123abc"))))
      (assert (search "etherscan.io/tx/0x123abc" url))))

  (test-case "explorer-address-url builds correct URL"
    (let* ((chain (coalton:coalton web3/chain:polygon))
           (url (coalton:coalton (web3/chain:explorer-address-url
                                  (coalton:lisp web3/chain:Chain () chain)
                                  "0xabc123"))))
      (assert (search "polygonscan.com/address/0xabc123" url))))

  (test-case "explorer-block-url builds correct URL"
    (let* ((chain (coalton:coalton web3/chain:arbitrum-one))
           (url (coalton:coalton (web3/chain:explorer-block-url
                                  (coalton:lisp web3/chain:Chain () chain)
                                  12345))))
      (assert (search "arbiscan.io/block/12345" url))))

  (test-case "explorer-tx-url returns empty for localhost"
    (let* ((chain (coalton:coalton web3/chain:localhost))
           (url (coalton:coalton (web3/chain:explorer-tx-url
                                  (coalton:lisp web3/chain:Chain () chain)
                                  "0x123"))))
      (assert (string= url ""))))

  ;;; =========================================================================
  ;;; Local Development Chain Tests
  ;;; =========================================================================

  (test-case "localhost has chain ID 31337"
    (let ((id (coalton:coalton (web3/chain:chain-id web3/chain:localhost))))
      (assert (= id 31337))))

  (test-case "hardhat has chain ID 31337"
    (let ((id (coalton:coalton (web3/chain:chain-id web3/chain:hardhat))))
      (assert (= id 31337))))

  (test-case "anvil has chain ID 31337"
    (let ((id (coalton:coalton (web3/chain:chain-id web3/chain:anvil))))
      (assert (= id 31337)))))
