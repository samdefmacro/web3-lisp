(in-package #:web3/erc20)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; ERC-20 Function Selectors
  ;;; =========================================================================

  (declare selector-name types:Bytes)
  (define selector-name
    "Function selector for name() -> 0x06fdde03"
    (abi:function-selector "name()"))

  (declare selector-symbol types:Bytes)
  (define selector-symbol
    "Function selector for symbol() -> 0x95d89b41"
    (abi:function-selector "symbol()"))

  (declare selector-decimals types:Bytes)
  (define selector-decimals
    "Function selector for decimals() -> 0x313ce567"
    (abi:function-selector "decimals()"))

  (declare selector-total-supply types:Bytes)
  (define selector-total-supply
    "Function selector for totalSupply() -> 0x18160ddd"
    (abi:function-selector "totalSupply()"))

  (declare selector-balance-of types:Bytes)
  (define selector-balance-of
    "Function selector for balanceOf(address) -> 0x70a08231"
    (abi:function-selector "balanceOf(address)"))

  (declare selector-allowance types:Bytes)
  (define selector-allowance
    "Function selector for allowance(address,address) -> 0xdd62ed3e"
    (abi:function-selector "allowance(address,address)"))

  (declare selector-transfer types:Bytes)
  (define selector-transfer
    "Function selector for transfer(address,uint256) -> 0xa9059cbb"
    (abi:function-selector "transfer(address,uint256)"))

  (declare selector-approve types:Bytes)
  (define selector-approve
    "Function selector for approve(address,uint256) -> 0x095ea7b3"
    (abi:function-selector "approve(address,uint256)"))

  (declare selector-transfer-from types:Bytes)
  (define selector-transfer-from
    "Function selector for transferFrom(address,address,uint256) -> 0x23b872dd"
    (abi:function-selector "transferFrom(address,address,uint256)"))

  ;;; =========================================================================
  ;;; Internal helpers - eth_call -> abi-decode -> extract single value
  ;;; =========================================================================

  (declare %call-decode-string
           (provider:HttpProvider -> addr:Address -> types:Bytes -> String
            -> (types:Web3Result String)))
  (define (%call-decode-string provider token-address calldata fn-label)
    (do (raw     <- (provider:eth-call provider None token-address calldata))
        (decoded <- (abi:abi-decode (Cons abi:AbiString Nil) raw))
        (match decoded
          ((Cons (abi:AbiStringVal s) (Nil)) (Ok s))
          (_ (Err (types:AbiError
                   (lisp String (fn-label)
                     (cl:format cl:nil "erc20:~A: unexpected response format" fn-label))))))))

  (declare %call-decode-u256
           (provider:HttpProvider -> addr:Address -> types:Bytes -> String
            -> (types:Web3Result types:U256)))
  (define (%call-decode-u256 provider token-address calldata fn-label)
    (do (raw     <- (provider:eth-call provider None token-address calldata))
        (decoded <- (abi:abi-decode (Cons (abi:AbiUint 256) Nil) raw))
        (match decoded
          ((Cons (abi:AbiUintVal u) (Nil)) (Ok u))
          (_ (Err (types:AbiError
                   (lisp String (fn-label)
                     (cl:format cl:nil "erc20:~A: unexpected response format" fn-label))))))))

  ;;; =========================================================================
  ;;; Read Functions (View Calls)
  ;;; =========================================================================

  (declare erc20-name (provider:HttpProvider -> addr:Address -> (types:Web3Result String)))
  (define (erc20-name provider token-address)
    "Get the token name"
    (%call-decode-string provider token-address selector-name "name"))

  (declare erc20-symbol (provider:HttpProvider -> addr:Address -> (types:Web3Result String)))
  (define (erc20-symbol provider token-address)
    "Get the token symbol"
    (%call-decode-string provider token-address selector-symbol "symbol"))

  (declare erc20-decimals (provider:HttpProvider -> addr:Address -> (types:Web3Result U8)))
  (define (erc20-decimals provider token-address)
    "Get the token decimals (typically 18)"
    (do (raw     <- (provider:eth-call provider None token-address selector-decimals))
        (decoded <- (abi:abi-decode (Cons (abi:AbiUint 8) Nil) raw))
        (match decoded
          ((Cons (abi:AbiUintVal u256) (Nil))
           (Ok (lisp U8 (u256)
                 (cl:let ((n (web3/types:u256-to-integer
                              (coalton (lisp types:U256 () u256)))))
                   (cl:min n 255)))))
          (_ (Err (types:AbiError "erc20:decimals: unexpected response format"))))))

  (declare erc20-total-supply (provider:HttpProvider -> addr:Address ->
                               (types:Web3Result types:U256)))
  (define (erc20-total-supply provider token-address)
    "Get the total token supply"
    (%call-decode-u256 provider token-address selector-total-supply "totalSupply"))

  (declare erc20-balance-of (provider:HttpProvider -> addr:Address -> addr:Address ->
                             (types:Web3Result types:U256)))
  (define (erc20-balance-of provider token-address owner)
    "Get the token balance of an address"
    (%call-decode-u256
     provider token-address
     (abi:abi-encode-with-selector
      selector-balance-of
      (Cons (abi:AbiAddressVal (addr:address-bytes owner)) Nil))
     "balanceOf"))

  (declare erc20-allowance (provider:HttpProvider -> addr:Address ->
                            addr:Address -> addr:Address ->
                            (types:Web3Result types:U256)))
  (define (erc20-allowance provider token-address owner spender)
    "Get the allowance for a spender on an owner's tokens"
    (%call-decode-u256
     provider token-address
     (abi:abi-encode-with-selector
      selector-allowance
      (Cons (abi:AbiAddressVal (addr:address-bytes owner))
            (Cons (abi:AbiAddressVal (addr:address-bytes spender)) Nil)))
     "allowance"))

  ;;; =========================================================================
  ;;; Write Function Calldata Builders
  ;;; =========================================================================

  (declare erc20-transfer-data (addr:Address -> types:U256 -> types:Bytes))
  (define (erc20-transfer-data to amount)
    "Build calldata for transfer(address,uint256)"
    (abi:abi-encode-with-selector
     selector-transfer
     (Cons (abi:AbiAddressVal (addr:address-bytes to))
           (Cons (abi:AbiUintVal amount) Nil))))

  (declare erc20-approve-data (addr:Address -> types:U256 -> types:Bytes))
  (define (erc20-approve-data spender amount)
    "Build calldata for approve(address,uint256)"
    (abi:abi-encode-with-selector
     selector-approve
     (Cons (abi:AbiAddressVal (addr:address-bytes spender))
           (Cons (abi:AbiUintVal amount) Nil))))

  (declare erc20-transfer-from-data (addr:Address -> addr:Address -> types:U256 -> types:Bytes))
  (define (erc20-transfer-from-data from to amount)
    "Build calldata for transferFrom(address,address,uint256)"
    (abi:abi-encode-with-selector
     selector-transfer-from
     (Cons (abi:AbiAddressVal (addr:address-bytes from))
           (Cons (abi:AbiAddressVal (addr:address-bytes to))
                 (Cons (abi:AbiUintVal amount) Nil))))))
