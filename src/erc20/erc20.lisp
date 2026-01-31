(in-package #:web3/erc20)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; ERC-20 Function Selectors
  ;;; =========================================================================

  (declare selector-name (Unit -> types:Bytes))
  (define (selector-name)
    "Function selector for name() -> 0x06fdde03"
    (abi:function-selector "name()"))

  (declare selector-symbol (Unit -> types:Bytes))
  (define (selector-symbol)
    "Function selector for symbol() -> 0x95d89b41"
    (abi:function-selector "symbol()"))

  (declare selector-decimals (Unit -> types:Bytes))
  (define (selector-decimals)
    "Function selector for decimals() -> 0x313ce567"
    (abi:function-selector "decimals()"))

  (declare selector-total-supply (Unit -> types:Bytes))
  (define (selector-total-supply)
    "Function selector for totalSupply() -> 0x18160ddd"
    (abi:function-selector "totalSupply()"))

  (declare selector-balance-of (Unit -> types:Bytes))
  (define (selector-balance-of)
    "Function selector for balanceOf(address) -> 0x70a08231"
    (abi:function-selector "balanceOf(address)"))

  (declare selector-allowance (Unit -> types:Bytes))
  (define (selector-allowance)
    "Function selector for allowance(address,address) -> 0xdd62ed3e"
    (abi:function-selector "allowance(address,address)"))

  (declare selector-transfer (Unit -> types:Bytes))
  (define (selector-transfer)
    "Function selector for transfer(address,uint256) -> 0xa9059cbb"
    (abi:function-selector "transfer(address,uint256)"))

  (declare selector-approve (Unit -> types:Bytes))
  (define (selector-approve)
    "Function selector for approve(address,uint256) -> 0x095ea7b3"
    (abi:function-selector "approve(address,uint256)"))

  (declare selector-transfer-from (Unit -> types:Bytes))
  (define (selector-transfer-from)
    "Function selector for transferFrom(address,address,uint256) -> 0x23b872dd"
    (abi:function-selector "transferFrom(address,address,uint256)"))

  ;;; =========================================================================
  ;;; Write Function Calldata Builders
  ;;; =========================================================================

  (declare erc20-transfer-data (addr:Address -> types:U256 -> types:Bytes))
  (define (erc20-transfer-data to amount)
    "Build calldata for transfer(address,uint256)"
    (abi:abi-encode-with-selector
     (selector-transfer)
     (Cons (abi:AbiAddressVal (addr:address-bytes to))
           (Cons (abi:AbiUintVal amount) Nil))))

  (declare erc20-approve-data (addr:Address -> types:U256 -> types:Bytes))
  (define (erc20-approve-data spender amount)
    "Build calldata for approve(address,uint256)"
    (abi:abi-encode-with-selector
     (selector-approve)
     (Cons (abi:AbiAddressVal (addr:address-bytes spender))
           (Cons (abi:AbiUintVal amount) Nil))))

  (declare erc20-transfer-from-data (addr:Address -> addr:Address -> types:U256 -> types:Bytes))
  (define (erc20-transfer-from-data from to amount)
    "Build calldata for transferFrom(address,address,uint256)"
    (abi:abi-encode-with-selector
     (selector-transfer-from)
     (Cons (abi:AbiAddressVal (addr:address-bytes from))
           (Cons (abi:AbiAddressVal (addr:address-bytes to))
                 (Cons (abi:AbiUintVal amount) Nil)))))

  ;;; =========================================================================
  ;;; Read Functions (View Calls)
  ;;; =========================================================================

  (declare erc20-name (provider:HttpProvider -> addr:Address -> (types:Web3Result String)))
  (define (erc20-name provider token-address)
    "Get the token name"
    (let ((calldata (selector-name)))
      (match (provider:eth-call provider None token-address calldata)
        ((Err e) (Err e))
        ((Ok result)
         (match (abi:abi-decode (Cons abi:AbiString Nil) result)
           ((Err e) (Err e))
           ((Ok decoded)
            (match decoded
              ((Cons (abi:AbiStringVal s) (Nil)) (Ok s))
              (_ (Err (types:AbiError "Unexpected response format for name()"))))))))))

  (declare erc20-symbol (provider:HttpProvider -> addr:Address -> (types:Web3Result String)))
  (define (erc20-symbol provider token-address)
    "Get the token symbol"
    (let ((calldata (selector-symbol)))
      (match (provider:eth-call provider None token-address calldata)
        ((Err e) (Err e))
        ((Ok result)
         (match (abi:abi-decode (Cons abi:AbiString Nil) result)
           ((Err e) (Err e))
           ((Ok decoded)
            (match decoded
              ((Cons (abi:AbiStringVal s) (Nil)) (Ok s))
              (_ (Err (types:AbiError "Unexpected response format for symbol()"))))))))))

  (declare erc20-decimals (provider:HttpProvider -> addr:Address -> (types:Web3Result U8)))
  (define (erc20-decimals provider token-address)
    "Get the token decimals (typically 18)"
    (let ((calldata (selector-decimals)))
      (match (provider:eth-call provider None token-address calldata)
        ((Err e) (Err e))
        ((Ok result)
         (match (abi:abi-decode (Cons (abi:AbiUint 8) Nil) result)
           ((Err e) (Err e))
           ((Ok decoded)
            (match decoded
              ((Cons (abi:AbiUintVal u256) (Nil))
               (Ok (lisp U8 (u256)
                     (cl:let ((n (web3/types::%u256-to-bignum
                                  (coalton (lisp types:U256 () u256)))))
                       (cl:min n 255)))))
              (_ (Err (types:AbiError "Unexpected response format for decimals()"))))))))))

  (declare erc20-total-supply (provider:HttpProvider -> addr:Address -> (types:Web3Result types:U256)))
  (define (erc20-total-supply provider token-address)
    "Get the total token supply"
    (let ((calldata (selector-total-supply)))
      (match (provider:eth-call provider None token-address calldata)
        ((Err e) (Err e))
        ((Ok result)
         (match (abi:abi-decode (Cons (abi:AbiUint 256) Nil) result)
           ((Err e) (Err e))
           ((Ok decoded)
            (match decoded
              ((Cons (abi:AbiUintVal u256) (Nil)) (Ok u256))
              (_ (Err (types:AbiError "Unexpected response format for totalSupply()"))))))))))

  (declare erc20-balance-of (provider:HttpProvider -> addr:Address -> addr:Address ->
                             (types:Web3Result types:U256)))
  (define (erc20-balance-of provider token-address owner)
    "Get the token balance of an address"
    (let ((calldata (abi:abi-encode-with-selector
                     (selector-balance-of)
                     (Cons (abi:AbiAddressVal (addr:address-bytes owner)) Nil))))
      (match (provider:eth-call provider None token-address calldata)
        ((Err e) (Err e))
        ((Ok result)
         (match (abi:abi-decode (Cons (abi:AbiUint 256) Nil) result)
           ((Err e) (Err e))
           ((Ok decoded)
            (match decoded
              ((Cons (abi:AbiUintVal u256) (Nil)) (Ok u256))
              (_ (Err (types:AbiError "Unexpected response format for balanceOf()"))))))))))

  (declare erc20-allowance (provider:HttpProvider -> addr:Address -> addr:Address -> addr:Address ->
                            (types:Web3Result types:U256)))
  (define (erc20-allowance provider token-address owner spender)
    "Get the allowance for a spender on an owner's tokens"
    (let ((calldata (abi:abi-encode-with-selector
                     (selector-allowance)
                     (Cons (abi:AbiAddressVal (addr:address-bytes owner))
                           (Cons (abi:AbiAddressVal (addr:address-bytes spender)) Nil)))))
      (match (provider:eth-call provider None token-address calldata)
        ((Err e) (Err e))
        ((Ok result)
         (match (abi:abi-decode (Cons (abi:AbiUint 256) Nil) result)
           ((Err e) (Err e))
           ((Ok decoded)
            (match decoded
              ((Cons (abi:AbiUintVal u256) (Nil)) (Ok u256))
              (_ (Err (types:AbiError "Unexpected response format for allowance()")))))))))))
