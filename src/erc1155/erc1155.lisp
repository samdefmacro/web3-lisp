(in-package #:web3/erc1155)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; ERC-1155 Function Selectors
  ;;; =========================================================================

  (declare selector-uri (Unit -> types:Bytes))
  (define (selector-uri)
    "Function selector for uri(uint256) -> 0x0e89341c"
    (abi:function-selector "uri(uint256)"))

  (declare selector-balance-of (Unit -> types:Bytes))
  (define (selector-balance-of)
    "Function selector for balanceOf(address,uint256) -> 0x00fdd58e"
    (abi:function-selector "balanceOf(address,uint256)"))

  (declare selector-balance-of-batch (Unit -> types:Bytes))
  (define (selector-balance-of-batch)
    "Function selector for balanceOfBatch(address[],uint256[]) -> 0x4e1273f4"
    (abi:function-selector "balanceOfBatch(address[],uint256[])"))

  (declare selector-is-approved-for-all (Unit -> types:Bytes))
  (define (selector-is-approved-for-all)
    "Function selector for isApprovedForAll(address,address) -> 0xe985e9c5"
    (abi:function-selector "isApprovedForAll(address,address)"))

  (declare selector-safe-transfer-from (Unit -> types:Bytes))
  (define (selector-safe-transfer-from)
    "Function selector for safeTransferFrom(address,address,uint256,uint256,bytes) -> 0xf242432a"
    (abi:function-selector "safeTransferFrom(address,address,uint256,uint256,bytes)"))

  (declare selector-safe-batch-transfer-from (Unit -> types:Bytes))
  (define (selector-safe-batch-transfer-from)
    "Function selector for safeBatchTransferFrom(address,address,uint256[],uint256[],bytes) -> 0x2eb2c2d6"
    (abi:function-selector "safeBatchTransferFrom(address,address,uint256[],uint256[],bytes)"))

  (declare selector-set-approval-for-all (Unit -> types:Bytes))
  (define (selector-set-approval-for-all)
    "Function selector for setApprovalForAll(address,bool) -> 0xa22cb465"
    (abi:function-selector "setApprovalForAll(address,bool)"))

  ;;; =========================================================================
  ;;; Write Function Calldata Builders
  ;;; =========================================================================

  (declare erc1155-safe-transfer-from-data
           (addr:Address -> addr:Address -> types:U256 -> types:U256 -> types:Bytes -> types:Bytes))
  (define (erc1155-safe-transfer-from-data from to token-id amount data)
    "Build calldata for safeTransferFrom(address,address,uint256,uint256,bytes)"
    (abi:abi-encode-with-selector
     (selector-safe-transfer-from)
     (Cons (abi:AbiAddressVal (addr:address-bytes from))
           (Cons (abi:AbiAddressVal (addr:address-bytes to))
                 (Cons (abi:AbiUintVal token-id)
                       (Cons (abi:AbiUintVal amount)
                             (Cons (abi:AbiBytesVal data) Nil)))))))

  (declare erc1155-safe-batch-transfer-from-data
           (addr:Address -> addr:Address -> (List types:U256) -> (List types:U256) -> types:Bytes -> types:Bytes))
  (define (erc1155-safe-batch-transfer-from-data from to token-ids amounts data)
    "Build calldata for safeBatchTransferFrom(address,address,uint256[],uint256[],bytes)"
    (let ((ids-abi (map (fn (id) (abi:AbiUintVal id)) token-ids))
          (amounts-abi (map (fn (amt) (abi:AbiUintVal amt)) amounts)))
      (abi:abi-encode-with-selector
       (selector-safe-batch-transfer-from)
       (Cons (abi:AbiAddressVal (addr:address-bytes from))
             (Cons (abi:AbiAddressVal (addr:address-bytes to))
                   (Cons (abi:AbiArrayVal ids-abi)
                         (Cons (abi:AbiArrayVal amounts-abi)
                               (Cons (abi:AbiBytesVal data) Nil))))))))

  (declare erc1155-set-approval-for-all-data (addr:Address -> Boolean -> types:Bytes))
  (define (erc1155-set-approval-for-all-data operator approved)
    "Build calldata for setApprovalForAll(address,bool)"
    (abi:abi-encode-with-selector
     (selector-set-approval-for-all)
     (Cons (abi:AbiAddressVal (addr:address-bytes operator))
           (Cons (abi:AbiBoolVal approved) Nil))))

  ;;; =========================================================================
  ;;; Read Functions (View Calls)
  ;;; =========================================================================

  (declare erc1155-uri (provider:HttpProvider -> addr:Address -> types:U256 -> (types:Web3Result String)))
  (define (erc1155-uri provider contract-address token-id)
    "Get the URI for a token's metadata"
    (let ((calldata (abi:abi-encode-with-selector
                     (selector-uri)
                     (Cons (abi:AbiUintVal token-id) Nil))))
      (match (provider:eth-call provider None contract-address calldata)
        ((Err e) (Err e))
        ((Ok result)
         (match (abi:abi-decode (Cons abi:AbiString Nil) result)
           ((Err e) (Err e))
           ((Ok decoded)
            (match decoded
              ((Cons (abi:AbiStringVal s) (Nil)) (Ok s))
              (_ (Err (types:AbiError "Unexpected response format for uri()"))))))))))

  (declare erc1155-balance-of (provider:HttpProvider -> addr:Address -> addr:Address -> types:U256 ->
                               (types:Web3Result types:U256)))
  (define (erc1155-balance-of provider contract-address account token-id)
    "Get the balance of a specific token for an account"
    (let ((calldata (abi:abi-encode-with-selector
                     (selector-balance-of)
                     (Cons (abi:AbiAddressVal (addr:address-bytes account))
                           (Cons (abi:AbiUintVal token-id) Nil)))))
      (match (provider:eth-call provider None contract-address calldata)
        ((Err e) (Err e))
        ((Ok result)
         (match (abi:abi-decode (Cons (abi:AbiUint 256) Nil) result)
           ((Err e) (Err e))
           ((Ok decoded)
            (match decoded
              ((Cons (abi:AbiUintVal balance) (Nil)) (Ok balance))
              (_ (Err (types:AbiError "Unexpected response format for balanceOf()"))))))))))

  (declare erc1155-balance-of-batch
           (provider:HttpProvider -> addr:Address -> (List addr:Address) -> (List types:U256) ->
            (types:Web3Result (List types:U256))))
  (define (erc1155-balance-of-batch provider contract-address accounts token-ids)
    "Get balances for multiple account/token pairs"
    (let ((accounts-abi (map (fn (acc) (abi:AbiAddressVal (addr:address-bytes acc))) accounts))
          (ids-abi (map (fn (id) (abi:AbiUintVal id)) token-ids)))
      (let ((calldata (abi:abi-encode-with-selector
                       (selector-balance-of-batch)
                       (Cons (abi:AbiArrayVal accounts-abi)
                             (Cons (abi:AbiArrayVal ids-abi) Nil)))))
        (match (provider:eth-call provider None contract-address calldata)
          ((Err e) (Err e))
          ((Ok result)
           (match (abi:abi-decode (Cons (abi:AbiArray (abi:AbiUint 256)) Nil) result)
             ((Err e) (Err e))
             ((Ok decoded)
              (match decoded
                ((Cons (abi:AbiArrayVal balances) (Nil))
                 (Ok (map (fn (v)
                            (match v
                              ((abi:AbiUintVal u) u)
                              (_ (types:u256-zero))))
                          balances)))
                (_ (Err (types:AbiError "Unexpected response format for balanceOfBatch()")))))))))))

  (declare erc1155-is-approved-for-all
           (provider:HttpProvider -> addr:Address -> addr:Address -> addr:Address ->
            (types:Web3Result Boolean)))
  (define (erc1155-is-approved-for-all provider contract-address account operator)
    "Check if an operator is approved for all tokens of an account"
    (let ((calldata (abi:abi-encode-with-selector
                     (selector-is-approved-for-all)
                     (Cons (abi:AbiAddressVal (addr:address-bytes account))
                           (Cons (abi:AbiAddressVal (addr:address-bytes operator)) Nil)))))
      (match (provider:eth-call provider None contract-address calldata)
        ((Err e) (Err e))
        ((Ok result)
         (match (abi:abi-decode (Cons abi:AbiBool Nil) result)
           ((Err e) (Err e))
           ((Ok decoded)
            (match decoded
              ((Cons (abi:AbiBoolVal b) (Nil)) (Ok b))
              (_ (Err (types:AbiError "Unexpected response format for isApprovedForAll()")))))))))))
