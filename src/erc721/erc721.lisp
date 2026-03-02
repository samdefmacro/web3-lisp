(in-package #:web3/erc721)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; ERC-721 Function Selectors
  ;;; =========================================================================

  ;; Metadata extension
  (declare selector-name types:Bytes)
  (define selector-name
    "Function selector for name() -> 0x06fdde03"
    (abi:function-selector "name()"))

  (declare selector-symbol types:Bytes)
  (define selector-symbol
    "Function selector for symbol() -> 0x95d89b41"
    (abi:function-selector "symbol()"))

  (declare selector-token-uri types:Bytes)
  (define selector-token-uri
    "Function selector for tokenURI(uint256) -> 0xc87b56dd"
    (abi:function-selector "tokenURI(uint256)"))

  ;; Core ERC-721
  (declare selector-balance-of types:Bytes)
  (define selector-balance-of
    "Function selector for balanceOf(address) -> 0x70a08231"
    (abi:function-selector "balanceOf(address)"))

  (declare selector-owner-of types:Bytes)
  (define selector-owner-of
    "Function selector for ownerOf(uint256) -> 0x6352211e"
    (abi:function-selector "ownerOf(uint256)"))

  (declare selector-get-approved types:Bytes)
  (define selector-get-approved
    "Function selector for getApproved(uint256) -> 0x081812fc"
    (abi:function-selector "getApproved(uint256)"))

  (declare selector-is-approved-for-all types:Bytes)
  (define selector-is-approved-for-all
    "Function selector for isApprovedForAll(address,address) -> 0xe985e9c5"
    (abi:function-selector "isApprovedForAll(address,address)"))

  (declare selector-transfer-from types:Bytes)
  (define selector-transfer-from
    "Function selector for transferFrom(address,address,uint256) -> 0x23b872dd"
    (abi:function-selector "transferFrom(address,address,uint256)"))

  (declare selector-safe-transfer-from types:Bytes)
  (define selector-safe-transfer-from
    "Function selector for safeTransferFrom(address,address,uint256) -> 0x42842e0e"
    (abi:function-selector "safeTransferFrom(address,address,uint256)"))

  (declare selector-safe-transfer-from-with-data types:Bytes)
  (define selector-safe-transfer-from-with-data
    "Function selector for safeTransferFrom(address,address,uint256,bytes) -> 0xb88d4fde"
    (abi:function-selector "safeTransferFrom(address,address,uint256,bytes)"))

  (declare selector-approve types:Bytes)
  (define selector-approve
    "Function selector for approve(address,uint256) -> 0x095ea7b3"
    (abi:function-selector "approve(address,uint256)"))

  (declare selector-set-approval-for-all types:Bytes)
  (define selector-set-approval-for-all
    "Function selector for setApprovalForAll(address,bool) -> 0xa22cb465"
    (abi:function-selector "setApprovalForAll(address,bool)"))

  ;;; =========================================================================
  ;;; Write Function Calldata Builders
  ;;; =========================================================================

  (declare erc721-transfer-from-data (addr:Address -> addr:Address -> types:U256 -> types:Bytes))
  (define (erc721-transfer-from-data from to token-id)
    "Build calldata for transferFrom(address,address,uint256)"
    (abi:abi-encode-with-selector
     selector-transfer-from
     (Cons (abi:AbiAddressVal (addr:address-bytes from))
           (Cons (abi:AbiAddressVal (addr:address-bytes to))
                 (Cons (abi:AbiUintVal token-id) Nil)))))

  (declare erc721-safe-transfer-from-data (addr:Address -> addr:Address -> types:U256 -> types:Bytes))
  (define (erc721-safe-transfer-from-data from to token-id)
    "Build calldata for safeTransferFrom(address,address,uint256)"
    (abi:abi-encode-with-selector
     selector-safe-transfer-from
     (Cons (abi:AbiAddressVal (addr:address-bytes from))
           (Cons (abi:AbiAddressVal (addr:address-bytes to))
                 (Cons (abi:AbiUintVal token-id) Nil)))))

  (declare erc721-safe-transfer-from-with-data (addr:Address -> addr:Address -> types:U256 -> types:Bytes -> types:Bytes))
  (define (erc721-safe-transfer-from-with-data from to token-id data)
    "Build calldata for safeTransferFrom(address,address,uint256,bytes)"
    (abi:abi-encode-with-selector
     selector-safe-transfer-from-with-data
     (Cons (abi:AbiAddressVal (addr:address-bytes from))
           (Cons (abi:AbiAddressVal (addr:address-bytes to))
                 (Cons (abi:AbiUintVal token-id)
                       (Cons (abi:AbiBytesVal data) Nil))))))

  (declare erc721-approve-data (addr:Address -> types:U256 -> types:Bytes))
  (define (erc721-approve-data to token-id)
    "Build calldata for approve(address,uint256)"
    (abi:abi-encode-with-selector
     selector-approve
     (Cons (abi:AbiAddressVal (addr:address-bytes to))
           (Cons (abi:AbiUintVal token-id) Nil))))

  (declare erc721-set-approval-for-all-data (addr:Address -> Boolean -> types:Bytes))
  (define (erc721-set-approval-for-all-data operator approved)
    "Build calldata for setApprovalForAll(address,bool)"
    (abi:abi-encode-with-selector
     selector-set-approval-for-all
     (Cons (abi:AbiAddressVal (addr:address-bytes operator))
           (Cons (abi:AbiBoolVal approved) Nil))))

  ;;; =========================================================================
  ;;; Read Functions (View Calls)
  ;;; =========================================================================

  (declare erc721-name (provider:HttpProvider -> addr:Address -> (types:Web3Result String)))
  (define (erc721-name provider nft-address)
    "Get the NFT collection name"
    (let ((calldata selector-name))
      (match (provider:eth-call provider None nft-address calldata)
        ((Err e) (Err e))
        ((Ok result)
         (match (abi:abi-decode (Cons abi:AbiString Nil) result)
           ((Err e) (Err e))
           ((Ok decoded)
            (match decoded
              ((Cons (abi:AbiStringVal s) (Nil)) (Ok s))
              (_ (Err (types:AbiError "Unexpected response format for name()"))))))))))

  (declare erc721-symbol (provider:HttpProvider -> addr:Address -> (types:Web3Result String)))
  (define (erc721-symbol provider nft-address)
    "Get the NFT collection symbol"
    (let ((calldata selector-symbol))
      (match (provider:eth-call provider None nft-address calldata)
        ((Err e) (Err e))
        ((Ok result)
         (match (abi:abi-decode (Cons abi:AbiString Nil) result)
           ((Err e) (Err e))
           ((Ok decoded)
            (match decoded
              ((Cons (abi:AbiStringVal s) (Nil)) (Ok s))
              (_ (Err (types:AbiError "Unexpected response format for symbol()"))))))))))

  (declare erc721-token-uri (provider:HttpProvider -> addr:Address -> types:U256 -> (types:Web3Result String)))
  (define (erc721-token-uri provider nft-address token-id)
    "Get the token URI for metadata"
    (let ((calldata (abi:abi-encode-with-selector
                     selector-token-uri
                     (Cons (abi:AbiUintVal token-id) Nil))))
      (match (provider:eth-call provider None nft-address calldata)
        ((Err e) (Err e))
        ((Ok result)
         (match (abi:abi-decode (Cons abi:AbiString Nil) result)
           ((Err e) (Err e))
           ((Ok decoded)
            (match decoded
              ((Cons (abi:AbiStringVal s) (Nil)) (Ok s))
              (_ (Err (types:AbiError "Unexpected response format for tokenURI()"))))))))))

  (declare erc721-balance-of (provider:HttpProvider -> addr:Address -> addr:Address ->
                              (types:Web3Result types:U256)))
  (define (erc721-balance-of provider nft-address owner)
    "Get the number of NFTs owned by an address"
    (let ((calldata (abi:abi-encode-with-selector
                     selector-balance-of
                     (Cons (abi:AbiAddressVal (addr:address-bytes owner)) Nil))))
      (match (provider:eth-call provider None nft-address calldata)
        ((Err e) (Err e))
        ((Ok result)
         (match (abi:abi-decode (Cons (abi:AbiUint 256) Nil) result)
           ((Err e) (Err e))
           ((Ok decoded)
            (match decoded
              ((Cons (abi:AbiUintVal u256) (Nil)) (Ok u256))
              (_ (Err (types:AbiError "Unexpected response format for balanceOf()"))))))))))

  (declare erc721-owner-of (provider:HttpProvider -> addr:Address -> types:U256 ->
                            (types:Web3Result addr:Address)))
  (define (erc721-owner-of provider nft-address token-id)
    "Get the owner of a specific token"
    (let ((calldata (abi:abi-encode-with-selector
                     selector-owner-of
                     (Cons (abi:AbiUintVal token-id) Nil))))
      (match (provider:eth-call provider None nft-address calldata)
        ((Err e) (Err e))
        ((Ok result)
         (match (abi:abi-decode (Cons abi:AbiAddress Nil) result)
           ((Err e) (Err e))
           ((Ok decoded)
            (match decoded
              ((Cons (abi:AbiAddressVal addr-bytes) (Nil))
               (addr:address-from-bytes addr-bytes))
              (_ (Err (types:AbiError "Unexpected response format for ownerOf()"))))))))))

  (declare erc721-get-approved (provider:HttpProvider -> addr:Address -> types:U256 ->
                                (types:Web3Result addr:Address)))
  (define (erc721-get-approved provider nft-address token-id)
    "Get the approved address for a token"
    (let ((calldata (abi:abi-encode-with-selector
                     selector-get-approved
                     (Cons (abi:AbiUintVal token-id) Nil))))
      (match (provider:eth-call provider None nft-address calldata)
        ((Err e) (Err e))
        ((Ok result)
         (match (abi:abi-decode (Cons abi:AbiAddress Nil) result)
           ((Err e) (Err e))
           ((Ok decoded)
            (match decoded
              ((Cons (abi:AbiAddressVal addr-bytes) (Nil))
               (addr:address-from-bytes addr-bytes))
              (_ (Err (types:AbiError "Unexpected response format for getApproved()"))))))))))

  (declare erc721-is-approved-for-all (provider:HttpProvider -> addr:Address ->
                                        addr:Address -> addr:Address ->
                                        (types:Web3Result Boolean)))
  (define (erc721-is-approved-for-all provider nft-address owner operator)
    "Check if an operator is approved for all tokens of an owner"
    (let ((calldata (abi:abi-encode-with-selector
                     selector-is-approved-for-all
                     (Cons (abi:AbiAddressVal (addr:address-bytes owner))
                           (Cons (abi:AbiAddressVal (addr:address-bytes operator)) Nil)))))
      (match (provider:eth-call provider None nft-address calldata)
        ((Err e) (Err e))
        ((Ok result)
         (match (abi:abi-decode (Cons abi:AbiBool Nil) result)
           ((Err e) (Err e))
           ((Ok decoded)
            (match decoded
              ((Cons (abi:AbiBoolVal b) (Nil)) (Ok b))
              (_ (Err (types:AbiError "Unexpected response format for isApprovedForAll()")))))))))))
