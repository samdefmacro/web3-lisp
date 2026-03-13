;;;; ERC-165 Standard Interface Detection
;;;;
;;;; Implements supportsInterface(bytes4) for detecting which standards
;;;; a contract implements (ERC-721, ERC-1155, ERC-20, etc.).
;;;; https://eips.ethereum.org/EIPS/eip-165

(in-package #:web3/erc165)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Function Selector
  ;;; =========================================================================

  (declare selector-supports-interface types:Bytes)
  (define selector-supports-interface
    "Function selector for supportsInterface(bytes4) -> 0x01ffc9a7"
    (abi:function-selector "supportsInterface(bytes4)"))

  ;;; =========================================================================
  ;;; Well-known Interface IDs
  ;;; =========================================================================

  (declare %make-interface-id (U8 -> U8 -> U8 -> U8 -> types:Bytes))
  (define (%make-interface-id a b c d)
    "Create a 4-byte interface ID."
    (lisp types:Bytes (a b c d)
      (cl:make-array 4 :initial-contents (cl:list a b c d)
                     :fill-pointer 4 :adjustable cl:t)))

  (declare interface-erc165 types:Bytes)
  (define interface-erc165
    "ERC-165 interface ID: 0x01ffc9a7"
    (%make-interface-id #x01 #xff #xc9 #xa7))

  (declare interface-erc721 types:Bytes)
  (define interface-erc721
    "ERC-721 interface ID: 0x80ac58cd"
    (%make-interface-id #x80 #xac #x58 #xcd))

  (declare interface-erc721-metadata types:Bytes)
  (define interface-erc721-metadata
    "ERC-721 Metadata interface ID: 0x5b5e139f"
    (%make-interface-id #x5b #x5e #x13 #x9f))

  (declare interface-erc721-enumerable types:Bytes)
  (define interface-erc721-enumerable
    "ERC-721 Enumerable interface ID: 0x780e9d63"
    (%make-interface-id #x78 #x0e #x9d #x63))

  (declare interface-erc1155 types:Bytes)
  (define interface-erc1155
    "ERC-1155 interface ID: 0xd9b67a26"
    (%make-interface-id #xd9 #xb6 #x7a #x26))

  (declare interface-erc1155-metadata-uri types:Bytes)
  (define interface-erc1155-metadata-uri
    "ERC-1155 MetadataURI interface ID: 0x0e89341c"
    (%make-interface-id #x0e #x89 #x34 #x1c))

  (declare interface-erc20 types:Bytes)
  (define interface-erc20
    "ERC-20 interface ID: 0x36372b07"
    (%make-interface-id #x36 #x37 #x2b #x07))

  ;;; =========================================================================
  ;;; Core Function
  ;;; =========================================================================

  (declare supports-interface (provider:HttpProvider -> addr:Address -> types:Bytes
                               -> (types:Web3Result Boolean)))
  (define (supports-interface provider contract-address interface-id)
    "Check if a contract supports a given interface (ERC-165).
     interface-id should be a 4-byte identifier."
    (let ((calldata (abi:abi-encode-with-selector
                     selector-supports-interface
                     (Cons (abi:AbiBytesFixedVal (types:bytes-pad-right 32 interface-id))
                           Nil))))
      (match (provider:eth-call provider None contract-address calldata)
        ((Err e) (Err e))
        ((Ok result)
         (match (abi:abi-decode (Cons abi:AbiBool Nil) result)
           ((Err e) (Err e))
           ((Ok decoded)
            (match decoded
              ((Cons (abi:AbiBoolVal b) (Nil)) (Ok b))
              (_ (Err (types:AbiError "Unexpected response format for supportsInterface()")))))))))))
