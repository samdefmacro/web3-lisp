(in-package #:web3/transaction)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; Transaction Type enum
  (define-type TransactionType
    "Ethereum transaction type"
    LegacyTx    ; Type 0 (pre-EIP-2718)
    EIP2930Tx   ; Type 1 (access list)
    EIP1559Tx   ; Type 2 (fee market)
    EIP4844Tx)  ; Type 3 (blob transaction)

  ;;; Access list entry: (address, [storage-keys])
  (define-type-alias AccessListEntry (Tuple addr:Address (List types:Bytes)))
  (define-type-alias AccessList (List AccessListEntry))

  ;;; Blob versioned hashes (32 bytes each, used for EIP-4844)
  (define-type-alias BlobVersionedHashes (List types:Bytes))

  ;;; Transaction struct
  (define-struct Transaction
    "Ethereum transaction"
    (tx-type TransactionType)
    (tx-chain-id U64)
    (tx-nonce U64)
    (tx-gas-price types:U256)             ; legacy/EIP-2930 only, zero for EIP-1559+
    (tx-max-priority-fee types:U256)      ; EIP-1559/EIP-4844 only, zero for legacy
    (tx-max-fee types:U256)               ; max-fee-per-gas (EIP-1559/4844 only, zero for legacy)
    (tx-gas-limit U64)
    (tx-to (Optional addr:Address))       ; None = contract creation
    (tx-value types:U256)                 ; value in wei
    (tx-data types:Bytes)                 ; calldata
    (tx-access-list AccessList)           ; access list (empty for legacy)
    (tx-max-fee-per-blob-gas types:U256)  ; EIP-4844 only, zero for others
    (tx-blob-versioned-hashes BlobVersionedHashes)) ; EIP-4844 only, empty for others

  ;; Convenience constructors

  (declare make-transaction (TransactionType -> U64 -> U64 -> types:U256 -> types:U256 ->
                             U64 -> (Optional addr:Address) -> types:U256 -> types:Bytes ->
                             AccessList -> Transaction))
  (define (make-transaction typ chain-id nonce price-field max-fee gas-limit to value data access-list)
    "Create a transaction. For legacy/EIP-2930, price-field is gas-price.
     For EIP-1559, price-field is max-priority-fee-per-gas."
    (match typ
      ((LegacyTx)
       (Transaction typ chain-id nonce price-field types:u256-zero max-fee gas-limit to value data
                    access-list types:u256-zero Nil))
      ((EIP2930Tx)
       (Transaction typ chain-id nonce price-field types:u256-zero max-fee gas-limit to value data
                    access-list types:u256-zero Nil))
      ((EIP1559Tx)
       (Transaction typ chain-id nonce types:u256-zero price-field max-fee gas-limit to value data
                    access-list types:u256-zero Nil))
      ((EIP4844Tx)
       (Transaction typ chain-id nonce types:u256-zero price-field max-fee gas-limit to value data
                    access-list types:u256-zero Nil))))

  (declare make-blob-transaction (U64 -> U64 -> types:U256 -> types:U256 -> U64 ->
                                  (Optional addr:Address) -> types:U256 -> types:Bytes ->
                                  AccessList -> types:U256 -> BlobVersionedHashes -> Transaction))
  (define (make-blob-transaction chain-id nonce max-priority-fee max-fee gas-limit to value data
                                 access-list max-fee-per-blob-gas blob-hashes)
    "Create an EIP-4844 blob transaction"
    (Transaction EIP4844Tx chain-id nonce types:u256-zero max-priority-fee max-fee gas-limit to value
                 data access-list max-fee-per-blob-gas blob-hashes)))
