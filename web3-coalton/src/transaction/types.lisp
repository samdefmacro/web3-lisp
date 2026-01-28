(in-package #:web3/transaction)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; Transaction Type enum
  (define-type TransactionType
    "Ethereum transaction type"
    LegacyTx    ; Type 0 (pre-EIP-2718)
    EIP2930Tx   ; Type 1 (access list)
    EIP1559Tx)  ; Type 2 (fee market)

  ;;; Access list entry: (address, [storage-keys])
  (define-type-alias AccessListEntry (Tuple addr:Address (List types:Bytes)))
  (define-type-alias AccessList (List AccessListEntry))

  ;;; Transaction struct
  (define-type Transaction
    "Ethereum transaction"
    (%Transaction
     TransactionType        ; type
     U64                    ; chain-id
     U64                    ; nonce
     types:U256             ; gas-price (legacy) or max-priority-fee (EIP-1559)
     types:U256             ; max-fee-per-gas (EIP-1559 only, zero for legacy)
     U64                    ; gas-limit
     (Optional addr:Address) ; to (None = contract creation)
     types:U256             ; value in wei
     types:Bytes            ; data (calldata)
     AccessList))           ; access list (empty for legacy)

  ;; Constructors and accessors

  (declare make-transaction (TransactionType -> U64 -> U64 -> types:U256 -> types:U256 ->
                             U64 -> (Optional addr:Address) -> types:U256 -> types:Bytes ->
                             AccessList -> Transaction))
  (define (make-transaction typ chain-id nonce gas-price max-fee gas-limit to value data access-list)
    (%Transaction typ chain-id nonce gas-price max-fee gas-limit to value data access-list))

  (declare tx-type (Transaction -> TransactionType))
  (define (tx-type tx) (match tx ((%Transaction t _ _ _ _ _ _ _ _ _) t)))

  (declare tx-chain-id (Transaction -> U64))
  (define (tx-chain-id tx) (match tx ((%Transaction _ c _ _ _ _ _ _ _ _) c)))

  (declare tx-nonce (Transaction -> U64))
  (define (tx-nonce tx) (match tx ((%Transaction _ _ n _ _ _ _ _ _ _) n)))

  (declare tx-gas-price (Transaction -> types:U256))
  (define (tx-gas-price tx) (match tx ((%Transaction _ _ _ gp _ _ _ _ _ _) gp)))

  (declare tx-max-priority-fee (Transaction -> types:U256))
  (define (tx-max-priority-fee tx) (match tx ((%Transaction _ _ _ mpf _ _ _ _ _ _) mpf)))

  (declare tx-max-fee (Transaction -> types:U256))
  (define (tx-max-fee tx) (match tx ((%Transaction _ _ _ _ mf _ _ _ _ _) mf)))

  (declare tx-gas-limit (Transaction -> U64))
  (define (tx-gas-limit tx) (match tx ((%Transaction _ _ _ _ _ gl _ _ _ _) gl)))

  (declare tx-to (Transaction -> (Optional addr:Address)))
  (define (tx-to tx) (match tx ((%Transaction _ _ _ _ _ _ to _ _ _) to)))

  (declare tx-value (Transaction -> types:U256))
  (define (tx-value tx) (match tx ((%Transaction _ _ _ _ _ _ _ v _ _) v)))

  (declare tx-data (Transaction -> types:Bytes))
  (define (tx-data tx) (match tx ((%Transaction _ _ _ _ _ _ _ _ d _) d)))

  (declare tx-access-list (Transaction -> AccessList))
  (define (tx-access-list tx) (match tx ((%Transaction _ _ _ _ _ _ _ _ _ al) al))))
