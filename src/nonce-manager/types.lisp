;;;; Nonce Manager types
;;;; Type definitions for nonce tracking

(in-package #:web3/nonce-manager)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Nonce Key
  ;;; =========================================================================

  ;; Key for nonce lookup: (address-bytes, chain-id)
  ;; Using address bytes instead of Address type for easier comparison
  (define-type-alias NonceKey (Tuple types:Bytes UFix))

  (declare make-nonce-key (addr:Address -> UFix -> NonceKey))
  (define (make-nonce-key address chain-id)
    "Create a nonce key from address and chain ID"
    (Tuple (addr:address-bytes address) chain-id))

  (declare nonce-key-equal? (NonceKey -> NonceKey -> Boolean))
  (define (nonce-key-equal? k1 k2)
    "Check if two nonce keys are equal"
    (match k1
      ((Tuple addr1 chain1)
       (match k2
         ((Tuple addr2 chain2)
          (and (types:bytes-equal? addr1 addr2)
               (== chain1 chain2)))))))

  ;;; =========================================================================
  ;;; Nonce Manager
  ;;; =========================================================================

  (define-struct NonceManager
    "Manages transaction nonces for multiple addresses and chains.
     Caches nonces and tracks local deltas from confirmed counts."
    (nm-provider provider:HttpProvider)
    ;; Cache of confirmed nonces: (NonceKey, confirmed-nonce)
    (nm-cache (cell:Cell (List (Tuple NonceKey U64))))
    ;; Local deltas: (NonceKey, pending-transactions-count)
    (nm-deltas (cell:Cell (List (Tuple NonceKey U64))))))
