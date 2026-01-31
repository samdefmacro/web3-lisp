(in-package #:web3/transaction)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; Internal helpers for RLP encoding of transaction fields

  (declare %u64-to-rlp-item (U64 -> rlp:RlpItem))
  (define (%u64-to-rlp-item n)
    "Convert U64 to RLP bytes item"
    (rlp:RlpBytes
     (lisp types:Bytes (n)
       (cl:if (cl:zerop n)
              (cl:make-array 0 :fill-pointer 0 :adjustable cl:t)
              (cl:let* ((byte-count (cl:ceiling (cl:integer-length n) 8))
                        (result (cl:make-array byte-count :fill-pointer byte-count
                                                         :adjustable cl:t)))
                (cl:loop :for i :from 0 :below byte-count
                         :do (cl:setf (cl:aref result (cl:- byte-count 1 i))
                                      (cl:ldb (cl:byte 8 (cl:* i 8)) n)))
                result)))))

  (declare %u256-to-rlp-item (types:U256 -> rlp:RlpItem))
  (define (%u256-to-rlp-item u)
    "Convert U256 to RLP bytes item (big-endian, no leading zeros)"
    (rlp:RlpBytes
     (lisp types:Bytes (u)
       (cl:let* ((n (web3/types::%u256-to-bignum (coalton (lisp types:U256 () u)))))
         (cl:if (cl:zerop n)
                (cl:make-array 0 :fill-pointer 0 :adjustable cl:t)
                (cl:let* ((byte-count (cl:ceiling (cl:integer-length n) 8))
                          (result (cl:make-array byte-count :fill-pointer byte-count
                                                            :adjustable cl:t)))
                  (cl:loop :for i :from 0 :below byte-count
                           :do (cl:setf (cl:aref result (cl:- byte-count 1 i))
                                        (cl:ldb (cl:byte 8 (cl:* i 8)) n)))
                  result))))))

  (declare %address-to-rlp-item ((Optional addr:Address) -> rlp:RlpItem))
  (define (%address-to-rlp-item maybe-addr)
    "Convert optional address to RLP item (empty bytes for None)"
    (match maybe-addr
      ((Some addr) (rlp:RlpBytes (addr:address-bytes addr)))
      ((None) (rlp:RlpBytes (types:bytes-empty)))))

  (declare %encode-access-list (AccessList -> rlp:RlpItem))
  (define (%encode-access-list al)
    "Encode access list as RLP"
    (rlp:RlpList
     (map (fn (entry)
            (match entry
              ((Tuple address keys)
               (rlp:RlpList
                (Cons (rlp:RlpBytes (addr:address-bytes address))
                      (Cons (rlp:RlpList
                             (map (fn (key) (rlp:RlpBytes key)) keys))
                            Nil))))))
          al)))

  ;;; Transaction encoding for signing

  (declare tx-encode-for-signing (Transaction -> types:Bytes))
  (define (tx-encode-for-signing tx)
    "Encode a transaction for signing (pre-signature hash input)"
    (match (tx-type tx)
      ((LegacyTx)
       (rlp:rlp-encode
        (rlp:RlpList
         (Cons (%u64-to-rlp-item (tx-nonce tx))
               (Cons (%u256-to-rlp-item (tx-gas-price tx))
                     (Cons (%u64-to-rlp-item (tx-gas-limit tx))
                           (Cons (%address-to-rlp-item (tx-to tx))
                                 (Cons (%u256-to-rlp-item (tx-value tx))
                                       (Cons (rlp:RlpBytes (tx-data tx))
                                             (Cons (%u64-to-rlp-item (tx-chain-id tx))
                                                   (Cons (rlp:RlpBytes (types:bytes-empty))
                                                         (Cons (rlp:RlpBytes (types:bytes-empty))
                                                               Nil))))))))))))

      ((EIP2930Tx)
       (types:bytes-append
        (types:bytes-from-list (Cons 1 Nil))
        (rlp:rlp-encode
         (rlp:RlpList
          (Cons (%u64-to-rlp-item (tx-chain-id tx))
                (Cons (%u64-to-rlp-item (tx-nonce tx))
                      (Cons (%u256-to-rlp-item (tx-gas-price tx))
                            (Cons (%u64-to-rlp-item (tx-gas-limit tx))
                                  (Cons (%address-to-rlp-item (tx-to tx))
                                        (Cons (%u256-to-rlp-item (tx-value tx))
                                              (Cons (rlp:RlpBytes (tx-data tx))
                                                    (Cons (%encode-access-list (tx-access-list tx))
                                                          Nil))))))))))))

      ((EIP1559Tx)
       (types:bytes-append
        (types:bytes-from-list (Cons 2 Nil))
        (rlp:rlp-encode
         (rlp:RlpList
          (Cons (%u64-to-rlp-item (tx-chain-id tx))
                (Cons (%u64-to-rlp-item (tx-nonce tx))
                      (Cons (%u256-to-rlp-item (tx-max-priority-fee tx))
                            (Cons (%u256-to-rlp-item (tx-max-fee tx))
                                  (Cons (%u64-to-rlp-item (tx-gas-limit tx))
                                        (Cons (%address-to-rlp-item (tx-to tx))
                                              (Cons (%u256-to-rlp-item (tx-value tx))
                                                    (Cons (rlp:RlpBytes (tx-data tx))
                                                          (Cons (%encode-access-list (tx-access-list tx))
                                                                Nil))))))))))))))))

;;; Coalton toplevel for signed-tx-encode (needs lisp interop for v computation)
(coalton-toplevel

  (declare signed-tx-encode (Transaction -> crypto:Signature -> types:Bytes))
  (define (signed-tx-encode tx sig)
    "Encode a signed transaction (ready for broadcast)"
    (let ((r-item (rlp:RlpBytes (crypto:signature-r sig)))
          (s-item (rlp:RlpBytes (crypto:signature-s sig)))
          (v-raw (crypto:signature-v sig)))
      (match (tx-type tx)
        ;; Legacy: RLP([nonce, gasPrice, gasLimit, to, value, data, v, r, s])
        ;; v = chainId * 2 + 35 + recovery_id (EIP-155)
        ((LegacyTx)
         (let ((chain-id (tx-chain-id tx)))
           (let ((v-value (lisp U64 (chain-id v-raw)
                            (cl:+ v-raw (cl:* chain-id 2) 35))))
             (rlp:rlp-encode
              (rlp:RlpList
               (Cons (%u64-to-rlp-item (tx-nonce tx))
                     (Cons (%u256-to-rlp-item (tx-gas-price tx))
                           (Cons (%u64-to-rlp-item (tx-gas-limit tx))
                                 (Cons (%address-to-rlp-item (tx-to tx))
                                       (Cons (%u256-to-rlp-item (tx-value tx))
                                             (Cons (rlp:RlpBytes (tx-data tx))
                                                   (Cons (%u64-to-rlp-item v-value)
                                                         (Cons r-item
                                                               (Cons s-item Nil))))))))))))))

        ((EIP2930Tx)
         (let ((v-u64 (lisp U64 (v-raw) v-raw)))
           (types:bytes-append
            (types:bytes-from-list (Cons 1 Nil))
            (rlp:rlp-encode
             (rlp:RlpList
              (Cons (%u64-to-rlp-item (tx-chain-id tx))
                    (Cons (%u64-to-rlp-item (tx-nonce tx))
                          (Cons (%u256-to-rlp-item (tx-gas-price tx))
                                (Cons (%u64-to-rlp-item (tx-gas-limit tx))
                                      (Cons (%address-to-rlp-item (tx-to tx))
                                            (Cons (%u256-to-rlp-item (tx-value tx))
                                                  (Cons (rlp:RlpBytes (tx-data tx))
                                                        (Cons (%encode-access-list (tx-access-list tx))
                                                              (Cons (%u64-to-rlp-item v-u64)
                                                                    (Cons r-item
                                                                          (Cons s-item Nil))))))))))))))))

        ((EIP1559Tx)
         (let ((v-u64 (lisp U64 (v-raw) v-raw)))
           (types:bytes-append
            (types:bytes-from-list (Cons 2 Nil))
            (rlp:rlp-encode
             (rlp:RlpList
              (Cons (%u64-to-rlp-item (tx-chain-id tx))
                    (Cons (%u64-to-rlp-item (tx-nonce tx))
                          (Cons (%u256-to-rlp-item (tx-max-priority-fee tx))
                                (Cons (%u256-to-rlp-item (tx-max-fee tx))
                                      (Cons (%u64-to-rlp-item (tx-gas-limit tx))
                                            (Cons (%address-to-rlp-item (tx-to tx))
                                                  (Cons (%u256-to-rlp-item (tx-value tx))
                                                        (Cons (rlp:RlpBytes (tx-data tx))
                                                              (Cons (%encode-access-list (tx-access-list tx))
                                                                    (Cons (%u64-to-rlp-item v-u64)
                                                                          (Cons r-item
                                                                                (Cons s-item Nil)))))))))))))))))))))
