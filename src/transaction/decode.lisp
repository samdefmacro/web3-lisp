(in-package #:web3/transaction)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; Transaction decoding

  (declare tx-decode (types:Bytes -> (types:Web3Result Transaction)))
  (define (tx-decode raw)
    "Decode a raw transaction from bytes"
    (if (== (types:bytes-length raw) 0)
        (Err (types:TransactionError "Empty transaction"))
        (let ((first-byte (types:bytes-ref-unsafe 0 raw)))
          (cond
            ;; EIP-2718 typed transactions: first byte < 0x7f
            ;; Type 1: EIP-2930
            ((== first-byte 1)
             (%decode-eip2930 (types:bytes-drop 1 raw)))

            ;; Type 2: EIP-1559
            ((== first-byte 2)
             (%decode-eip1559 (types:bytes-drop 1 raw)))

            ;; Type 3: EIP-4844 (blob transaction)
            ((== first-byte 3)
             (%decode-eip4844 (types:bytes-drop 1 raw)))

            ;; Legacy: first byte >= 0xc0 (RLP list prefix)
            ((>= first-byte #xc0)
             (%decode-legacy raw))

            (True
             (Err (types:TransactionError "Unknown transaction type")))))))

  ;;; Internal decoders

  (declare %rlp-item-to-bytes (rlp:RlpItem -> types:Bytes))
  (define (%rlp-item-to-bytes item)
    "Extract bytes from an RLP item"
    (match item
      ((rlp:RlpBytes b) b)
      ((rlp:RlpList _) types:bytes-empty)))

  (declare %rlp-bytes-to-u64 (types:Bytes -> U64))
  (define (%rlp-bytes-to-u64 bytes)
    "Convert big-endian bytes to U64"
    (lisp U64 (bytes)
      (cl:let ((result 0))
        (cl:loop :for i :from 0 :below (cl:length bytes)
                 :do (cl:setf result (cl:+ (cl:ash result 8) (cl:aref bytes i))))
        result)))

  (declare %rlp-bytes-to-u256 (types:Bytes -> types:U256))
  (define (%rlp-bytes-to-u256 bytes)
    "Convert big-endian bytes to U256"
    (lisp types:U256 (bytes)
      (cl:let ((n 0))
        (cl:loop :for i :from 0 :below (cl:length bytes)
                 :do (cl:setf n (cl:+ (cl:ash n 8) (cl:aref bytes i))))
        (web3/types:u256-from-integer n))))

  (declare %rlp-bytes-to-address (types:Bytes -> (Optional addr:Address)))
  (define (%rlp-bytes-to-address bytes)
    "Convert bytes to optional address (empty = None)"
    (if (== (types:bytes-length bytes) 0)
        None
        (match (addr:address-from-bytes bytes)
          ((Ok a) (Some a))
          ((Err _) None))))

  (declare %decode-storage-keys (rlp:RlpItem -> (List types:Bytes)))
  (define (%decode-storage-keys item)
    "Extract storage keys from an RLP item"
    (match item
      ((rlp:RlpList key-items)
       (map %rlp-item-to-bytes key-items))
      ((rlp:RlpBytes _) Nil)))

  (declare %empty-access-entry AccessListEntry)
  (define %empty-access-entry
    (Tuple addr:address-zero (the (List types:Bytes) Nil)))

  (declare %decode-access-list-items ((List rlp:RlpItem) -> AccessList))
  (define (%decode-access-list-items items)
    "Decode access list from RLP items"
    (map (fn (item)
           (match item
             ((rlp:RlpList entry-items)
              (match entry-items
                ((Cons addr-item (Cons keys-item (Nil)))
                 (let ((address (match (addr:address-from-bytes (%rlp-item-to-bytes addr-item))
                                  ((Ok a) a)
                                  ((Err _) addr:address-zero))))
                   (let ((keys (%decode-storage-keys keys-item)))
                     (Tuple address keys))))
                (_ %empty-access-entry)))
             (_ %empty-access-entry)))
         items))

  (declare %nth-rlp-item (UFix -> (List rlp:RlpItem) -> rlp:RlpItem))
  (define (%nth-rlp-item n items)
    "Get nth RLP item from list (returns empty bytes if out of bounds)"
    (match items
      ((Nil) (rlp:RlpBytes types:bytes-empty))
      ((Cons first rest)
       (if (== n 0)
           first
           (%nth-rlp-item (- n 1) rest)))))

  (declare %decode-legacy (types:Bytes -> (types:Web3Result Transaction)))
  (define (%decode-legacy raw)
    "Decode a legacy transaction"
    (match (rlp:rlp-decode raw)
      ((Err _e) (Err (types:TransactionError "Invalid RLP in legacy tx")))
      ((Ok (Tuple item _))
       (match item
         ((rlp:RlpList items)
          (if (< (list:length items) 9)
              (Err (types:TransactionError "Legacy tx needs 9 fields"))
              ;; [nonce, gasPrice, gasLimit, to, value, data, v, r, s]
              (let ((nonce (%rlp-bytes-to-u64 (%rlp-item-to-bytes (%nth-rlp-item 0 items))))
                    (gas-price (%rlp-bytes-to-u256 (%rlp-item-to-bytes (%nth-rlp-item 1 items))))
                    (gas-limit (%rlp-bytes-to-u64 (%rlp-item-to-bytes (%nth-rlp-item 2 items))))
                    (to (%rlp-bytes-to-address (%rlp-item-to-bytes (%nth-rlp-item 3 items))))
                    (value (%rlp-bytes-to-u256 (%rlp-item-to-bytes (%nth-rlp-item 4 items))))
                    (data (%rlp-item-to-bytes (%nth-rlp-item 5 items)))
                    (v-raw (%rlp-bytes-to-u64 (%rlp-item-to-bytes (%nth-rlp-item 6 items)))))
                ;; Derive chain-id from v (EIP-155: v = chainId * 2 + 35 + recovery_id)
                (let ((chain-id (if (>= v-raw 35)
                                    (lisp U64 (v-raw) (cl:floor (cl:- v-raw 35) 2))
                                    0)))
                  (Ok (make-transaction LegacyTx chain-id nonce gas-price
                                        types:u256-zero gas-limit to value data Nil))))))
         (_ (Err (types:TransactionError "Expected RLP list for legacy tx")))))))

  (declare %decode-eip2930 (types:Bytes -> (types:Web3Result Transaction)))
  (define (%decode-eip2930 payload)
    "Decode an EIP-2930 transaction (after removing type prefix)"
    (match (rlp:rlp-decode payload)
      ((Err _e) (Err (types:TransactionError "Invalid RLP in EIP-2930 tx")))
      ((Ok (Tuple item _))
       (match item
         ((rlp:RlpList items)
          (if (< (list:length items) 8)
              (Err (types:TransactionError "EIP-2930 tx needs 8+ fields"))
              ;; [chainId, nonce, gasPrice, gasLimit, to, value, data, accessList, ...]
              (let ((chain-id (%rlp-bytes-to-u64 (%rlp-item-to-bytes (%nth-rlp-item 0 items))))
                    (nonce (%rlp-bytes-to-u64 (%rlp-item-to-bytes (%nth-rlp-item 1 items))))
                    (gas-price (%rlp-bytes-to-u256 (%rlp-item-to-bytes (%nth-rlp-item 2 items))))
                    (gas-limit (%rlp-bytes-to-u64 (%rlp-item-to-bytes (%nth-rlp-item 3 items))))
                    (to (%rlp-bytes-to-address (%rlp-item-to-bytes (%nth-rlp-item 4 items))))
                    (value (%rlp-bytes-to-u256 (%rlp-item-to-bytes (%nth-rlp-item 5 items))))
                    (data (%rlp-item-to-bytes (%nth-rlp-item 6 items)))
                    (access-list (match (%nth-rlp-item 7 items)
                                   ((rlp:RlpList al-items) (%decode-access-list-items al-items))
                                   (_ Nil))))
                (Ok (make-transaction EIP2930Tx chain-id nonce gas-price
                                      types:u256-zero gas-limit to value data access-list)))))
         (_ (Err (types:TransactionError "Expected RLP list for EIP-2930 tx")))))))

  (declare %decode-eip1559 (types:Bytes -> (types:Web3Result Transaction)))
  (define (%decode-eip1559 payload)
    "Decode an EIP-1559 transaction (after removing type prefix)"
    (match (rlp:rlp-decode payload)
      ((Err _e) (Err (types:TransactionError "Invalid RLP in EIP-1559 tx")))
      ((Ok (Tuple item _))
       (match item
         ((rlp:RlpList items)
          (if (< (list:length items) 9)
              (Err (types:TransactionError "EIP-1559 tx needs 9+ fields"))
              ;; [chainId, nonce, maxPriorityFee, maxFee, gasLimit, to, value, data, accessList, ...]
              (let ((chain-id (%rlp-bytes-to-u64 (%rlp-item-to-bytes (%nth-rlp-item 0 items))))
                    (nonce (%rlp-bytes-to-u64 (%rlp-item-to-bytes (%nth-rlp-item 1 items))))
                    (max-priority (%rlp-bytes-to-u256 (%rlp-item-to-bytes (%nth-rlp-item 2 items))))
                    (max-fee (%rlp-bytes-to-u256 (%rlp-item-to-bytes (%nth-rlp-item 3 items))))
                    (gas-limit (%rlp-bytes-to-u64 (%rlp-item-to-bytes (%nth-rlp-item 4 items))))
                    (to (%rlp-bytes-to-address (%rlp-item-to-bytes (%nth-rlp-item 5 items))))
                    (value (%rlp-bytes-to-u256 (%rlp-item-to-bytes (%nth-rlp-item 6 items))))
                    (data (%rlp-item-to-bytes (%nth-rlp-item 7 items)))
                    (access-list (match (%nth-rlp-item 8 items)
                                   ((rlp:RlpList al-items) (%decode-access-list-items al-items))
                                   (_ Nil))))
                (Ok (make-transaction EIP1559Tx chain-id nonce max-priority max-fee
                                      gas-limit to value data access-list)))))
         (_ (Err (types:TransactionError "Expected RLP list for EIP-1559 tx")))))))

  (declare %decode-blob-versioned-hashes (rlp:RlpItem -> BlobVersionedHashes))
  (define (%decode-blob-versioned-hashes item)
    "Decode blob versioned hashes from an RLP item"
    (match item
      ((rlp:RlpList hash-items)
       (map %rlp-item-to-bytes hash-items))
      ((rlp:RlpBytes _) Nil)))

  (declare %decode-eip4844 (types:Bytes -> (types:Web3Result Transaction)))
  (define (%decode-eip4844 payload)
    "Decode an EIP-4844 blob transaction (after removing type prefix)"
    (match (rlp:rlp-decode payload)
      ((Err _e) (Err (types:TransactionError "Invalid RLP in EIP-4844 tx")))
      ((Ok (Tuple item _))
       (match item
         ((rlp:RlpList items)
          (if (< (list:length items) 11)
              (Err (types:TransactionError "EIP-4844 tx needs 11+ fields"))
              ;; [chainId, nonce, maxPriorityFee, maxFee, gasLimit, to, value, data,
              ;;  accessList, maxFeePerBlobGas, blobVersionedHashes, ...]
              (let ((chain-id (%rlp-bytes-to-u64 (%rlp-item-to-bytes (%nth-rlp-item 0 items))))
                    (nonce (%rlp-bytes-to-u64 (%rlp-item-to-bytes (%nth-rlp-item 1 items))))
                    (max-priority (%rlp-bytes-to-u256 (%rlp-item-to-bytes (%nth-rlp-item 2 items))))
                    (max-fee (%rlp-bytes-to-u256 (%rlp-item-to-bytes (%nth-rlp-item 3 items))))
                    (gas-limit (%rlp-bytes-to-u64 (%rlp-item-to-bytes (%nth-rlp-item 4 items))))
                    (to (%rlp-bytes-to-address (%rlp-item-to-bytes (%nth-rlp-item 5 items))))
                    (value (%rlp-bytes-to-u256 (%rlp-item-to-bytes (%nth-rlp-item 6 items))))
                    (data (%rlp-item-to-bytes (%nth-rlp-item 7 items)))
                    (access-list (match (%nth-rlp-item 8 items)
                                   ((rlp:RlpList al-items) (%decode-access-list-items al-items))
                                   (_ Nil)))
                    (max-fee-per-blob-gas (%rlp-bytes-to-u256 (%rlp-item-to-bytes (%nth-rlp-item 9 items))))
                    (blob-hashes (%decode-blob-versioned-hashes (%nth-rlp-item 10 items))))
                (Ok (make-blob-transaction chain-id nonce max-priority max-fee
                                           gas-limit to value data access-list
                                           max-fee-per-blob-gas blob-hashes)))))
         (_ (Err (types:TransactionError "Expected RLP list for EIP-4844 tx"))))))))
