(in-package #:web3/provider)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; High-level Ethereum JSON-RPC methods

  (declare %hex-result-to-u64 (String -> U64))
  (define (%hex-result-to-u64 hex-str)
    "Parse a 0x-prefixed hex string into a U64"
    (lisp U64 (hex-str)
      (cl:let ((str (cl:if (cl:and (cl:>= (cl:length hex-str) 2)
                                   (cl:string= hex-str "0x" :end1 2))
                           (cl:subseq hex-str 2)
                           hex-str)))
        (cl:if (cl:zerop (cl:length str))
               0
               (cl:parse-integer str :radix 16)))))

  (declare %hex-result-to-u256 (String -> types:U256))
  (define (%hex-result-to-u256 hex-str)
    "Parse a 0x-prefixed hex string into a U256"
    (lisp types:U256 (hex-str)
      (cl:let* ((str (cl:if (cl:and (cl:>= (cl:length hex-str) 2)
                                    (cl:string= hex-str "0x" :end1 2))
                            (cl:subseq hex-str 2)
                            hex-str))
                (n (cl:parse-integer str :radix 16)))
        (web3/types:u256-from-integer n))))

  (declare eth-chain-id (HttpProvider -> (types:Web3Result U64)))
  (define (eth-chain-id provider)
    "Get the chain ID"
    (match (json-rpc-call provider "eth_chainId" "[]")
      ((Err e) (Err e))
      ((Ok result) (Ok (%hex-result-to-u64 result)))))

  (declare eth-block-number (HttpProvider -> (types:Web3Result U64)))
  (define (eth-block-number provider)
    "Get the latest block number"
    (match (json-rpc-call provider "eth_blockNumber" "[]")
      ((Err e) (Err e))
      ((Ok result) (Ok (%hex-result-to-u64 result)))))

  (declare eth-get-balance (HttpProvider -> addr:Address -> (types:Web3Result types:U256)))
  (define (eth-get-balance provider address)
    "Get the balance of an address in wei"
    (let ((addr-hex (addr:address-to-hex address)))
      (let ((params (lisp String (addr-hex)
                      (cl:format cl:nil "[\"~A\",\"latest\"]" addr-hex))))
        (match (json-rpc-call provider "eth_getBalance" params)
          ((Err e) (Err e))
          ((Ok result) (Ok (%hex-result-to-u256 result)))))))

  (declare eth-get-transaction-count (HttpProvider -> addr:Address -> (types:Web3Result U64)))
  (define (eth-get-transaction-count provider address)
    "Get the transaction count (nonce) of an address"
    (let ((addr-hex (addr:address-to-hex address)))
      (let ((params (lisp String (addr-hex)
                      (cl:format cl:nil "[\"~A\",\"latest\"]" addr-hex))))
        (match (json-rpc-call provider "eth_getTransactionCount" params)
          ((Err e) (Err e))
          ((Ok result) (Ok (%hex-result-to-u64 result)))))))

  (declare eth-gas-price (HttpProvider -> (types:Web3Result types:U256)))
  (define (eth-gas-price provider)
    "Get the current gas price"
    (match (json-rpc-call provider "eth_gasPrice" "[]")
      ((Err e) (Err e))
      ((Ok result) (Ok (%hex-result-to-u256 result)))))

  (declare eth-estimate-gas (HttpProvider -> addr:Address -> (Optional addr:Address) ->
                             types:U256 -> types:Bytes -> (types:Web3Result U64)))
  (define (eth-estimate-gas provider from to value data)
    "Estimate gas for a transaction"
    (let ((from-hex (addr:address-to-hex from))
          (value-hex (types:hex-encode-prefixed (types:u256-to-bytes value)))
          (data-hex (types:hex-encode-prefixed data)))
      (let ((params
              (match to
                ((Some to-addr)
                 (let ((to-hex (addr:address-to-hex to-addr)))
                   (lisp String (from-hex to-hex value-hex data-hex)
                     (cl:format cl:nil "[{\"from\":\"~A\",\"to\":\"~A\",\"value\":\"~A\",\"data\":\"~A\"}]"
                                from-hex to-hex value-hex data-hex))))
                ((None)
                 (lisp String (from-hex value-hex data-hex)
                   (cl:format cl:nil "[{\"from\":\"~A\",\"value\":\"~A\",\"data\":\"~A\"}]"
                              from-hex value-hex data-hex))))))
        (match (json-rpc-call provider "eth_estimateGas" params)
          ((Err e) (Err e))
          ((Ok result) (Ok (%hex-result-to-u64 result)))))))

  (declare eth-send-raw-transaction (HttpProvider -> types:Bytes -> (types:Web3Result String)))
  (define (eth-send-raw-transaction provider raw-tx)
    "Send a signed raw transaction, returns transaction hash"
    (let ((tx-hex (types:hex-encode-prefixed raw-tx)))
      (let ((params (lisp String (tx-hex)
                      (cl:format cl:nil "[\"~A\"]" tx-hex))))
        (json-rpc-call provider "eth_sendRawTransaction" params))))

  (declare eth-call (HttpProvider -> (Optional addr:Address) -> addr:Address ->
                     types:Bytes -> (types:Web3Result types:Bytes)))
  (define (eth-call provider from to data)
    "Execute a read-only call"
    (let ((to-hex (addr:address-to-hex to))
          (data-hex (types:hex-encode-prefixed data)))
      (let ((params
              (match from
                ((Some from-addr)
                 (let ((from-hex (addr:address-to-hex from-addr)))
                   (lisp String (from-hex to-hex data-hex)
                     (cl:format cl:nil "[{\"from\":\"~A\",\"to\":\"~A\",\"data\":\"~A\"},\"latest\"]"
                                from-hex to-hex data-hex))))
                ((None)
                 (lisp String (to-hex data-hex)
                   (cl:format cl:nil "[{\"to\":\"~A\",\"data\":\"~A\"},\"latest\"]"
                              to-hex data-hex))))))
        (match (json-rpc-call provider "eth_call" params)
          ((Err e) (Err e))
          ((Ok result) (types:hex-decode result))))))

  (declare eth-get-transaction-receipt (HttpProvider -> String -> (types:Web3Result String)))
  (define (eth-get-transaction-receipt provider tx-hash)
    "Get a transaction receipt (returns raw JSON string)"
    (let ((params (lisp String (tx-hash)
                    (cl:format cl:nil "[\"~A\"]" tx-hash))))
      (json-rpc-call provider "eth_getTransactionReceipt" params)))

  ;;; =========================================================================
  ;;; Additional JSON-RPC Methods
  ;;; =========================================================================

  (declare eth-get-code (HttpProvider -> addr:Address -> (types:Web3Result types:Bytes)))
  (define (eth-get-code provider address)
    "Get the contract bytecode at an address"
    (let ((addr-hex (addr:address-to-hex address)))
      (let ((params (lisp String (addr-hex)
                      (cl:format cl:nil "[\"~A\",\"latest\"]" addr-hex))))
        (match (json-rpc-call provider "eth_getCode" params)
          ((Err e) (Err e))
          ((Ok result) (types:hex-decode result))))))

  (declare eth-get-storage-at (HttpProvider -> addr:Address -> types:U256 -> (types:Web3Result types:Bytes)))
  (define (eth-get-storage-at provider address slot)
    "Get the value at a storage position of an address"
    (let ((addr-hex (addr:address-to-hex address))
          (slot-hex (types:hex-encode-prefixed (types:u256-to-bytes slot))))
      (let ((params (lisp String (addr-hex slot-hex)
                      (cl:format cl:nil "[\"~A\",\"~A\",\"latest\"]" addr-hex slot-hex))))
        (match (json-rpc-call provider "eth_getStorageAt" params)
          ((Err e) (Err e))
          ((Ok result) (types:hex-decode result))))))

  (declare eth-max-priority-fee-per-gas (HttpProvider -> (types:Web3Result types:U256)))
  (define (eth-max-priority-fee-per-gas provider)
    "Get the suggested max priority fee per gas (EIP-1559)"
    (match (json-rpc-call provider "eth_maxPriorityFeePerGas" "[]")
      ((Err e) (Err e))
      ((Ok result) (Ok (%hex-result-to-u256 result)))))

  (declare eth-get-block-by-number (HttpProvider -> String -> Boolean -> (types:Web3Result (Optional String))))
  (define (eth-get-block-by-number provider block-tag full-txs)
    "Get block by number or tag (\"latest\", \"earliest\", \"pending\", or hex number).
     Returns None if block not found. Result is raw JSON for parsing with web3/block."
    (let ((full-str (if full-txs "true" "false")))
      (let ((params (lisp String (block-tag full-str)
                      (cl:format cl:nil "[\"~A\",~A]" block-tag full-str))))
        (json-rpc-call-nullable provider "eth_getBlockByNumber" params))))

  (declare eth-get-block-by-hash (HttpProvider -> String -> Boolean -> (types:Web3Result (Optional String))))
  (define (eth-get-block-by-hash provider block-hash full-txs)
    "Get block by hash. Returns None if block not found.
     Result is raw JSON for parsing with web3/block."
    (let ((full-str (if full-txs "true" "false")))
      (let ((params (lisp String (block-hash full-str)
                      (cl:format cl:nil "[\"~A\",~A]" block-hash full-str))))
        (json-rpc-call-nullable provider "eth_getBlockByHash" params))))

  (declare eth-get-transaction-by-hash (HttpProvider -> String -> (types:Web3Result (Optional String))))
  (define (eth-get-transaction-by-hash provider tx-hash)
    "Get transaction by hash. Returns None if not found.
     Result is raw JSON string."
    (let ((params (lisp String (tx-hash)
                    (cl:format cl:nil "[\"~A\"]" tx-hash))))
      (json-rpc-call-nullable provider "eth_getTransactionByHash" params)))

  (declare eth-fee-history (HttpProvider -> U64 -> String -> String -> (types:Web3Result String)))
  (define (eth-fee-history provider block-count newest-block reward-percentiles)
    "Get fee history. block-count is number of blocks, newest-block is tag/hex,
     reward-percentiles is a JSON array string like \"[25,50,75]\".
     Returns raw JSON result string."
    (let ((params (lisp String (block-count newest-block reward-percentiles)
                    (cl:format cl:nil "[\"0x~X\",\"~A\",~A]" block-count newest-block reward-percentiles))))
      (json-rpc-call provider "eth_feeHistory" params)))

  (declare eth-syncing (HttpProvider -> (types:Web3Result Boolean)))
  (define (eth-syncing provider)
    "Check if the node is syncing. Returns True if syncing, False if synced."
    (match (json-rpc-call-nullable provider "eth_syncing" "[]")
      ((Err e) (Err e))
      ((Ok (None)) (Ok False))
      ((Ok (Some _)) (Ok True)))))

;;; =========================================================================
;;; Transaction Receipt Waiting (CL-level polling loop)
;;; =========================================================================

(cl:defun %wait-for-receipt-impl (provider tx-hash max-attempts poll-interval-ms)
  "Poll eth_getTransactionReceipt until receipt is found or attempts exhausted."
  (cl:let ((params (cl:format cl:nil "[\"~A\"]" tx-hash))
           (delay-seconds (cl:/ poll-interval-ms 1000.0)))
    (cl:block done
      (cl:dotimes (i max-attempts
                     (coalton-prelude:Err
                      (web3/types:ProviderError
                       (cl:format cl:nil "Transaction not confirmed after ~A attempts" max-attempts))))
        (cl:when (cl:> i 0)
          (cl:sleep delay-seconds))
        (cl:let ((result (coalton:coalton
                           (web3/provider:json-rpc-call-nullable
                             (coalton:lisp web3/provider:HttpProvider () provider)
                             "eth_getTransactionReceipt"
                             (coalton:lisp coalton:String () params)))))
          (cl:cond
            ;; RPC error → return immediately
            ((cl:not (web3/types:%result-ok-p result))
             (cl:return-from done result))
            ;; Got a receipt (Some) → unwrap and return Ok
            ((web3/types:%is-some-p (web3/types:%unwrap-ok result))
             (cl:return-from done
               (coalton-prelude:Ok
                (web3/types:%unwrap-some (web3/types:%unwrap-ok result)))))
            ;; None → receipt not yet available, continue polling
            ))))))

(coalton-toplevel
  (declare wait-for-transaction-receipt
    (HttpProvider -> String -> UFix -> UFix -> (types:Web3Result String)))
  (define (wait-for-transaction-receipt provider tx-hash max-attempts poll-interval-ms)
    "Poll for a transaction receipt until confirmed or max attempts reached.
     max-attempts is the number of polling attempts before timeout.
     poll-interval-ms is the delay between polls in milliseconds.
     Typical usage: (wait-for-transaction-receipt provider hash 60 2000)
     Returns the receipt as a raw JSON string for parsing with web3/receipt."
    (lisp (types:Web3Result String) (provider tx-hash max-attempts poll-interval-ms)
      (%wait-for-receipt-impl provider tx-hash max-attempts poll-interval-ms))))
