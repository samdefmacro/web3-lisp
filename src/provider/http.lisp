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
        (cl:parse-integer str :radix 16))))

  (declare %hex-result-to-u256 (String -> types:U256))
  (define (%hex-result-to-u256 hex-str)
    "Parse a 0x-prefixed hex string into a U256"
    (lisp types:U256 (hex-str)
      (cl:let* ((str (cl:if (cl:and (cl:>= (cl:length hex-str) 2)
                                    (cl:string= hex-str "0x" :end1 2))
                            (cl:subseq hex-str 2)
                            hex-str))
                (n (cl:parse-integer str :radix 16)))
        (web3/types::%bignum-to-u256 n))))

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
      (json-rpc-call provider "eth_getTransactionReceipt" params))))
