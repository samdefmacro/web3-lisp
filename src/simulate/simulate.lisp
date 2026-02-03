(in-package #:web3/simulate)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Block Specifier
  ;;; =========================================================================

  (define-type BlockSpec
    "Specifies which block to use for simulation"
    BlockLatest
    BlockPending
    BlockEarliest
    (BlockNumber U64))

  (declare block-spec-to-string (BlockSpec -> String))
  (define (block-spec-to-string spec)
    "Convert block spec to JSON-RPC block parameter"
    (match spec
      ((BlockLatest) "\"latest\"")
      ((BlockPending) "\"pending\"")
      ((BlockEarliest) "\"earliest\"")
      ((BlockNumber n)
       (lisp String (n)
         (cl:format cl:nil "\"0x~X\"" n)))))

  ;;; =========================================================================
  ;;; Call Options
  ;;; =========================================================================

  (define-struct CallOptions
    "Options for eth_call and simulation"
    (call-options-block BlockSpec)
    (call-options-from (Optional addr:Address))
    (call-options-gas-limit (Optional U64))
    (call-options-gas-price (Optional types:U256))
    (call-options-value (Optional types:U256)))

  (declare make-call-options (BlockSpec -> (Optional addr:Address) -> (Optional U64) ->
                              (Optional types:U256) -> (Optional types:U256) -> CallOptions))
  (define (make-call-options block from gas-limit gas-price value)
    (CallOptions block from gas-limit gas-price value))

  (declare default-call-options (Unit -> CallOptions))
  (define (default-call-options _)
    "Create default call options (latest block, no overrides)"
    (CallOptions BlockLatest None None None None))

  ;;; =========================================================================
  ;;; Simulation Result
  ;;; =========================================================================

  (define-struct SimulationResult
    "Result of a transaction simulation"
    (simulation-return-data types:Bytes)
    (simulation-gas-used U64)
    (simulation-success Boolean))

  (declare make-simulation-result (types:Bytes -> U64 -> Boolean -> SimulationResult))
  (define (make-simulation-result data gas success)
    (SimulationResult data gas success))

  ;;; =========================================================================
  ;;; Gas Estimate Result
  ;;; =========================================================================

  (define-struct GasEstimate
    "Comprehensive gas estimate for a transaction"
    (gas-estimate-gas-limit U64)
    (gas-estimate-gas-price types:U256)
    (gas-estimate-max-fee types:U256)
    (gas-estimate-max-priority-fee types:U256)
    (gas-estimate-total-cost types:U256))

  (declare make-gas-estimate (U64 -> types:U256 -> types:U256 -> types:U256 ->
                              types:U256 -> GasEstimate))
  (define (make-gas-estimate limit price max-fee priority total)
    (GasEstimate limit price max-fee priority total))

  ;;; =========================================================================
  ;;; Access List Result
  ;;; =========================================================================

  (define-struct AccessListResult
    "Result of eth_createAccessList"
    (access-list-result-list tx:AccessList)
    (access-list-result-gas-used U64))

  (declare make-access-list-result (tx:AccessList -> U64 -> AccessListResult))
  (define (make-access-list-result al gas)
    (AccessListResult al gas))

  ;;; =========================================================================
  ;;; Internal Helpers
  ;;; =========================================================================

  (declare %hex-result-to-u64 (String -> U64))
  (define (%hex-result-to-u64 hex-str)
    "Parse hex string to U64"
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
    "Parse hex string to U256"
    (lisp types:U256 (hex-str)
      (cl:let* ((str (cl:if (cl:and (cl:>= (cl:length hex-str) 2)
                                    (cl:string= hex-str "0x" :end1 2))
                            (cl:subseq hex-str 2)
                            hex-str))
                (n (cl:if (cl:zerop (cl:length str))
                          0
                          (cl:parse-integer str :radix 16))))
        (web3/types::%bignum-to-u256 n))))

  (declare %optional-address-to-json (String -> (Optional addr:Address) -> String))
  (define (%optional-address-to-json prefix opt-addr)
    "Convert optional address to JSON field or empty string"
    (match opt-addr
      ((None) "")
      ((Some addr)
       (let ((hex (addr:address-to-hex addr)))
         (lisp String (prefix hex)
           (cl:format cl:nil ",\"~A\":\"~A\"" prefix hex))))))

  (declare %optional-u64-to-json (String -> (Optional U64) -> String))
  (define (%optional-u64-to-json prefix opt-val)
    "Convert optional U64 to JSON field or empty string"
    (match opt-val
      ((None) "")
      ((Some val)
       (lisp String (prefix val)
         (cl:format cl:nil ",\"~A\":\"0x~X\"" prefix val)))))

  (declare %optional-u256-to-json (String -> (Optional types:U256) -> String))
  (define (%optional-u256-to-json prefix opt-val)
    "Convert optional U256 to JSON field or empty string"
    (match opt-val
      ((None) "")
      ((Some val)
       (let ((hex (types:hex-encode-prefixed (types:u256-to-bytes val))))
         (lisp String (prefix hex)
           (cl:format cl:nil ",\"~A\":\"~A\"" prefix hex))))))

  ;;; =========================================================================
  ;;; Core Simulation Functions
  ;;; =========================================================================

  (declare simulate-call (provider:HttpProvider -> addr:Address ->
                          types:Bytes -> CallOptions -> (types:Web3Result types:Bytes)))
  (define (simulate-call provider to data opts)
    "Simulate a call with options (wraps eth_call with more control)"
    (match opts
      ((CallOptions block-spec from-opt gas-opt price-opt value-opt)
       (let ((to-hex (addr:address-to-hex to))
             (data-hex (types:hex-encode-prefixed data))
             (block-str (block-spec-to-string block-spec))
             (from-json (%optional-address-to-json "from" from-opt))
             (gas-json (%optional-u64-to-json "gas" gas-opt))
             (price-json (%optional-u256-to-json "gasPrice" price-opt))
             (value-json (%optional-u256-to-json "value" value-opt)))
         (let ((params (lisp String (to-hex data-hex block-str from-json gas-json price-json value-json)
                         (cl:format cl:nil "[{\"to\":\"~A\",\"data\":\"~A\"~A~A~A~A},~A]"
                                    to-hex data-hex from-json gas-json price-json value-json block-str))))
           (match (provider:json-rpc-call provider "eth_call" params)
             ((Err e) (Err e))
             ((Ok result) (types:hex-decode result))))))))

  (declare simulate-transaction (provider:HttpProvider -> tx:Transaction ->
                                 CallOptions -> (types:Web3Result types:Bytes)))
  (define (simulate-transaction provider transaction opts)
    "Simulate a full transaction object"
    (match (tx:tx-to transaction)
      ((None) (Err (types:TransactionError "Cannot simulate contract creation")))
      ((Some to-addr)
       (match opts
         ((CallOptions block-spec from-opt _ _ _)
          (let ((to-hex (addr:address-to-hex to-addr))
                (data-hex (types:hex-encode-prefixed (tx:tx-data transaction)))
                (value-hex (types:hex-encode-prefixed (types:u256-to-bytes (tx:tx-value transaction))))
                (gas-limit (tx:tx-gas-limit transaction))
                (block-str (block-spec-to-string block-spec))
                (from-json (%optional-address-to-json "from" from-opt)))
            (let ((params (lisp String (to-hex data-hex value-hex gas-limit block-str from-json)
                            (cl:format cl:nil "[{\"to\":\"~A\",\"data\":\"~A\",\"value\":\"~A\",\"gas\":\"0x~X\"~A},~A]"
                                       to-hex data-hex value-hex gas-limit from-json block-str))))
              (match (provider:json-rpc-call provider "eth_call" params)
                ((Err e) (Err e))
                ((Ok result) (types:hex-decode result))))))))))

  ;;; =========================================================================
  ;;; Gas Estimation Functions
  ;;; =========================================================================

  (declare estimate-gas (provider:HttpProvider -> addr:Address ->
                         types:Bytes -> types:U256 -> CallOptions ->
                         (types:Web3Result U64)))
  (define (estimate-gas provider to data value opts)
    "Estimate gas for a call with options"
    (match opts
      ((CallOptions block-spec from-opt _ _ _)
       (let ((to-hex (addr:address-to-hex to))
             (data-hex (types:hex-encode-prefixed data))
             (value-hex (types:hex-encode-prefixed (types:u256-to-bytes value)))
             (block-str (block-spec-to-string block-spec))
             (from-json (%optional-address-to-json "from" from-opt)))
         (let ((params (lisp String (to-hex data-hex value-hex block-str from-json)
                         (cl:format cl:nil "[{\"to\":\"~A\",\"data\":\"~A\",\"value\":\"~A\"~A},~A]"
                                    to-hex data-hex value-hex from-json block-str))))
           (match (provider:json-rpc-call provider "eth_estimateGas" params)
             ((Err e) (Err e))
             ((Ok result) (Ok (%hex-result-to-u64 result)))))))))

  (declare estimate-transaction-gas (provider:HttpProvider -> tx:Transaction ->
                                     (types:Web3Result U64)))
  (define (estimate-transaction-gas provider transaction)
    "Estimate gas for a full transaction object"
    (let ((data-hex (types:hex-encode-prefixed (tx:tx-data transaction)))
          (value-hex (types:hex-encode-prefixed (types:u256-to-bytes (tx:tx-value transaction)))))
      (match (tx:tx-to transaction)
        ((None)
         (let ((params (lisp String (value-hex data-hex)
                         (cl:format cl:nil "[{\"value\":\"~A\",\"data\":\"~A\"}]"
                                    value-hex data-hex))))
           (match (provider:json-rpc-call provider "eth_estimateGas" params)
             ((Err e) (Err e))
             ((Ok result) (Ok (%hex-result-to-u64 result))))))
        ((Some to-addr)
         (let ((to-hex (addr:address-to-hex to-addr)))
           (let ((params (lisp String (to-hex value-hex data-hex)
                           (cl:format cl:nil "[{\"to\":\"~A\",\"value\":\"~A\",\"data\":\"~A\"}]"
                                      to-hex value-hex data-hex))))
             (match (provider:json-rpc-call provider "eth_estimateGas" params)
               ((Err e) (Err e))
               ((Ok result) (Ok (%hex-result-to-u64 result))))))))))

  (declare estimate-transaction-cost (provider:HttpProvider -> tx:Transaction ->
                                      (types:Web3Result GasEstimate)))
  (define (estimate-transaction-cost provider transaction)
    "Estimate total cost including gas price from network"
    (match (estimate-transaction-gas provider transaction)
      ((Err e) (Err e))
      ((Ok gas-limit)
       (let ((buffered-gas (gas:add-gas-buffer (lisp UFix (gas-limit) gas-limit) 20)))
         (match (provider:eth-gas-price provider)
           ((Err e) (Err e))
           ((Ok gas-price)
            (let ((priority-fee-result
                    (provider:json-rpc-call provider "eth_maxPriorityFeePerGas" "[]")))
              (match priority-fee-result
                ((Err _)
                 (let ((total-cost (types:u256-mul gas-price
                                                   (types:u256-from-integer (into buffered-gas)))))
                   (Ok (GasEstimate (lisp U64 (buffered-gas) buffered-gas) gas-price gas-price
                                    (types:u256-zero) total-cost))))
                ((Ok priority-hex)
                 (let ((priority-fee (%hex-result-to-u256 priority-hex))
                       (max-fee gas-price))
                   (let ((total-cost (types:u256-mul max-fee
                                                     (types:u256-from-integer (into buffered-gas)))))
                     (Ok (GasEstimate (lisp U64 (buffered-gas) buffered-gas) gas-price max-fee
                                      priority-fee total-cost)))))))))))))

  ;;; =========================================================================
  ;;; Access List Creation (EIP-2930)
  ;;; =========================================================================

  (declare create-access-list (provider:HttpProvider -> addr:Address ->
                               types:Bytes -> types:U256 -> (Optional addr:Address) ->
                               (types:Web3Result AccessListResult)))
  (define (create-access-list provider to data value from)
    "Create an access list for a transaction using eth_createAccessList"
    (let ((to-hex (addr:address-to-hex to))
          (value-hex (types:hex-encode-prefixed (types:u256-to-bytes value)))
          (data-hex (types:hex-encode-prefixed data))
          (from-json (%optional-address-to-json "from" from)))
      (let ((params (lisp String (to-hex value-hex data-hex from-json)
                      (cl:format cl:nil "[{\"to\":\"~A\",\"value\":\"~A\",\"data\":\"~A\"~A},\"latest\"]"
                                 to-hex value-hex data-hex from-json))))
        (match (provider:json-rpc-call provider "eth_createAccessList" params)
          ((Err e) (Err e))
          ((Ok result)
           (lisp (types:Web3Result AccessListResult) (result)
             (cl:handler-case
                 (cl:let* ((json (cl-json:decode-json-from-string result))
                           (access-list-json (cl:cdr (cl:assoc :access-list json)))
                           (gas-used-str (cl:cdr (cl:assoc :gas-used json)))
                           (gas-used (cl:parse-integer (cl:subseq gas-used-str 2) :radix 16))
                           (access-list
                             (cl:loop :for entry :in access-list-json
                                      :collect
                                      (cl:let* ((addr-str (cl:cdr (cl:assoc :address entry)))
                                                (keys-json (cl:cdr (cl:assoc :storage-keys entry)))
                                                (addr-result (coalton (addr:address-from-hex
                                                                       (lisp String () addr-str))))
                                                (addr (cl:typecase addr-result
                                                        (coalton-library/classes::result/ok
                                                         (cl:slot-value addr-result
                                                                        'coalton-library/classes::_0))
                                                        (cl:t (coalton (addr:address-zero)))))
                                                (keys (cl:loop :for key-str :in keys-json
                                                               :collect
                                                               (cl:let ((key-result
                                                                          (coalton (types:hex-decode
                                                                                    (lisp String () key-str)))))
                                                                 (cl:typecase key-result
                                                                   (coalton-library/classes::result/ok
                                                                    (cl:slot-value key-result
                                                                                   'coalton-library/classes::_0))
                                                                   (cl:t (coalton (types:bytes-empty coalton:Unit))))))))
                                        (coalton-prelude:Tuple addr keys)))))
                   (coalton-prelude:Ok (AccessListResult access-list gas-used)))
               (cl:error (e)
                 (coalton-prelude:Err
                  (web3/types:ProviderError
                   (cl:format cl:nil "Failed to parse access list: ~A" e)))))))))))

  ;;; =========================================================================
  ;;; Transaction Population Helper
  ;;; =========================================================================

  (declare populate-transaction (provider:HttpProvider -> addr:Address ->
                                 tx:TransactionType -> (Optional addr:Address) ->
                                 types:U256 -> types:Bytes ->
                                 (types:Web3Result tx:Transaction)))
  (define (populate-transaction provider from tx-type to value data)
    "Populate a transaction with network values (nonce, gas, fees)"
    (match (provider:eth-get-transaction-count provider from)
      ((Err e) (Err e))
      ((Ok nonce)
       (let ((data-hex (types:hex-encode-prefixed data))
             (value-hex (types:hex-encode-prefixed (types:u256-to-bytes value))))
         (let ((gas-result
                 (match to
                   ((Some to-addr)
                    (let ((to-hex (addr:address-to-hex to-addr)))
                      (let ((params (lisp String (to-hex value-hex data-hex)
                                      (cl:format cl:nil "[{\"to\":\"~A\",\"value\":\"~A\",\"data\":\"~A\"}]"
                                                 to-hex value-hex data-hex))))
                        (match (provider:json-rpc-call provider "eth_estimateGas" params)
                          ((Err e) (Err e))
                          ((Ok result) (Ok (%hex-result-to-u64 result)))))))
                   ((None)
                    (let ((params (lisp String (value-hex data-hex)
                                    (cl:format cl:nil "[{\"value\":\"~A\",\"data\":\"~A\"}]"
                                               value-hex data-hex))))
                      (match (provider:json-rpc-call provider "eth_estimateGas" params)
                        ((Err e) (Err e))
                        ((Ok result) (Ok (%hex-result-to-u64 result)))))))))
           (match gas-result
             ((Err e) (Err e))
             ((Ok estimated-gas)
              (let ((gas-limit-ufix (gas:add-gas-buffer (lisp UFix (estimated-gas) estimated-gas) 20)))
                (let ((gas-limit (lisp U64 (gas-limit-ufix) gas-limit-ufix)))
                  (match (provider:eth-chain-id provider)
                    ((Err e) (Err e))
                    ((Ok chain-id)
                     (match tx-type
                       ((tx:LegacyTx)
                        (match (provider:eth-gas-price provider)
                          ((Err e) (Err e))
                          ((Ok gas-price)
                           (Ok (tx:make-transaction tx:LegacyTx chain-id nonce gas-price
                                                    (types:u256-zero) gas-limit to value data Nil)))))
                       ((tx:EIP2930Tx)
                        (match (provider:eth-gas-price provider)
                          ((Err e) (Err e))
                          ((Ok gas-price)
                           (Ok (tx:make-transaction tx:EIP2930Tx chain-id nonce gas-price
                                                    (types:u256-zero) gas-limit to value data Nil)))))
                       ((tx:EIP1559Tx)
                        (match (provider:eth-gas-price provider)
                          ((Err e) (Err e))
                          ((Ok base-fee)
                           (let ((priority-result
                                   (provider:json-rpc-call provider "eth_maxPriorityFeePerGas" "[]")))
                             (let ((priority-fee
                                     (match priority-result
                                       ((Err _) (types:u256-from-integer 1500000000))
                                       ((Ok hex) (%hex-result-to-u256 hex)))))
                               (let ((max-fee (types:u256-add
                                               (types:u256-mul base-fee (types:u256-from-integer 2))
                                               priority-fee)))
                                 (Ok (tx:make-transaction tx:EIP1559Tx chain-id nonce priority-fee
                                                          max-fee gas-limit to value data Nil))))))))
                       ((tx:EIP4844Tx)
                        (Err (types:TransactionError
                              "Use make-blob-transaction for EIP-4844 transactions")))))))))))))))

) ; end coalton-toplevel
