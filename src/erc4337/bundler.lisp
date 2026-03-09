(in-package #:web3/erc4337)
(named-readtables:in-readtable coalton:coalton)

;;; =========================================================================
;;; CL Helpers for JSON Serialization
;;; =========================================================================

(cl:defun %user-op-to-json (op)
  "Serialize a v0.6 UserOperation to a JSON object string for bundler RPC.
   All numeric fields are hex-encoded, all bytes fields are hex-encoded."
  (cl:let ((sender (coalton:coalton
                    (web3/address:address-to-hex
                     (user-op-sender (coalton:lisp UserOperation () op)))))
           (nonce (web3/types:u256-to-integer
                   (coalton:coalton
                    (user-op-nonce (coalton:lisp UserOperation () op)))))
           (init-code (coalton:coalton
                       (types:hex-encode-prefixed
                        (user-op-init-code (coalton:lisp UserOperation () op)))))
           (call-data (coalton:coalton
                       (types:hex-encode-prefixed
                        (user-op-call-data (coalton:lisp UserOperation () op)))))
           (call-gas-limit (web3/types:u256-to-integer
                            (coalton:coalton
                             (user-op-call-gas-limit (coalton:lisp UserOperation () op)))))
           (verification-gas-limit (web3/types:u256-to-integer
                                    (coalton:coalton
                                     (user-op-verification-gas-limit (coalton:lisp UserOperation () op)))))
           (pre-verification-gas (web3/types:u256-to-integer
                                  (coalton:coalton
                                   (user-op-pre-verification-gas (coalton:lisp UserOperation () op)))))
           (max-fee-per-gas (web3/types:u256-to-integer
                             (coalton:coalton
                              (user-op-max-fee-per-gas (coalton:lisp UserOperation () op)))))
           (max-priority-fee (web3/types:u256-to-integer
                              (coalton:coalton
                               (user-op-max-priority-fee-per-gas (coalton:lisp UserOperation () op)))))
           (paymaster-data (coalton:coalton
                            (types:hex-encode-prefixed
                             (user-op-paymaster-and-data (coalton:lisp UserOperation () op)))))
           (signature (coalton:coalton
                       (types:hex-encode-prefixed
                        (user-op-signature (coalton:lisp UserOperation () op))))))
    (cl:format cl:nil
               "{\"sender\":\"~A\",\"nonce\":\"0x~X\",\"initCode\":\"~A\",\"callData\":\"~A\",\"callGasLimit\":\"0x~X\",\"verificationGasLimit\":\"0x~X\",\"preVerificationGas\":\"0x~X\",\"maxFeePerGas\":\"0x~X\",\"maxPriorityFeePerGas\":\"0x~X\",\"paymasterAndData\":\"~A\",\"signature\":\"~A\"}"
               sender nonce init-code call-data
               call-gas-limit verification-gas-limit pre-verification-gas
               max-fee-per-gas max-priority-fee
               paymaster-data signature)))

(cl:defun %packed-user-op-to-json (op)
  "Serialize a v0.7 PackedUserOperation to JSON for bundler RPC.
   Uses the ERC-7769 v0.7 field names: factory/factoryData (split from initCode),
   individual gas limits (unpacked from accountGasLimits/gasFees),
   paymaster/paymasterData (split from paymasterAndData)."
  (cl:let* ((sender (coalton:coalton
                     (web3/address:address-to-hex
                      (packed-op-sender (coalton:lisp PackedUserOperation () op)))))
            (nonce (web3/types:u256-to-integer
                    (coalton:coalton
                     (packed-op-nonce (coalton:lisp PackedUserOperation () op)))))
            (init-code (coalton:coalton
                        (packed-op-init-code (coalton:lisp PackedUserOperation () op))))
            (call-data (coalton:coalton
                        (types:hex-encode-prefixed
                         (packed-op-call-data (coalton:lisp PackedUserOperation () op)))))
            ;; Unpack accountGasLimits -> verificationGasLimit + callGasLimit
            (gas-limits (coalton:coalton
                         (unpack-account-gas-limits
                          (packed-op-account-gas-limits (coalton:lisp PackedUserOperation () op)))))
            (verification-gas-limit
              (web3/types:u256-to-integer
               (cl:slot-value gas-limits 'coalton-library/classes::_0)))
            (call-gas-limit
              (web3/types:u256-to-integer
               (cl:slot-value gas-limits 'coalton-library/classes::_1)))
            (pre-verification-gas (web3/types:u256-to-integer
                                   (coalton:coalton
                                    (packed-op-pre-verification-gas (coalton:lisp PackedUserOperation () op)))))
            ;; Unpack gasFees -> maxPriorityFeePerGas + maxFeePerGas
            (fees (coalton:coalton
                   (unpack-gas-fees
                    (packed-op-gas-fees (coalton:lisp PackedUserOperation () op)))))
            (max-priority-fee
              (web3/types:u256-to-integer
               (cl:slot-value fees 'coalton-library/classes::_0)))
            (max-fee-per-gas
              (web3/types:u256-to-integer
               (cl:slot-value fees 'coalton-library/classes::_1)))
            ;; Split initCode into factory + factoryData (first 20 bytes = factory address)
            (init-code-len (cl:length init-code))
            (paymaster-and-data (coalton:coalton
                                 (packed-op-paymaster-and-data (coalton:lisp PackedUserOperation () op))))
            (pm-len (cl:length paymaster-and-data))
            (signature (coalton:coalton
                        (types:hex-encode-prefixed
                         (packed-op-signature (coalton:lisp PackedUserOperation () op))))))
    ;; Build JSON with v0.7 RPC field names
    (cl:with-output-to-string (s)
      (cl:format s "{\"sender\":\"~A\",\"nonce\":\"0x~X\",\"callData\":~A"
                 sender nonce call-data)
      ;; factory + factoryData (only if initCode is non-empty)
      (cl:if (cl:> init-code-len 0)
             (cl:let ((factory-bytes (coalton:coalton
                                      (types:bytes-take 20 (coalton:lisp types:Bytes () init-code))))
                      (factory-data-bytes (coalton:coalton
                                           (types:bytes-drop 20 (coalton:lisp types:Bytes () init-code)))))
               (cl:format s ",\"factory\":\"~A\",\"factoryData\":\"~A\""
                          (coalton:coalton (types:hex-encode-prefixed (coalton:lisp types:Bytes () factory-bytes)))
                          (coalton:coalton (types:hex-encode-prefixed (coalton:lisp types:Bytes () factory-data-bytes)))))
             (cl:format s ""))
      (cl:format s ",\"callGasLimit\":\"0x~X\",\"verificationGasLimit\":\"0x~X\",\"preVerificationGas\":\"0x~X\",\"maxFeePerGas\":\"0x~X\",\"maxPriorityFeePerGas\":\"0x~X\""
                 call-gas-limit verification-gas-limit pre-verification-gas
                 max-fee-per-gas max-priority-fee)
      ;; paymaster + paymasterVerificationGasLimit + paymasterPostOpGasLimit + paymasterData
      (cl:if (cl:> pm-len 0)
             (cl:let* ((pm-addr-bytes (coalton:coalton
                                       (types:bytes-take 20 (coalton:lisp types:Bytes () paymaster-and-data))))
                       (pm-vgl-bytes (coalton:coalton
                                      (types:bytes-slice 20 16 (coalton:lisp types:Bytes () paymaster-and-data))))
                       (pm-pogl-bytes (coalton:coalton
                                       (types:bytes-slice 36 16 (coalton:lisp types:Bytes () paymaster-and-data))))
                       (pm-data-bytes (coalton:coalton
                                       (types:bytes-drop 52 (coalton:lisp types:Bytes () paymaster-and-data))))
                       (pm-vgl-padded (coalton:coalton
                                       (types:bytes-pad-left 32 (coalton:lisp types:Bytes () pm-vgl-bytes))))
                       (pm-pogl-padded (coalton:coalton
                                        (types:bytes-pad-left 32 (coalton:lisp types:Bytes () pm-pogl-bytes))))
                       (pm-vgl (web3/types:u256-to-integer
                                (web3/types:%unwrap-ok
                                 (coalton:coalton (types:u256-from-bytes (coalton:lisp types:Bytes () pm-vgl-padded))))))
                       (pm-pogl (web3/types:u256-to-integer
                                 (web3/types:%unwrap-ok
                                  (coalton:coalton (types:u256-from-bytes (coalton:lisp types:Bytes () pm-pogl-padded)))))))
               (cl:format s ",\"paymaster\":\"~A\",\"paymasterVerificationGasLimit\":\"0x~X\",\"paymasterPostOpGasLimit\":\"0x~X\",\"paymasterData\":\"~A\""
                          (coalton:coalton (types:hex-encode-prefixed (coalton:lisp types:Bytes () pm-addr-bytes)))
                          pm-vgl pm-pogl
                          (coalton:coalton (types:hex-encode-prefixed (coalton:lisp types:Bytes () pm-data-bytes)))))
             (cl:format s ""))
      (cl:format s ",\"signature\":\"~A\"}" signature))))

(cl:defun %parse-gas-estimate (json-str)
  "Parse a gas estimate JSON response into a GasEstimate"
  (cl:let* ((json (cl-json:decode-json-from-string json-str))
            (pvg-raw (cl:cdr (cl:assoc :pre-verification-gas json)))
            (vgl-raw (cl:or (cl:cdr (cl:assoc :verification-gas-limit json))
                            (cl:cdr (cl:assoc :verification-gas json))))
            (cgl-raw (cl:cdr (cl:assoc :call-gas-limit json)))
            (pm-vgl-raw (cl:cdr (cl:assoc :paymaster-verification-gas-limit json)))
            (pm-pogl-raw (cl:cdr (cl:assoc :paymaster-post-op-gas-limit json))))
    (%GasEstimate
     (web3/types:%parse-hex-u256 (cl:or pvg-raw "0x0"))
     (web3/types:%parse-hex-u256 (cl:or vgl-raw "0x0"))
     (web3/types:%parse-hex-u256 (cl:or cgl-raw "0x0"))
     (cl:if pm-vgl-raw
            (coalton-prelude:Some (web3/types:%parse-hex-u256 pm-vgl-raw))
            (coalton:coalton (the (Optional types:U256) None)))
     (cl:if pm-pogl-raw
            (coalton-prelude:Some (web3/types:%parse-hex-u256 pm-pogl-raw))
            (coalton:coalton (the (Optional types:U256) None))))))

;;; =========================================================================
;;; Bundler JSON-RPC Methods
;;; =========================================================================

(cl:defun %rpc-estimate-gas (provider params)
  "Call eth_estimateUserOperationGas and parse the result into a GasEstimate."
  (cl:let ((result (coalton:coalton
                    (provider:json-rpc-call
                     (coalton:lisp provider:HttpProvider () provider)
                     "eth_estimateUserOperationGas"
                     (coalton:lisp String () params)))))
    (cl:if (web3/types:%result-ok-p result)
           (cl:handler-case
               (coalton-prelude:Ok (%parse-gas-estimate (web3/types:%unwrap-ok result)))
             (cl:error (e)
               (coalton-prelude:Err
                (web3/types:ProviderError
                 (cl:format cl:nil "Failed to parse gas estimate: ~A" e)))))
           result)))

(coalton-toplevel

  ;;; v0.6 Bundler RPC

  (declare eth-send-user-operation
    (provider:HttpProvider -> UserOperation -> addr:Address -> (types:Web3Result String)))
  (define (eth-send-user-operation provider op entrypoint)
    "Submit a v0.6 UserOperation to the bundler mempool. Returns the userOpHash on success."
    (let ((ep-hex (addr:address-to-hex entrypoint)))
      (let ((params (lisp String (op ep-hex)
                      (cl:format cl:nil "[~A,\"~A\"]"
                                 (%user-op-to-json op) ep-hex))))
        (provider:json-rpc-call provider "eth_sendUserOperation" params))))

  (declare eth-estimate-user-operation-gas
    (provider:HttpProvider -> UserOperation -> addr:Address -> (types:Web3Result GasEstimate)))
  (define (eth-estimate-user-operation-gas provider op entrypoint)
    "Estimate gas limits for a v0.6 UserOperation. The signature field can be a dummy value.
     Returns gas estimates from the bundler."
    (let ((ep-hex (addr:address-to-hex entrypoint)))
      (let ((params (lisp String (op ep-hex)
                      (cl:format cl:nil "[~A,\"~A\"]"
                                 (%user-op-to-json op) ep-hex))))
        (lisp (types:Web3Result GasEstimate) (provider params)
          (%rpc-estimate-gas provider params)))))

  ;;; v0.7 Bundler RPC

  (declare eth-send-packed-user-operation
    (provider:HttpProvider -> PackedUserOperation -> addr:Address -> (types:Web3Result String)))
  (define (eth-send-packed-user-operation provider op entrypoint)
    "Submit a v0.7 PackedUserOperation to the bundler mempool. Returns the userOpHash.
     Serializes using ERC-7769 v0.7 field names (factory/factoryData, individual gas limits,
     paymaster/paymasterData)."
    (let ((ep-hex (addr:address-to-hex entrypoint)))
      (let ((params (lisp String (op ep-hex)
                      (cl:format cl:nil "[~A,\"~A\"]"
                                 (%packed-user-op-to-json op) ep-hex))))
        (provider:json-rpc-call provider "eth_sendUserOperation" params))))

  (declare eth-estimate-packed-user-operation-gas
    (provider:HttpProvider -> PackedUserOperation -> addr:Address -> (types:Web3Result GasEstimate)))
  (define (eth-estimate-packed-user-operation-gas provider op entrypoint)
    "Estimate gas limits for a v0.7 PackedUserOperation.
     Returns gas estimates including paymasterVerificationGasLimit and paymasterPostOpGasLimit."
    (let ((ep-hex (addr:address-to-hex entrypoint)))
      (let ((params (lisp String (op ep-hex)
                      (cl:format cl:nil "[~A,\"~A\"]"
                                 (%packed-user-op-to-json op) ep-hex))))
        (lisp (types:Web3Result GasEstimate) (provider params)
          (%rpc-estimate-gas provider params)))))

  ;;; Shared Bundler RPC (version-agnostic)

  (declare eth-get-user-operation-by-hash
    (provider:HttpProvider -> String -> (types:Web3Result (Optional String))))
  (define (eth-get-user-operation-by-hash provider user-op-hash-hex)
    "Get a UserOperation by its hash. Returns None if not found.
     Result is the raw JSON string containing userOp, entryPoint, blockNumber, transactionHash."
    (let ((params (lisp String (user-op-hash-hex)
                    (cl:format cl:nil "[\"~A\"]" user-op-hash-hex))))
      (provider:json-rpc-call-nullable provider "eth_getUserOperationByHash" params)))

  (declare eth-get-user-operation-receipt
    (provider:HttpProvider -> String -> (types:Web3Result (Optional String))))
  (define (eth-get-user-operation-receipt provider user-op-hash-hex)
    "Get the receipt for a UserOperation by its hash. Returns None if not yet included.
     Result is raw JSON containing userOpHash, sender, nonce, actualGasCost,
     actualGasUsed, success, logs, and receipt."
    (let ((params (lisp String (user-op-hash-hex)
                    (cl:format cl:nil "[\"~A\"]" user-op-hash-hex))))
      (provider:json-rpc-call-nullable provider "eth_getUserOperationReceipt" params)))

  (declare eth-supported-entry-points
    (provider:HttpProvider -> (types:Web3Result String)))
  (define (eth-supported-entry-points provider)
    "Query the bundler for supported EntryPoint addresses.
     Returns raw JSON array of address strings."
    (provider:json-rpc-call provider "eth_supportedEntryPoints" "[]")))
