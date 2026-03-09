(in-package #:web3/erc4337)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; EntryPoint Contract Addresses
  ;;; =========================================================================

  (declare %unwrap-address (String -> addr:Address))
  (define (%unwrap-address hex)
    "Unwrap a known-valid hex address. Only for compile-time constants."
    (match (addr:address-from-hex hex)
      ((Ok a) a)
      ((Err _) (error "Invalid hardcoded address"))))

  (declare %unwrap-u256 ((types:Web3Result types:U256) -> types:U256))
  (define (%unwrap-u256 r)
    "Unwrap a known-valid U256 result."
    (match r
      ((Ok v) v)
      ((Err _) (error "Invalid U256 bytes"))))

  (declare entrypoint-v06 addr:Address)
  (define entrypoint-v06
    "EntryPoint v0.6 address: 0x5FF137D4b0FDCD49DcA30c7CF57E578a026d2789"
    (%unwrap-address "0x5FF137D4b0FDCD49DcA30c7CF57E578a026d2789"))

  (declare entrypoint-v07 addr:Address)
  (define entrypoint-v07
    "EntryPoint v0.7 address: 0x0000000071727De22E5E9d8BAf0edAc6f37da032"
    (%unwrap-address "0x0000000071727De22E5E9d8BAf0edAc6f37da032"))

  ;;; =========================================================================
  ;;; v0.6 UserOperation
  ;;; =========================================================================

  (define-type UserOperation
    "ERC-4337 v0.6 UserOperation"
    (%UserOperation
     addr:Address     ; sender
     types:U256       ; nonce
     types:Bytes      ; initCode
     types:Bytes      ; callData
     types:U256       ; callGasLimit
     types:U256       ; verificationGasLimit
     types:U256       ; preVerificationGas
     types:U256       ; maxFeePerGas
     types:U256       ; maxPriorityFeePerGas
     types:Bytes      ; paymasterAndData
     types:Bytes))    ; signature

  (declare make-user-operation
    (addr:Address -> types:U256 -> types:Bytes -> types:Bytes ->
     types:U256 -> types:U256 -> types:U256 ->
     types:U256 -> types:U256 ->
     types:Bytes -> types:Bytes -> UserOperation))
  (define (make-user-operation sender nonce init-code call-data
                               call-gas-limit verification-gas-limit pre-verification-gas
                               max-fee-per-gas max-priority-fee-per-gas
                               paymaster-and-data signature)
    "Create a v0.6 UserOperation"
    (%UserOperation sender nonce init-code call-data
                    call-gas-limit verification-gas-limit pre-verification-gas
                    max-fee-per-gas max-priority-fee-per-gas
                    paymaster-and-data signature))

  (declare user-op-sender (UserOperation -> addr:Address))
  (define (user-op-sender op)
    (match op ((%UserOperation s _ _ _ _ _ _ _ _ _ _) s)))

  (declare user-op-nonce (UserOperation -> types:U256))
  (define (user-op-nonce op)
    (match op ((%UserOperation _ n _ _ _ _ _ _ _ _ _) n)))

  (declare user-op-init-code (UserOperation -> types:Bytes))
  (define (user-op-init-code op)
    (match op ((%UserOperation _ _ ic _ _ _ _ _ _ _ _) ic)))

  (declare user-op-call-data (UserOperation -> types:Bytes))
  (define (user-op-call-data op)
    (match op ((%UserOperation _ _ _ cd _ _ _ _ _ _ _) cd)))

  (declare user-op-call-gas-limit (UserOperation -> types:U256))
  (define (user-op-call-gas-limit op)
    (match op ((%UserOperation _ _ _ _ cgl _ _ _ _ _ _) cgl)))

  (declare user-op-verification-gas-limit (UserOperation -> types:U256))
  (define (user-op-verification-gas-limit op)
    (match op ((%UserOperation _ _ _ _ _ vgl _ _ _ _ _) vgl)))

  (declare user-op-pre-verification-gas (UserOperation -> types:U256))
  (define (user-op-pre-verification-gas op)
    (match op ((%UserOperation _ _ _ _ _ _ pvg _ _ _ _) pvg)))

  (declare user-op-max-fee-per-gas (UserOperation -> types:U256))
  (define (user-op-max-fee-per-gas op)
    (match op ((%UserOperation _ _ _ _ _ _ _ mfpg _ _ _) mfpg)))

  (declare user-op-max-priority-fee-per-gas (UserOperation -> types:U256))
  (define (user-op-max-priority-fee-per-gas op)
    (match op ((%UserOperation _ _ _ _ _ _ _ _ mpfpg _ _) mpfpg)))

  (declare user-op-paymaster-and-data (UserOperation -> types:Bytes))
  (define (user-op-paymaster-and-data op)
    (match op ((%UserOperation _ _ _ _ _ _ _ _ _ pd _) pd)))

  (declare user-op-signature (UserOperation -> types:Bytes))
  (define (user-op-signature op)
    (match op ((%UserOperation _ _ _ _ _ _ _ _ _ _ sig) sig)))

  ;;; =========================================================================
  ;;; v0.7 PackedUserOperation
  ;;; =========================================================================

  (define-type PackedUserOperation
    "ERC-4337 v0.7 PackedUserOperation with packed gas fields"
    (%PackedUserOperation
     addr:Address     ; sender
     types:U256       ; nonce
     types:Bytes      ; initCode
     types:Bytes      ; callData
     types:Bytes      ; accountGasLimits (bytes32: verificationGasLimit | callGasLimit)
     types:U256       ; preVerificationGas
     types:Bytes      ; gasFees (bytes32: maxPriorityFeePerGas | maxFeePerGas)
     types:Bytes      ; paymasterAndData
     types:Bytes))    ; signature

  (declare make-packed-user-operation
    (addr:Address -> types:U256 -> types:Bytes -> types:Bytes ->
     types:Bytes -> types:U256 -> types:Bytes ->
     types:Bytes -> types:Bytes -> PackedUserOperation))
  (define (make-packed-user-operation sender nonce init-code call-data
                                      account-gas-limits pre-verification-gas gas-fees
                                      paymaster-and-data signature)
    "Create a v0.7 PackedUserOperation"
    (%PackedUserOperation sender nonce init-code call-data
                          account-gas-limits pre-verification-gas gas-fees
                          paymaster-and-data signature))

  (declare packed-op-sender (PackedUserOperation -> addr:Address))
  (define (packed-op-sender op)
    (match op ((%PackedUserOperation s _ _ _ _ _ _ _ _) s)))

  (declare packed-op-nonce (PackedUserOperation -> types:U256))
  (define (packed-op-nonce op)
    (match op ((%PackedUserOperation _ n _ _ _ _ _ _ _) n)))

  (declare packed-op-init-code (PackedUserOperation -> types:Bytes))
  (define (packed-op-init-code op)
    (match op ((%PackedUserOperation _ _ ic _ _ _ _ _ _) ic)))

  (declare packed-op-call-data (PackedUserOperation -> types:Bytes))
  (define (packed-op-call-data op)
    (match op ((%PackedUserOperation _ _ _ cd _ _ _ _ _) cd)))

  (declare packed-op-account-gas-limits (PackedUserOperation -> types:Bytes))
  (define (packed-op-account-gas-limits op)
    (match op ((%PackedUserOperation _ _ _ _ agl _ _ _ _) agl)))

  (declare packed-op-pre-verification-gas (PackedUserOperation -> types:U256))
  (define (packed-op-pre-verification-gas op)
    (match op ((%PackedUserOperation _ _ _ _ _ pvg _ _ _) pvg)))

  (declare packed-op-gas-fees (PackedUserOperation -> types:Bytes))
  (define (packed-op-gas-fees op)
    (match op ((%PackedUserOperation _ _ _ _ _ _ gf _ _) gf)))

  (declare packed-op-paymaster-and-data (PackedUserOperation -> types:Bytes))
  (define (packed-op-paymaster-and-data op)
    (match op ((%PackedUserOperation _ _ _ _ _ _ _ pd _) pd)))

  (declare packed-op-signature (PackedUserOperation -> types:Bytes))
  (define (packed-op-signature op)
    (match op ((%PackedUserOperation _ _ _ _ _ _ _ _ sig) sig)))

  ;;; =========================================================================
  ;;; v0.7 Packing Helpers
  ;;; =========================================================================

  (declare pack-account-gas-limits (types:U256 -> types:U256 -> types:Bytes))
  (define (pack-account-gas-limits verification-gas-limit call-gas-limit)
    "Pack verificationGasLimit (high 128 bits) and callGasLimit (low 128 bits) into bytes32"
    (types:bytes-concat-many
     (Cons (types:bytes-drop 16 (types:u256-to-bytes verification-gas-limit))
           (Cons (types:bytes-drop 16 (types:u256-to-bytes call-gas-limit))
                 Nil))))

  (declare unpack-account-gas-limits (types:Bytes -> (Tuple types:U256 types:U256)))
  (define (unpack-account-gas-limits packed)
    "Unpack bytes32 into (verificationGasLimit, callGasLimit)"
    (let ((vgl-padded (types:bytes-pad-left 32 (types:bytes-take 16 packed)))
          (cgl-padded (types:bytes-pad-left 32 (types:bytes-slice 16 16 packed))))
      (Tuple (%unwrap-u256 (types:u256-from-bytes vgl-padded))
             (%unwrap-u256 (types:u256-from-bytes cgl-padded)))))

  (declare pack-gas-fees (types:U256 -> types:U256 -> types:Bytes))
  (define (pack-gas-fees max-priority-fee-per-gas max-fee-per-gas)
    "Pack maxPriorityFeePerGas (high 128 bits) and maxFeePerGas (low 128 bits) into bytes32"
    (types:bytes-concat-many
     (Cons (types:bytes-drop 16 (types:u256-to-bytes max-priority-fee-per-gas))
           (Cons (types:bytes-drop 16 (types:u256-to-bytes max-fee-per-gas))
                 Nil))))

  (declare unpack-gas-fees (types:Bytes -> (Tuple types:U256 types:U256)))
  (define (unpack-gas-fees packed)
    "Unpack bytes32 into (maxPriorityFeePerGas, maxFeePerGas)"
    (let ((mpf-padded (types:bytes-pad-left 32 (types:bytes-take 16 packed)))
          (mf-padded (types:bytes-pad-left 32 (types:bytes-slice 16 16 packed))))
      (Tuple (%unwrap-u256 (types:u256-from-bytes mpf-padded))
             (%unwrap-u256 (types:u256-from-bytes mf-padded)))))

  (declare pack-paymaster-data
    (addr:Address -> types:U256 -> types:U256 -> types:Bytes -> types:Bytes))
  (define (pack-paymaster-data paymaster verification-gas-limit post-op-gas-limit data)
    "Pack paymaster address + verification gas (16 bytes) + postOp gas (16 bytes) + data"
    (let ((pm-bytes (addr:address-bytes paymaster))
          (vgl-bytes (types:u256-to-bytes verification-gas-limit))
          (pogl-bytes (types:u256-to-bytes post-op-gas-limit)))
      (types:bytes-concat-many
       (Cons pm-bytes
             (Cons (types:bytes-drop 16 vgl-bytes)
                   (Cons (types:bytes-drop 16 pogl-bytes)
                         (Cons data Nil)))))))

  (declare unpack-paymaster-data
    (types:Bytes -> (Tuple addr:Address (Tuple types:U256 (Tuple types:U256 types:Bytes)))))
  (define (unpack-paymaster-data packed)
    "Unpack paymasterAndData into (paymaster, verificationGasLimit, postOpGasLimit, data)"
    (let ((pm-bytes (types:bytes-take 20 packed))
          (vgl-part (types:bytes-slice 20 16 packed))
          (pogl-part (types:bytes-slice 36 16 packed))
          (data (types:bytes-drop 52 packed)))
      ;; Pad the 16-byte gas values to 32 bytes for U256 conversion
      (let ((vgl-padded (types:bytes-pad-left 32 vgl-part))
            (pogl-padded (types:bytes-pad-left 32 pogl-part)))
        (Tuple (match (addr:address-from-bytes pm-bytes)
                ((Ok a) a)
                ((Err _) (error "Invalid paymaster address bytes")))
               (Tuple (%unwrap-u256 (types:u256-from-bytes vgl-padded))
                      (Tuple (%unwrap-u256 (types:u256-from-bytes pogl-padded))
                             data))))))

  ;;; =========================================================================
  ;;; Conversion Between v0.6 and v0.7
  ;;; =========================================================================

  (declare user-op-to-packed (UserOperation -> PackedUserOperation))
  (define (user-op-to-packed op)
    "Convert v0.6 UserOperation to v0.7 PackedUserOperation"
    (make-packed-user-operation
     (user-op-sender op)
     (user-op-nonce op)
     (user-op-init-code op)
     (user-op-call-data op)
     (pack-account-gas-limits (user-op-verification-gas-limit op)
                              (user-op-call-gas-limit op))
     (user-op-pre-verification-gas op)
     (pack-gas-fees (user-op-max-priority-fee-per-gas op)
                    (user-op-max-fee-per-gas op))
     (user-op-paymaster-and-data op)
     (user-op-signature op)))

  (declare packed-to-user-op (PackedUserOperation -> UserOperation))
  (define (packed-to-user-op op)
    "Convert v0.7 PackedUserOperation to v0.6 UserOperation"
    (let ((gas-limits (unpack-account-gas-limits (packed-op-account-gas-limits op)))
          (fees (unpack-gas-fees (packed-op-gas-fees op))))
      (match gas-limits
        ((Tuple vgl cgl)
         (match fees
           ((Tuple mpfpg mfpg)
            (make-user-operation
             (packed-op-sender op)
             (packed-op-nonce op)
             (packed-op-init-code op)
             (packed-op-call-data op)
             cgl vgl
             (packed-op-pre-verification-gas op)
             mfpg mpfpg
             (packed-op-paymaster-and-data op)
             (packed-op-signature op))))))))

  ;;; =========================================================================
  ;;; Gas Estimate Result
  ;;; =========================================================================

  (define-type GasEstimate
    "Gas estimation result from bundler"
    (%GasEstimate
     types:U256              ; preVerificationGas
     types:U256              ; verificationGasLimit
     types:U256              ; callGasLimit
     (Optional types:U256)   ; paymasterVerificationGasLimit (v0.7 only)
     (Optional types:U256))) ; paymasterPostOpGasLimit (v0.7 only)

  (declare gas-estimate-pre-verification-gas (GasEstimate -> types:U256))
  (define (gas-estimate-pre-verification-gas est)
    (match est ((%GasEstimate pvg _ _ _ _) pvg)))

  (declare gas-estimate-verification-gas-limit (GasEstimate -> types:U256))
  (define (gas-estimate-verification-gas-limit est)
    (match est ((%GasEstimate _ vgl _ _ _) vgl)))

  (declare gas-estimate-call-gas-limit (GasEstimate -> types:U256))
  (define (gas-estimate-call-gas-limit est)
    (match est ((%GasEstimate _ _ cgl _ _) cgl)))

  (declare gas-estimate-paymaster-verification-gas-limit (GasEstimate -> (Optional types:U256)))
  (define (gas-estimate-paymaster-verification-gas-limit est)
    (match est ((%GasEstimate _ _ _ pvgl _) pvgl)))

  (declare gas-estimate-paymaster-post-op-gas-limit (GasEstimate -> (Optional types:U256)))
  (define (gas-estimate-paymaster-post-op-gas-limit est)
    (match est ((%GasEstimate _ _ _ _ pogl) pogl))))
