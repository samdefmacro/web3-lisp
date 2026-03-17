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

  (define-struct UserOperation
    "ERC-4337 v0.6 UserOperation"
    (user-op-sender addr:Address)
    (user-op-nonce types:U256)
    (user-op-init-code types:Bytes)
    (user-op-call-data types:Bytes)
    (user-op-call-gas-limit types:U256)
    (user-op-verification-gas-limit types:U256)
    (user-op-pre-verification-gas types:U256)
    (user-op-max-fee-per-gas types:U256)
    (user-op-max-priority-fee-per-gas types:U256)
    (user-op-paymaster-and-data types:Bytes)
    (user-op-signature types:Bytes))

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
    (UserOperation sender nonce init-code call-data
                   call-gas-limit verification-gas-limit pre-verification-gas
                   max-fee-per-gas max-priority-fee-per-gas
                   paymaster-and-data signature))

  ;;; =========================================================================
  ;;; v0.7 PackedUserOperation
  ;;; =========================================================================

  (define-struct PackedUserOperation
    "ERC-4337 v0.7 PackedUserOperation with packed gas fields"
    (packed-op-sender addr:Address)
    (packed-op-nonce types:U256)
    (packed-op-init-code types:Bytes)
    (packed-op-call-data types:Bytes)
    (packed-op-account-gas-limits types:Bytes)   ; bytes32: verificationGasLimit | callGasLimit
    (packed-op-pre-verification-gas types:U256)
    (packed-op-gas-fees types:Bytes)             ; bytes32: maxPriorityFeePerGas | maxFeePerGas
    (packed-op-paymaster-and-data types:Bytes)
    (packed-op-signature types:Bytes))

  (declare make-packed-user-operation
    (addr:Address -> types:U256 -> types:Bytes -> types:Bytes ->
     types:Bytes -> types:U256 -> types:Bytes ->
     types:Bytes -> types:Bytes -> PackedUserOperation))
  (define (make-packed-user-operation sender nonce init-code call-data
                                      account-gas-limits pre-verification-gas gas-fees
                                      paymaster-and-data signature)
    "Create a v0.7 PackedUserOperation"
    (PackedUserOperation sender nonce init-code call-data
                         account-gas-limits pre-verification-gas gas-fees
                         paymaster-and-data signature))

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
     (.user-op-sender op)
     (.user-op-nonce op)
     (.user-op-init-code op)
     (.user-op-call-data op)
     (pack-account-gas-limits (.user-op-verification-gas-limit op)
                              (.user-op-call-gas-limit op))
     (.user-op-pre-verification-gas op)
     (pack-gas-fees (.user-op-max-priority-fee-per-gas op)
                    (.user-op-max-fee-per-gas op))
     (.user-op-paymaster-and-data op)
     (.user-op-signature op)))

  (declare packed-to-user-op (PackedUserOperation -> UserOperation))
  (define (packed-to-user-op op)
    "Convert v0.7 PackedUserOperation to v0.6 UserOperation"
    (let ((gas-limits (unpack-account-gas-limits (.packed-op-account-gas-limits op)))
          (fees (unpack-gas-fees (.packed-op-gas-fees op))))
      (match gas-limits
        ((Tuple vgl cgl)
         (match fees
           ((Tuple mpfpg mfpg)
            (make-user-operation
             (.packed-op-sender op)
             (.packed-op-nonce op)
             (.packed-op-init-code op)
             (.packed-op-call-data op)
             cgl vgl
             (.packed-op-pre-verification-gas op)
             mfpg mpfpg
             (.packed-op-paymaster-and-data op)
             (.packed-op-signature op))))))))

  ;;; =========================================================================
  ;;; Gas Estimate Result
  ;;; =========================================================================

  (define-struct GasEstimate
    "Gas estimation result from bundler"
    (gas-estimate-pre-verification-gas types:U256)
    (gas-estimate-verification-gas-limit types:U256)
    (gas-estimate-call-gas-limit types:U256)
    (gas-estimate-paymaster-verification-gas-limit (Optional types:U256))   ; v0.7 only
    (gas-estimate-paymaster-post-op-gas-limit (Optional types:U256)))       ; v0.7 only
)
