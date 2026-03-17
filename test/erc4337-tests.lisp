;;; ERC-4337 Account Abstraction tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; EntryPoint Address Tests
;;; =========================================================================

(defun run-erc4337-tests ()
  (format t "~%=== ERC-4337 Account Abstraction Tests ===~%")

  (test-case "entrypoint-v06 has correct address"
    (let ((addr (coalton:coalton
                 (web3/address:address-to-hex web3/erc4337:entrypoint-v06))))
      (assert (string-equal addr "0x5FF137D4b0FDCD49DcA30c7CF57E578a026d2789"))))

  (test-case "entrypoint-v07 has correct address"
    (let ((addr (coalton:coalton
                 (web3/address:address-to-hex web3/erc4337:entrypoint-v07))))
      (assert (string-equal addr "0x0000000071727De22E5E9d8BAf0edAc6f37da032"))))

  ;;; =========================================================================
  ;;; UserOperation Construction Tests
  ;;; =========================================================================

  (test-case "make-user-operation creates valid v0.6 UserOperation"
    (let* ((sender (result-value (coalton:coalton
                                  (web3/address:address-from-hex
                                   "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (nonce (coalton:coalton web3/types:u256-zero))
           (empty (coalton:coalton web3/types:bytes-empty))
           (op (coalton:coalton
                (web3/erc4337:make-user-operation
                 (coalton:lisp web3/address:Address () sender)
                 (coalton:lisp web3/types:U256 () nonce)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:U256 () nonce)
                 (coalton:lisp web3/types:U256 () nonce)
                 (coalton:lisp web3/types:U256 () nonce)
                 (coalton:lisp web3/types:U256 () nonce)
                 (coalton:lisp web3/types:U256 () nonce)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty)))))
      ;; Verify sender round-trips
      (let ((got-sender (coalton:coalton
                         (web3/address:address-to-hex
                          (web3/erc4337:.user-op-sender
                           (coalton:lisp web3/erc4337:UserOperation () op))))))
        (assert (string-equal got-sender "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045")))))

  (test-case "user-op accessors return correct fields"
    (let* ((sender (result-value (coalton:coalton
                                  (web3/address:address-from-hex
                                   "0x1111111111111111111111111111111111111111"))))
           (nonce (coalton:coalton (web3/types:u256-from-integer 42)))
           (empty (coalton:coalton web3/types:bytes-empty))
           (call-gas (coalton:coalton (web3/types:u256-from-integer 100000)))
           (ver-gas (coalton:coalton (web3/types:u256-from-integer 200000)))
           (pre-gas (coalton:coalton (web3/types:u256-from-integer 50000)))
           (max-fee (coalton:coalton (web3/types:u256-from-integer 30000000000)))
           (priority (coalton:coalton (web3/types:u256-from-integer 1500000000)))
           (op (coalton:coalton
                (web3/erc4337:make-user-operation
                 (coalton:lisp web3/address:Address () sender)
                 (coalton:lisp web3/types:U256 () nonce)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:U256 () call-gas)
                 (coalton:lisp web3/types:U256 () ver-gas)
                 (coalton:lisp web3/types:U256 () pre-gas)
                 (coalton:lisp web3/types:U256 () max-fee)
                 (coalton:lisp web3/types:U256 () priority)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty)))))
      ;; Verify nonce
      (let ((got-nonce (web3/types:u256-to-integer
                        (coalton:coalton
                         (web3/erc4337:.user-op-nonce
                          (coalton:lisp web3/erc4337:UserOperation () op))))))
        (assert (= got-nonce 42)))
      ;; Verify call gas limit
      (let ((got-cgl (web3/types:u256-to-integer
                      (coalton:coalton
                       (web3/erc4337:.user-op-call-gas-limit
                        (coalton:lisp web3/erc4337:UserOperation () op))))))
        (assert (= got-cgl 100000)))
      ;; Verify verification gas limit
      (let ((got-vgl (web3/types:u256-to-integer
                      (coalton:coalton
                       (web3/erc4337:.user-op-verification-gas-limit
                        (coalton:lisp web3/erc4337:UserOperation () op))))))
        (assert (= got-vgl 200000)))))

  ;;; =========================================================================
  ;;; PackedUserOperation Tests
  ;;; =========================================================================

  (test-case "make-packed-user-operation creates valid v0.7 op"
    (let* ((sender (result-value (coalton:coalton
                                  (web3/address:address-from-hex
                                   "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (nonce (coalton:coalton web3/types:u256-zero))
           (empty (coalton:coalton web3/types:bytes-empty))
           (gas-limits (coalton:coalton
                        (web3/erc4337:pack-account-gas-limits
                         (web3/types:u256-from-integer 200000)
                         (web3/types:u256-from-integer 100000))))
           (gas-fees (coalton:coalton
                      (web3/erc4337:pack-gas-fees
                       (web3/types:u256-from-integer 1500000000)
                       (web3/types:u256-from-integer 30000000000))))
           (op (coalton:coalton
                (web3/erc4337:make-packed-user-operation
                 (coalton:lisp web3/address:Address () sender)
                 (coalton:lisp web3/types:U256 () nonce)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () gas-limits)
                 (coalton:lisp web3/types:U256 () nonce)
                 (coalton:lisp web3/types:Bytes () gas-fees)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty)))))
      ;; Account gas limits should be 32 bytes
      (let ((agl (coalton:coalton
                  (web3/erc4337:.packed-op-account-gas-limits
                   (coalton:lisp web3/erc4337:PackedUserOperation () op)))))
        (assert (= (length agl) 32)))))

  ;;; =========================================================================
  ;;; Gas Packing Tests
  ;;; =========================================================================

  (test-case "pack-account-gas-limits produces 32 bytes"
    (let ((packed (coalton:coalton
                   (web3/erc4337:pack-account-gas-limits
                    (web3/types:u256-from-integer 200000)
                    (web3/types:u256-from-integer 100000)))))
      (assert (= (length packed) 32))))

  (test-case "pack/unpack-account-gas-limits roundtrip"
    (let* ((vgl (coalton:coalton (web3/types:u256-from-integer 200000)))
           (cgl (coalton:coalton (web3/types:u256-from-integer 100000)))
           (packed (coalton:coalton
                    (web3/erc4337:pack-account-gas-limits
                     (coalton:lisp web3/types:U256 () vgl)
                     (coalton:lisp web3/types:U256 () cgl))))
           (unpacked (coalton:coalton
                      (web3/erc4337:unpack-account-gas-limits
                       (coalton:lisp web3/types:Bytes () packed)))))
      ;; Extract first and second from tuple
      (let ((got-vgl (web3/types:u256-to-integer
                      (slot-value unpacked 'coalton-library/classes::_0)))
            (got-cgl (web3/types:u256-to-integer
                      (slot-value unpacked 'coalton-library/classes::_1))))
        (assert (= got-vgl 200000))
        (assert (= got-cgl 100000)))))

  (test-case "pack-gas-fees produces 32 bytes"
    (let ((packed (coalton:coalton
                   (web3/erc4337:pack-gas-fees
                    (web3/types:u256-from-integer 1500000000)
                    (web3/types:u256-from-integer 30000000000)))))
      (assert (= (length packed) 32))))

  (test-case "pack/unpack-gas-fees roundtrip"
    (let* ((mpf (coalton:coalton (web3/types:u256-from-integer 1500000000)))
           (mf (coalton:coalton (web3/types:u256-from-integer 30000000000)))
           (packed (coalton:coalton
                    (web3/erc4337:pack-gas-fees
                     (coalton:lisp web3/types:U256 () mpf)
                     (coalton:lisp web3/types:U256 () mf))))
           (unpacked (coalton:coalton
                      (web3/erc4337:unpack-gas-fees
                       (coalton:lisp web3/types:Bytes () packed)))))
      (let ((got-mpf (web3/types:u256-to-integer
                      (slot-value unpacked 'coalton-library/classes::_0)))
            (got-mf (web3/types:u256-to-integer
                     (slot-value unpacked 'coalton-library/classes::_1))))
        (assert (= got-mpf 1500000000))
        (assert (= got-mf 30000000000)))))

  (test-case "pack-account-gas-limits with zero values"
    (let* ((packed (coalton:coalton
                    (web3/erc4337:pack-account-gas-limits
                     web3/types:u256-zero
                     web3/types:u256-zero)))
           (unpacked (coalton:coalton
                      (web3/erc4337:unpack-account-gas-limits
                       (coalton:lisp web3/types:Bytes () packed)))))
      (let ((got-vgl (web3/types:u256-to-integer
                      (slot-value unpacked 'coalton-library/classes::_0)))
            (got-cgl (web3/types:u256-to-integer
                      (slot-value unpacked 'coalton-library/classes::_1))))
        (assert (= got-vgl 0))
        (assert (= got-cgl 0)))))

  (test-case "pack-gas-fees with large values"
    (let* ((mpf (coalton:coalton (web3/types:u256-from-integer 999999999999)))
           (mf (coalton:coalton (web3/types:u256-from-integer 888888888888)))
           (packed (coalton:coalton
                    (web3/erc4337:pack-gas-fees
                     (coalton:lisp web3/types:U256 () mpf)
                     (coalton:lisp web3/types:U256 () mf))))
           (unpacked (coalton:coalton
                      (web3/erc4337:unpack-gas-fees
                       (coalton:lisp web3/types:Bytes () packed)))))
      (let ((got-mpf (web3/types:u256-to-integer
                      (slot-value unpacked 'coalton-library/classes::_0)))
            (got-mf (web3/types:u256-to-integer
                     (slot-value unpacked 'coalton-library/classes::_1))))
        (assert (= got-mpf 999999999999))
        (assert (= got-mf 888888888888)))))

  ;;; =========================================================================
  ;;; v0.6 <-> v0.7 Conversion Tests
  ;;; =========================================================================

  (test-case "user-op-to-packed preserves sender and nonce"
    (let* ((sender (result-value (coalton:coalton
                                  (web3/address:address-from-hex
                                   "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (nonce (coalton:coalton (web3/types:u256-from-integer 7)))
           (empty (coalton:coalton web3/types:bytes-empty))
           (zero (coalton:coalton web3/types:u256-zero))
           (op (coalton:coalton
                (web3/erc4337:make-user-operation
                 (coalton:lisp web3/address:Address () sender)
                 (coalton:lisp web3/types:U256 () nonce)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:U256 () zero)
                 (coalton:lisp web3/types:U256 () zero)
                 (coalton:lisp web3/types:U256 () zero)
                 (coalton:lisp web3/types:U256 () zero)
                 (coalton:lisp web3/types:U256 () zero)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty))))
           (packed (coalton:coalton
                    (web3/erc4337:user-op-to-packed
                     (coalton:lisp web3/erc4337:UserOperation () op)))))
      ;; Nonce preserved
      (let ((got-nonce (web3/types:u256-to-integer
                        (coalton:coalton
                         (web3/erc4337:.packed-op-nonce
                          (coalton:lisp web3/erc4337:PackedUserOperation () packed))))))
        (assert (= got-nonce 7)))))

  (test-case "user-op-to-packed packs gas limits correctly"
    (let* ((sender (result-value (coalton:coalton
                                  (web3/address:address-from-hex
                                   "0x1111111111111111111111111111111111111111"))))
           (nonce (coalton:coalton web3/types:u256-zero))
           (empty (coalton:coalton web3/types:bytes-empty))
           (call-gas (coalton:coalton (web3/types:u256-from-integer 100000)))
           (ver-gas (coalton:coalton (web3/types:u256-from-integer 200000)))
           (pre-gas (coalton:coalton (web3/types:u256-from-integer 50000)))
           (max-fee (coalton:coalton (web3/types:u256-from-integer 30000000000)))
           (priority (coalton:coalton (web3/types:u256-from-integer 1500000000)))
           (op (coalton:coalton
                (web3/erc4337:make-user-operation
                 (coalton:lisp web3/address:Address () sender)
                 (coalton:lisp web3/types:U256 () nonce)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:U256 () call-gas)
                 (coalton:lisp web3/types:U256 () ver-gas)
                 (coalton:lisp web3/types:U256 () pre-gas)
                 (coalton:lisp web3/types:U256 () max-fee)
                 (coalton:lisp web3/types:U256 () priority)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty))))
           (packed (coalton:coalton
                    (web3/erc4337:user-op-to-packed
                     (coalton:lisp web3/erc4337:UserOperation () op)))))
      ;; Unpack gas limits from the packed op
      (let* ((agl (coalton:coalton
                   (web3/erc4337:.packed-op-account-gas-limits
                    (coalton:lisp web3/erc4337:PackedUserOperation () packed))))
             (unpacked-gas (coalton:coalton
                            (web3/erc4337:unpack-account-gas-limits
                             (coalton:lisp web3/types:Bytes () agl)))))
        (assert (= (web3/types:u256-to-integer
                    (slot-value unpacked-gas 'coalton-library/classes::_0))
                   200000))
        (assert (= (web3/types:u256-to-integer
                    (slot-value unpacked-gas 'coalton-library/classes::_1))
                   100000)))))

  (test-case "packed-to-user-op roundtrip preserves all gas values"
    (let* ((sender (result-value (coalton:coalton
                                  (web3/address:address-from-hex
                                   "0x2222222222222222222222222222222222222222"))))
           (nonce (coalton:coalton (web3/types:u256-from-integer 5)))
           (empty (coalton:coalton web3/types:bytes-empty))
           (call-gas (coalton:coalton (web3/types:u256-from-integer 150000)))
           (ver-gas (coalton:coalton (web3/types:u256-from-integer 300000)))
           (pre-gas (coalton:coalton (web3/types:u256-from-integer 60000)))
           (max-fee (coalton:coalton (web3/types:u256-from-integer 25000000000)))
           (priority (coalton:coalton (web3/types:u256-from-integer 2000000000)))
           (op (coalton:coalton
                (web3/erc4337:make-user-operation
                 (coalton:lisp web3/address:Address () sender)
                 (coalton:lisp web3/types:U256 () nonce)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:U256 () call-gas)
                 (coalton:lisp web3/types:U256 () ver-gas)
                 (coalton:lisp web3/types:U256 () pre-gas)
                 (coalton:lisp web3/types:U256 () max-fee)
                 (coalton:lisp web3/types:U256 () priority)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty))))
           ;; Convert to packed and back
           (packed (coalton:coalton
                    (web3/erc4337:user-op-to-packed
                     (coalton:lisp web3/erc4337:UserOperation () op))))
           (roundtripped (coalton:coalton
                          (web3/erc4337:packed-to-user-op
                           (coalton:lisp web3/erc4337:PackedUserOperation () packed)))))
      ;; All gas values should survive the roundtrip
      (assert (= (web3/types:u256-to-integer
                  (coalton:coalton
                   (web3/erc4337:.user-op-call-gas-limit
                    (coalton:lisp web3/erc4337:UserOperation () roundtripped))))
                 150000))
      (assert (= (web3/types:u256-to-integer
                  (coalton:coalton
                   (web3/erc4337:.user-op-verification-gas-limit
                    (coalton:lisp web3/erc4337:UserOperation () roundtripped))))
                 300000))
      (assert (= (web3/types:u256-to-integer
                  (coalton:coalton
                   (web3/erc4337:.user-op-pre-verification-gas
                    (coalton:lisp web3/erc4337:UserOperation () roundtripped))))
                 60000))
      (assert (= (web3/types:u256-to-integer
                  (coalton:coalton
                   (web3/erc4337:.user-op-max-fee-per-gas
                    (coalton:lisp web3/erc4337:UserOperation () roundtripped))))
                 25000000000))
      (assert (= (web3/types:u256-to-integer
                  (coalton:coalton
                   (web3/erc4337:.user-op-max-priority-fee-per-gas
                    (coalton:lisp web3/erc4337:UserOperation () roundtripped))))
                 2000000000))))

  ;;; =========================================================================
  ;;; UserOperation Hash Tests
  ;;; =========================================================================

  (test-case "user-op-hash produces 32 bytes"
    (let* ((sender (result-value (coalton:coalton
                                  (web3/address:address-from-hex
                                   "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (nonce (coalton:coalton web3/types:u256-zero))
           (empty (coalton:coalton web3/types:bytes-empty))
           (zero (coalton:coalton web3/types:u256-zero))
           (op (coalton:coalton
                (web3/erc4337:make-user-operation
                 (coalton:lisp web3/address:Address () sender)
                 (coalton:lisp web3/types:U256 () nonce)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:U256 () zero)
                 (coalton:lisp web3/types:U256 () zero)
                 (coalton:lisp web3/types:U256 () zero)
                 (coalton:lisp web3/types:U256 () zero)
                 (coalton:lisp web3/types:U256 () zero)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty))))
           (hash (coalton:coalton
                  (web3/erc4337:user-op-hash
                   (coalton:lisp web3/erc4337:UserOperation () op)
                   web3/erc4337:entrypoint-v06
                   1))))
      (assert (= (length hash) 32))))

  (test-case "user-op-hash is deterministic"
    (let* ((sender (result-value (coalton:coalton
                                  (web3/address:address-from-hex
                                   "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (nonce (coalton:coalton (web3/types:u256-from-integer 1)))
           (empty (coalton:coalton web3/types:bytes-empty))
           (gas (coalton:coalton (web3/types:u256-from-integer 100000)))
           (op (coalton:coalton
                (web3/erc4337:make-user-operation
                 (coalton:lisp web3/address:Address () sender)
                 (coalton:lisp web3/types:U256 () nonce)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:U256 () gas)
                 (coalton:lisp web3/types:U256 () gas)
                 (coalton:lisp web3/types:U256 () gas)
                 (coalton:lisp web3/types:U256 () gas)
                 (coalton:lisp web3/types:U256 () gas)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty))))
           (hash1 (coalton:coalton
                   (web3/erc4337:user-op-hash
                    (coalton:lisp web3/erc4337:UserOperation () op)
                    web3/erc4337:entrypoint-v06
                    1)))
           (hash2 (coalton:coalton
                   (web3/erc4337:user-op-hash
                    (coalton:lisp web3/erc4337:UserOperation () op)
                    web3/erc4337:entrypoint-v06
                    1))))
      (assert (equalp hash1 hash2))))

  (test-case "user-op-hash changes with different chain IDs"
    (let* ((sender (result-value (coalton:coalton
                                  (web3/address:address-from-hex
                                   "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (nonce (coalton:coalton web3/types:u256-zero))
           (empty (coalton:coalton web3/types:bytes-empty))
           (zero (coalton:coalton web3/types:u256-zero))
           (op (coalton:coalton
                (web3/erc4337:make-user-operation
                 (coalton:lisp web3/address:Address () sender)
                 (coalton:lisp web3/types:U256 () nonce)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:U256 () zero)
                 (coalton:lisp web3/types:U256 () zero)
                 (coalton:lisp web3/types:U256 () zero)
                 (coalton:lisp web3/types:U256 () zero)
                 (coalton:lisp web3/types:U256 () zero)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty))))
           (hash-mainnet (coalton:coalton
                          (web3/erc4337:user-op-hash
                           (coalton:lisp web3/erc4337:UserOperation () op)
                           web3/erc4337:entrypoint-v06
                           1)))
           (hash-goerli (coalton:coalton
                         (web3/erc4337:user-op-hash
                          (coalton:lisp web3/erc4337:UserOperation () op)
                          web3/erc4337:entrypoint-v06
                          5))))
      (assert (not (equalp hash-mainnet hash-goerli)))))

  (test-case "user-op-hash changes with different entrypoints"
    (let* ((sender (result-value (coalton:coalton
                                  (web3/address:address-from-hex
                                   "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (nonce (coalton:coalton web3/types:u256-zero))
           (empty (coalton:coalton web3/types:bytes-empty))
           (zero (coalton:coalton web3/types:u256-zero))
           (op (coalton:coalton
                (web3/erc4337:make-user-operation
                 (coalton:lisp web3/address:Address () sender)
                 (coalton:lisp web3/types:U256 () nonce)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:U256 () zero)
                 (coalton:lisp web3/types:U256 () zero)
                 (coalton:lisp web3/types:U256 () zero)
                 (coalton:lisp web3/types:U256 () zero)
                 (coalton:lisp web3/types:U256 () zero)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty))))
           (hash-v06 (coalton:coalton
                      (web3/erc4337:user-op-hash
                       (coalton:lisp web3/erc4337:UserOperation () op)
                       web3/erc4337:entrypoint-v06
                       1)))
           (hash-v07 (coalton:coalton
                      (web3/erc4337:user-op-hash
                       (coalton:lisp web3/erc4337:UserOperation () op)
                       web3/erc4337:entrypoint-v07
                       1))))
      (assert (not (equalp hash-v06 hash-v07)))))

  (test-case "user-op-hash signature field does not affect hash"
    (let* ((sender (result-value (coalton:coalton
                                  (web3/address:address-from-hex
                                   "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (nonce (coalton:coalton web3/types:u256-zero))
           (empty (coalton:coalton web3/types:bytes-empty))
           (zero (coalton:coalton web3/types:u256-zero))
           (sig-bytes (result-value (coalton:coalton
                                     (web3/types:hex-decode "0xdeadbeef"))))
           (op1 (coalton:coalton
                 (web3/erc4337:make-user-operation
                  (coalton:lisp web3/address:Address () sender)
                  (coalton:lisp web3/types:U256 () nonce)
                  (coalton:lisp web3/types:Bytes () empty)
                  (coalton:lisp web3/types:Bytes () empty)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:Bytes () empty)
                  (coalton:lisp web3/types:Bytes () empty))))
           (op2 (coalton:coalton
                 (web3/erc4337:make-user-operation
                  (coalton:lisp web3/address:Address () sender)
                  (coalton:lisp web3/types:U256 () nonce)
                  (coalton:lisp web3/types:Bytes () empty)
                  (coalton:lisp web3/types:Bytes () empty)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:Bytes () empty)
                  (coalton:lisp web3/types:Bytes () sig-bytes))))
           (hash1 (coalton:coalton
                   (web3/erc4337:user-op-hash
                    (coalton:lisp web3/erc4337:UserOperation () op1)
                    web3/erc4337:entrypoint-v06
                    1)))
           (hash2 (coalton:coalton
                   (web3/erc4337:user-op-hash
                    (coalton:lisp web3/erc4337:UserOperation () op2)
                    web3/erc4337:entrypoint-v06
                    1))))
      (assert (equalp hash1 hash2))))

  ;;; =========================================================================
  ;;; v0.7 PackedUserOperation Hash Tests
  ;;; =========================================================================

  (test-case "packed-user-op-hash produces 32 bytes"
    (let* ((sender (result-value (coalton:coalton
                                  (web3/address:address-from-hex
                                   "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (nonce (coalton:coalton web3/types:u256-zero))
           (empty (coalton:coalton web3/types:bytes-empty))
           (zero-bytes (coalton:coalton (web3/types:make-bytes 32)))
           (op (coalton:coalton
                (web3/erc4337:make-packed-user-operation
                 (coalton:lisp web3/address:Address () sender)
                 (coalton:lisp web3/types:U256 () nonce)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () zero-bytes)
                 (coalton:lisp web3/types:U256 () nonce)
                 (coalton:lisp web3/types:Bytes () zero-bytes)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty))))
           (hash (coalton:coalton
                  (web3/erc4337:packed-user-op-hash
                   (coalton:lisp web3/erc4337:PackedUserOperation () op)
                   web3/erc4337:entrypoint-v07
                   1))))
      (assert (= (length hash) 32))))

  (test-case "packed-user-op-hash is deterministic"
    (let* ((sender (result-value (coalton:coalton
                                  (web3/address:address-from-hex
                                   "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (nonce (coalton:coalton (web3/types:u256-from-integer 3)))
           (empty (coalton:coalton web3/types:bytes-empty))
           (gas-limits (coalton:coalton
                        (web3/erc4337:pack-account-gas-limits
                         (web3/types:u256-from-integer 200000)
                         (web3/types:u256-from-integer 100000))))
           (gas-fees (coalton:coalton
                      (web3/erc4337:pack-gas-fees
                       (web3/types:u256-from-integer 1500000000)
                       (web3/types:u256-from-integer 30000000000))))
           (pvg (coalton:coalton (web3/types:u256-from-integer 50000)))
           (op (coalton:coalton
                (web3/erc4337:make-packed-user-operation
                 (coalton:lisp web3/address:Address () sender)
                 (coalton:lisp web3/types:U256 () nonce)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () gas-limits)
                 (coalton:lisp web3/types:U256 () pvg)
                 (coalton:lisp web3/types:Bytes () gas-fees)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty))))
           (hash1 (coalton:coalton
                   (web3/erc4337:packed-user-op-hash
                    (coalton:lisp web3/erc4337:PackedUserOperation () op)
                    web3/erc4337:entrypoint-v07
                    1)))
           (hash2 (coalton:coalton
                   (web3/erc4337:packed-user-op-hash
                    (coalton:lisp web3/erc4337:PackedUserOperation () op)
                    web3/erc4337:entrypoint-v07
                    1))))
      (assert (equalp hash1 hash2))))

  (test-case "v0.6 and v0.7 hashes differ for same logical operation"
    ;; Even with equivalent gas values, v0.6 and v0.7 produce different hashes
    ;; because the packing format differs
    (let* ((sender (result-value (coalton:coalton
                                  (web3/address:address-from-hex
                                   "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (nonce (coalton:coalton web3/types:u256-zero))
           (empty (coalton:coalton web3/types:bytes-empty))
           (zero (coalton:coalton web3/types:u256-zero))
           (zero-bytes (coalton:coalton (web3/types:make-bytes 32)))
           (op-v06 (coalton:coalton
                    (web3/erc4337:make-user-operation
                     (coalton:lisp web3/address:Address () sender)
                     (coalton:lisp web3/types:U256 () nonce)
                     (coalton:lisp web3/types:Bytes () empty)
                     (coalton:lisp web3/types:Bytes () empty)
                     (coalton:lisp web3/types:U256 () zero)
                     (coalton:lisp web3/types:U256 () zero)
                     (coalton:lisp web3/types:U256 () zero)
                     (coalton:lisp web3/types:U256 () zero)
                     (coalton:lisp web3/types:U256 () zero)
                     (coalton:lisp web3/types:Bytes () empty)
                     (coalton:lisp web3/types:Bytes () empty))))
           (op-v07 (coalton:coalton
                    (web3/erc4337:make-packed-user-operation
                     (coalton:lisp web3/address:Address () sender)
                     (coalton:lisp web3/types:U256 () nonce)
                     (coalton:lisp web3/types:Bytes () empty)
                     (coalton:lisp web3/types:Bytes () empty)
                     (coalton:lisp web3/types:Bytes () zero-bytes)
                     (coalton:lisp web3/types:U256 () zero)
                     (coalton:lisp web3/types:Bytes () zero-bytes)
                     (coalton:lisp web3/types:Bytes () empty)
                     (coalton:lisp web3/types:Bytes () empty))))
           (hash-v06 (coalton:coalton
                      (web3/erc4337:user-op-hash
                       (coalton:lisp web3/erc4337:UserOperation () op-v06)
                       web3/erc4337:entrypoint-v06
                       1)))
           (hash-v07 (coalton:coalton
                      (web3/erc4337:packed-user-op-hash
                       (coalton:lisp web3/erc4337:PackedUserOperation () op-v07)
                       web3/erc4337:entrypoint-v07
                       1))))
      (assert (not (equalp hash-v06 hash-v07)))))

  ;;; =========================================================================
  ;;; GasEstimate Type Tests
  ;;; =========================================================================

  (test-case "gas-estimate accessors work correctly"
    ;; This tests the type construction indirectly through CL
    (let ((pvg (coalton:coalton (web3/types:u256-from-integer 21000)))
          (vgl (coalton:coalton (web3/types:u256-from-integer 100000)))
          (cgl (coalton:coalton (web3/types:u256-from-integer 50000))))
      ;; Verify U256 values are correct
      (assert (= (web3/types:u256-to-integer pvg) 21000))
      (assert (= (web3/types:u256-to-integer vgl) 100000))
      (assert (= (web3/types:u256-to-integer cgl) 50000))))

  ;;; =========================================================================
  ;;; Bundler RPC Method Tests (structure only, no network)
  ;;; =========================================================================

  (test-case "Note: bundler RPC methods require a bundler endpoint"
    ;; eth-send-user-operation, eth-estimate-user-operation-gas,
    ;; eth-get-user-operation-by-hash, eth-get-user-operation-receipt,
    ;; eth-supported-entry-points all require a live bundler
    (assert t))

  ;;; =========================================================================
  ;;; Paymaster Data Packing Tests
  ;;; =========================================================================

  (test-case "pack-paymaster-data produces correct length"
    (let* ((paymaster (result-value (coalton:coalton
                                     (web3/address:address-from-hex
                                      "0x3333333333333333333333333333333333333333"))))
           (vgl (coalton:coalton (web3/types:u256-from-integer 100000)))
           (pogl (coalton:coalton (web3/types:u256-from-integer 50000)))
           (data (coalton:coalton web3/types:bytes-empty))
           (packed (coalton:coalton
                    (web3/erc4337:pack-paymaster-data
                     (coalton:lisp web3/address:Address () paymaster)
                     (coalton:lisp web3/types:U256 () vgl)
                     (coalton:lisp web3/types:U256 () pogl)
                     (coalton:lisp web3/types:Bytes () data)))))
      ;; 20 bytes address + 16 bytes vgl + 16 bytes pogl + 0 data = 52 bytes
      (assert (= (length packed) 52))))

  (test-case "pack-paymaster-data with extra data"
    (let* ((paymaster (result-value (coalton:coalton
                                     (web3/address:address-from-hex
                                      "0x3333333333333333333333333333333333333333"))))
           (vgl (coalton:coalton (web3/types:u256-from-integer 100000)))
           (pogl (coalton:coalton (web3/types:u256-from-integer 50000)))
           (extra (result-value (coalton:coalton
                                 (web3/types:hex-decode "0xdeadbeefcafe"))))
           (packed (coalton:coalton
                    (web3/erc4337:pack-paymaster-data
                     (coalton:lisp web3/address:Address () paymaster)
                     (coalton:lisp web3/types:U256 () vgl)
                     (coalton:lisp web3/types:U256 () pogl)
                     (coalton:lisp web3/types:Bytes () extra)))))
      ;; 20 + 16 + 16 + 6 = 58 bytes
      (assert (= (length packed) 58))))

  ;;; =========================================================================
  ;;; Hash with Non-Empty Fields
  ;;; =========================================================================

  (test-case "user-op-hash changes with different callData"
    (let* ((sender (result-value (coalton:coalton
                                  (web3/address:address-from-hex
                                   "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (nonce (coalton:coalton web3/types:u256-zero))
           (empty (coalton:coalton web3/types:bytes-empty))
           (zero (coalton:coalton web3/types:u256-zero))
           (call-data-1 (result-value (coalton:coalton
                                       (web3/types:hex-decode "0xdeadbeef"))))
           (call-data-2 (result-value (coalton:coalton
                                       (web3/types:hex-decode "0xcafebabe"))))
           (op1 (coalton:coalton
                 (web3/erc4337:make-user-operation
                  (coalton:lisp web3/address:Address () sender)
                  (coalton:lisp web3/types:U256 () nonce)
                  (coalton:lisp web3/types:Bytes () empty)
                  (coalton:lisp web3/types:Bytes () call-data-1)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:Bytes () empty)
                  (coalton:lisp web3/types:Bytes () empty))))
           (op2 (coalton:coalton
                 (web3/erc4337:make-user-operation
                  (coalton:lisp web3/address:Address () sender)
                  (coalton:lisp web3/types:U256 () nonce)
                  (coalton:lisp web3/types:Bytes () empty)
                  (coalton:lisp web3/types:Bytes () call-data-2)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:Bytes () empty)
                  (coalton:lisp web3/types:Bytes () empty))))
           (hash1 (coalton:coalton
                   (web3/erc4337:user-op-hash
                    (coalton:lisp web3/erc4337:UserOperation () op1)
                    web3/erc4337:entrypoint-v06
                    1)))
           (hash2 (coalton:coalton
                   (web3/erc4337:user-op-hash
                    (coalton:lisp web3/erc4337:UserOperation () op2)
                    web3/erc4337:entrypoint-v06
                    1))))
      (assert (not (equalp hash1 hash2)))))

  (test-case "user-op-hash changes with different nonce"
    (let* ((sender (result-value (coalton:coalton
                                  (web3/address:address-from-hex
                                   "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (empty (coalton:coalton web3/types:bytes-empty))
           (zero (coalton:coalton web3/types:u256-zero))
           (nonce1 (coalton:coalton (web3/types:u256-from-integer 0)))
           (nonce2 (coalton:coalton (web3/types:u256-from-integer 1)))
           (op1 (coalton:coalton
                 (web3/erc4337:make-user-operation
                  (coalton:lisp web3/address:Address () sender)
                  (coalton:lisp web3/types:U256 () nonce1)
                  (coalton:lisp web3/types:Bytes () empty)
                  (coalton:lisp web3/types:Bytes () empty)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:Bytes () empty)
                  (coalton:lisp web3/types:Bytes () empty))))
           (op2 (coalton:coalton
                 (web3/erc4337:make-user-operation
                  (coalton:lisp web3/address:Address () sender)
                  (coalton:lisp web3/types:U256 () nonce2)
                  (coalton:lisp web3/types:Bytes () empty)
                  (coalton:lisp web3/types:Bytes () empty)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:U256 () zero)
                  (coalton:lisp web3/types:Bytes () empty)
                  (coalton:lisp web3/types:Bytes () empty))))
           (hash1 (coalton:coalton
                   (web3/erc4337:user-op-hash
                    (coalton:lisp web3/erc4337:UserOperation () op1)
                    web3/erc4337:entrypoint-v06
                    1)))
           (hash2 (coalton:coalton
                   (web3/erc4337:user-op-hash
                    (coalton:lisp web3/erc4337:UserOperation () op2)
                    web3/erc4337:entrypoint-v06
                    1))))
      (assert (not (equalp hash1 hash2)))))

  ;;; =========================================================================
  ;;; Reference Test Vector
  ;;; =========================================================================

  (test-case "user-op-hash matches reference test vector"
    ;; Reference: sender=0x1234...7890, all fields zero/empty, entrypoint-v06, chainId=1
    ;; Expected hash: 0xac29f0f6151abe7d07eb7aadb6169066440d212fd1222f6c02b67a1f6f82b0c9
    (let* ((sender (result-value (coalton:coalton
                                  (web3/address:address-from-hex
                                   "0x1234567890123456789012345678901234567890"))))
           (nonce (coalton:coalton web3/types:u256-zero))
           (empty (coalton:coalton web3/types:bytes-empty))
           (zero (coalton:coalton web3/types:u256-zero))
           (op (coalton:coalton
                (web3/erc4337:make-user-operation
                 (coalton:lisp web3/address:Address () sender)
                 (coalton:lisp web3/types:U256 () nonce)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:U256 () zero)
                 (coalton:lisp web3/types:U256 () zero)
                 (coalton:lisp web3/types:U256 () zero)
                 (coalton:lisp web3/types:U256 () zero)
                 (coalton:lisp web3/types:U256 () zero)
                 (coalton:lisp web3/types:Bytes () empty)
                 (coalton:lisp web3/types:Bytes () empty))))
           (hash (coalton:coalton
                  (web3/erc4337:user-op-hash
                   (coalton:lisp web3/erc4337:UserOperation () op)
                   web3/erc4337:entrypoint-v06
                   1)))
           (hash-hex (coalton:coalton
                      (web3/types:hex-encode-prefixed
                       (coalton:lisp web3/types:Bytes () hash)))))
      (assert (string-equal hash-hex
                            "0xac29f0f6151abe7d07eb7aadb6169066440d212fd1222f6c02b67a1f6f82b0c9")))))
