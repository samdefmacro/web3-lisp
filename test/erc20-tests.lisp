;;; ERC-20 module tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; ERC-20 Function Selector Tests
;;; =========================================================================

(defun run-erc20-tests ()
  (format t "~%=== ERC-20 Tests ===~%")

  ;; Function selector tests - verify they match known values
  (test-case "selector name() = 0x06fdde03"
    (let ((selector (coalton:coalton web3/erc20:selector-name)))
      (assert (= (length selector) 4))
      (assert (= (aref selector 0) #x06))
      (assert (= (aref selector 1) #xfd))
      (assert (= (aref selector 2) #xde))
      (assert (= (aref selector 3) #x03))))

  (test-case "selector symbol() = 0x95d89b41"
    (let ((selector (coalton:coalton web3/erc20:selector-symbol)))
      (assert (= (aref selector 0) #x95))
      (assert (= (aref selector 1) #xd8))
      (assert (= (aref selector 2) #x9b))
      (assert (= (aref selector 3) #x41))))

  (test-case "selector decimals() = 0x313ce567"
    (let ((selector (coalton:coalton web3/erc20:selector-decimals)))
      (assert (= (aref selector 0) #x31))
      (assert (= (aref selector 1) #x3c))
      (assert (= (aref selector 2) #xe5))
      (assert (= (aref selector 3) #x67))))

  (test-case "selector totalSupply() = 0x18160ddd"
    (let ((selector (coalton:coalton web3/erc20:selector-total-supply)))
      (assert (= (aref selector 0) #x18))
      (assert (= (aref selector 1) #x16))
      (assert (= (aref selector 2) #x0d))
      (assert (= (aref selector 3) #xdd))))

  (test-case "selector balanceOf(address) = 0x70a08231"
    (let ((selector (coalton:coalton web3/erc20:selector-balance-of)))
      (assert (= (aref selector 0) #x70))
      (assert (= (aref selector 1) #xa0))
      (assert (= (aref selector 2) #x82))
      (assert (= (aref selector 3) #x31))))

  (test-case "selector allowance(address,address) = 0xdd62ed3e"
    (let ((selector (coalton:coalton web3/erc20:selector-allowance)))
      (assert (= (aref selector 0) #xdd))
      (assert (= (aref selector 1) #x62))
      (assert (= (aref selector 2) #xed))
      (assert (= (aref selector 3) #x3e))))

  (test-case "selector transfer(address,uint256) = 0xa9059cbb"
    (let ((selector (coalton:coalton web3/erc20:selector-transfer)))
      (assert (= (aref selector 0) #xa9))
      (assert (= (aref selector 1) #x05))
      (assert (= (aref selector 2) #x9c))
      (assert (= (aref selector 3) #xbb))))

  (test-case "selector approve(address,uint256) = 0x095ea7b3"
    (let ((selector (coalton:coalton web3/erc20:selector-approve)))
      (assert (= (aref selector 0) #x09))
      (assert (= (aref selector 1) #x5e))
      (assert (= (aref selector 2) #xa7))
      (assert (= (aref selector 3) #xb3))))

  (test-case "selector transferFrom(address,address,uint256) = 0x23b872dd"
    (let ((selector (coalton:coalton web3/erc20:selector-transfer-from)))
      (assert (= (aref selector 0) #x23))
      (assert (= (aref selector 1) #xb8))
      (assert (= (aref selector 2) #x72))
      (assert (= (aref selector 3) #xdd))))

  ;;; =========================================================================
  ;;; Calldata Builder Tests
  ;;; =========================================================================

  (test-case "erc20-transfer-data builds correct calldata"
    (let* ((to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (amount (coalton:coalton (web3/types:u256-from-integer 1000000000000000000)))
           (calldata (coalton:coalton
                      (web3/erc20:erc20-transfer-data
                       (coalton:lisp web3/address:Address () to-addr)
                       (coalton:lisp web3/types:U256 () amount)))))
      ;; 4 bytes selector + 32 bytes address + 32 bytes amount = 68 bytes
      (assert (= (length calldata) 68))
      ;; Check selector
      (assert (= (aref calldata 0) #xa9))
      (assert (= (aref calldata 1) #x05))
      (assert (= (aref calldata 2) #x9c))
      (assert (= (aref calldata 3) #xbb))))

  (test-case "erc20-approve-data builds correct calldata"
    (let* ((spender (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0x1234567890123456789012345678901234567890"))))
           (amount (coalton:coalton (web3/types:u256-from-integer 500)))
           (calldata (coalton:coalton
                      (web3/erc20:erc20-approve-data
                       (coalton:lisp web3/address:Address () spender)
                       (coalton:lisp web3/types:U256 () amount)))))
      ;; 4 bytes selector + 32 bytes address + 32 bytes amount = 68 bytes
      (assert (= (length calldata) 68))
      ;; Check selector (approve)
      (assert (= (aref calldata 0) #x09))
      (assert (= (aref calldata 1) #x5e))
      (assert (= (aref calldata 2) #xa7))
      (assert (= (aref calldata 3) #xb3))))

  (test-case "erc20-transfer-from-data builds correct calldata"
    (let* ((from-addr (result-value (coalton:coalton
                                     (web3/address:address-from-hex
                                      "0x1111111111111111111111111111111111111111"))))
           (to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0x2222222222222222222222222222222222222222"))))
           (amount (coalton:coalton (web3/types:u256-from-integer 100)))
           (calldata (coalton:coalton
                      (web3/erc20:erc20-transfer-from-data
                       (coalton:lisp web3/address:Address () from-addr)
                       (coalton:lisp web3/address:Address () to-addr)
                       (coalton:lisp web3/types:U256 () amount)))))
      ;; 4 bytes selector + 32 bytes from + 32 bytes to + 32 bytes amount = 100 bytes
      (assert (= (length calldata) 100))
      ;; Check selector (transferFrom)
      (assert (= (aref calldata 0) #x23))
      (assert (= (aref calldata 1) #xb8))
      (assert (= (aref calldata 2) #x72))
      (assert (= (aref calldata 3) #xdd))))

  (test-case "erc20-transfer-data with zero amount"
    (let* ((to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0x0000000000000000000000000000000000000001"))))
           (amount (coalton:coalton web3/types:u256-zero))
           (calldata (coalton:coalton
                      (web3/erc20:erc20-transfer-data
                       (coalton:lisp web3/address:Address () to-addr)
                       (coalton:lisp web3/types:U256 () amount)))))
      (assert (= (length calldata) 68))
      ;; Last 32 bytes should be zero (amount)
      (assert (= (aref calldata 67) 0))
      (assert (= (aref calldata 36) 0))))

  (test-case "erc20-approve-data max uint256"
    ;; Common pattern: approve max uint256 for unlimited spending
    (let* ((spender (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0x1234567890123456789012345678901234567890"))))
           (max-amount (coalton:coalton web3/types:u256-max))
           (calldata (coalton:coalton
                      (web3/erc20:erc20-approve-data
                       (coalton:lisp web3/address:Address () spender)
                       (coalton:lisp web3/types:U256 () max-amount)))))
      (assert (= (length calldata) 68))
      ;; Last 32 bytes should all be 0xff (max uint256)
      (assert (= (aref calldata 36) #xff))
      (assert (= (aref calldata 67) #xff))))

  ;;; =========================================================================
  ;;; Selector Consistency Tests
  ;;; =========================================================================

  (test-case "selector-name is consistent across calls"
    (let ((sel1 (coalton:coalton web3/erc20:selector-name))
          (sel2 (coalton:coalton web3/erc20:selector-name)))
      (assert (equalp sel1 sel2))))

  (test-case "selector-transfer is consistent across calls"
    (let ((sel1 (coalton:coalton web3/erc20:selector-transfer))
          (sel2 (coalton:coalton web3/erc20:selector-transfer)))
      (assert (equalp sel1 sel2))))

  (test-case "selector-approve is consistent across calls"
    (let ((sel1 (coalton:coalton web3/erc20:selector-approve))
          (sel2 (coalton:coalton web3/erc20:selector-approve)))
      (assert (equalp sel1 sel2))))

  ;;; =========================================================================
  ;;; Address Encoding Tests
  ;;; =========================================================================

  (test-case "erc20-transfer-data encodes address with correct padding"
    (let* ((to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (amount (coalton:coalton (web3/types:u256-from-integer 1)))
           (calldata (coalton:coalton
                      (web3/erc20:erc20-transfer-data
                       (coalton:lisp web3/address:Address () to-addr)
                       (coalton:lisp web3/types:U256 () amount)))))
      ;; Address is padded to 32 bytes (12 zeros + 20 address bytes)
      ;; Bytes 4-15 should be zeros (padding)
      (assert (= (aref calldata 4) 0))
      (assert (= (aref calldata 15) 0))
      ;; Bytes 16-35 contain the address (starting at byte 16)
      (assert (= (aref calldata 16) #xd8))
      (assert (= (aref calldata 35) #x45))))

  (test-case "erc20-transfer-data with zero address"
    (let* ((zero-addr (result-value (coalton:coalton
                                     (web3/address:address-from-hex
                                      "0x0000000000000000000000000000000000000000"))))
           (amount (coalton:coalton (web3/types:u256-from-integer 100)))
           (calldata (coalton:coalton
                      (web3/erc20:erc20-transfer-data
                       (coalton:lisp web3/address:Address () zero-addr)
                       (coalton:lisp web3/types:U256 () amount)))))
      (assert (= (length calldata) 68))
      ;; First 32 bytes after selector should all be zero (zero address)
      (loop for i from 4 below 36
            do (assert (= (aref calldata i) 0)))))

  (test-case "erc20-transfer-data with max address"
    (let* ((max-addr (result-value (coalton:coalton
                                    (web3/address:address-from-hex
                                     "0xffffffffffffffffffffffffffffffffffffffff"))))
           (amount (coalton:coalton (web3/types:u256-from-integer 1)))
           (calldata (coalton:coalton
                      (web3/erc20:erc20-transfer-data
                       (coalton:lisp web3/address:Address () max-addr)
                       (coalton:lisp web3/types:U256 () amount)))))
      (assert (= (length calldata) 68))
      ;; Last 20 bytes of first arg should all be 0xff
      (assert (= (aref calldata 16) #xff))
      (assert (= (aref calldata 35) #xff))))

  ;;; =========================================================================
  ;;; Amount Encoding Tests
  ;;; =========================================================================

  (test-case "erc20-transfer-data with 1 wei"
    (let* ((to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0x1111111111111111111111111111111111111111"))))
           (amount (coalton:coalton (web3/types:u256-from-integer 1)))
           (calldata (coalton:coalton
                      (web3/erc20:erc20-transfer-data
                       (coalton:lisp web3/address:Address () to-addr)
                       (coalton:lisp web3/types:U256 () amount)))))
      ;; Amount 1 should have only last byte as 1
      (assert (= (aref calldata 67) 1))
      (assert (= (aref calldata 66) 0))))

  (test-case "erc20-transfer-data with 1 ether (10^18 wei)"
    (let* ((to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0x1111111111111111111111111111111111111111"))))
           ;; 1 ether = 1000000000000000000 wei = 0xde0b6b3a7640000
           (amount (coalton:coalton (web3/types:u256-from-integer 1000000000000000000)))
           (calldata (coalton:coalton
                      (web3/erc20:erc20-transfer-data
                       (coalton:lisp web3/address:Address () to-addr)
                       (coalton:lisp web3/types:U256 () amount)))))
      (assert (= (length calldata) 68))
      ;; Verify amount is encoded (last byte should be 0)
      (assert (= (aref calldata 67) 0))
      ;; Some non-zero bytes in the middle of the amount
      (assert (> (aref calldata 60) 0))))

  ;;; =========================================================================
  ;;; TransferFrom Edge Cases
  ;;; =========================================================================

  (test-case "erc20-transfer-from-data with same from and to address"
    (let* ((addr (result-value (coalton:coalton
                                (web3/address:address-from-hex
                                 "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))))
           (amount (coalton:coalton (web3/types:u256-from-integer 100)))
           (calldata (coalton:coalton
                      (web3/erc20:erc20-transfer-from-data
                       (coalton:lisp web3/address:Address () addr)
                       (coalton:lisp web3/address:Address () addr)
                       (coalton:lisp web3/types:U256 () amount)))))
      (assert (= (length calldata) 100))
      ;; Both from and to should have the same address bytes
      ;; From is at bytes 16-35, To is at bytes 48-67
      (assert (= (aref calldata 16) #xaa))
      (assert (= (aref calldata 48) #xaa))))

  (test-case "erc20-transfer-from-data preserves address order"
    (let* ((from-addr (result-value (coalton:coalton
                                     (web3/address:address-from-hex
                                      "0x1111111111111111111111111111111111111111"))))
           (to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0x2222222222222222222222222222222222222222"))))
           (amount (coalton:coalton (web3/types:u256-from-integer 50)))
           (calldata (coalton:coalton
                      (web3/erc20:erc20-transfer-from-data
                       (coalton:lisp web3/address:Address () from-addr)
                       (coalton:lisp web3/address:Address () to-addr)
                       (coalton:lisp web3/types:U256 () amount)))))
      ;; From address first (at byte 16)
      (assert (= (aref calldata 16) #x11))
      ;; To address second (at byte 48)
      (assert (= (aref calldata 48) #x22))))

  ;;; =========================================================================
  ;;; Approve Edge Cases
  ;;; =========================================================================

  (test-case "erc20-approve-data with zero amount (revoke)"
    (let* ((spender (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0x1234567890123456789012345678901234567890"))))
           (zero-amount (coalton:coalton web3/types:u256-zero))
           (calldata (coalton:coalton
                      (web3/erc20:erc20-approve-data
                       (coalton:lisp web3/address:Address () spender)
                       (coalton:lisp web3/types:U256 () zero-amount)))))
      (assert (= (length calldata) 68))
      ;; All amount bytes should be zero
      (loop for i from 36 below 68
            do (assert (= (aref calldata i) 0)))))

  (test-case "erc20-approve-data preserves spender address"
    (let* ((spender (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xabcdef0123456789abcdef0123456789abcdef01"))))
           (amount (coalton:coalton (web3/types:u256-from-integer 1000)))
           (calldata (coalton:coalton
                      (web3/erc20:erc20-approve-data
                       (coalton:lisp web3/address:Address () spender)
                       (coalton:lisp web3/types:U256 () amount)))))
      ;; Spender address at bytes 16-35
      (assert (= (aref calldata 16) #xab))
      (assert (= (aref calldata 17) #xcd))
      (assert (= (aref calldata 35) #x01))))

  ;;; =========================================================================
  ;;; Selector Size Tests
  ;;; =========================================================================

  (test-case "all selectors are exactly 4 bytes"
    (assert (= (length (coalton:coalton web3/erc20:selector-name)) 4))
    (assert (= (length (coalton:coalton web3/erc20:selector-symbol)) 4))
    (assert (= (length (coalton:coalton web3/erc20:selector-decimals)) 4))
    (assert (= (length (coalton:coalton web3/erc20:selector-total-supply)) 4))
    (assert (= (length (coalton:coalton web3/erc20:selector-balance-of)) 4))
    (assert (= (length (coalton:coalton web3/erc20:selector-allowance)) 4))
    (assert (= (length (coalton:coalton web3/erc20:selector-transfer)) 4))
    (assert (= (length (coalton:coalton web3/erc20:selector-approve)) 4))
    (assert (= (length (coalton:coalton web3/erc20:selector-transfer-from)) 4)))

  ;;; =========================================================================
  ;;; Network-Dependent Tests Note
  ;;; =========================================================================

  (test-case "Note: ERC20 read functions require network"
    ;; Functions that require a provider (erc20-name, erc20-symbol, etc.)
    ;; can only be tested with a running Ethereum node
    (assert t)))

;;; =========================================================================
;;; Main Test Runner
;;; =========================================================================

(defun run-all-tests ()
  "Run all web3-coalton tests"
  (setf *tests-passed* 0)
  (setf *tests-failed* 0)
  (format t "~%========================================~%")
  (format t "Running web3-coalton Tests~%")
  (format t "========================================~%")

  ;; Phase 1: Types + RLP + Crypto
  (run-hex-tests)
  (run-u256-tests)
  (run-bytes-tests)
  (run-unit-conversion-tests)
  (run-rlp-tests)
  (run-crypto-tests)

  ;; Phase 2: Address
  (run-address-tests)

  ;; Phase 3: ABI
  (run-abi-tests)

  ;; Phase 4: Transaction
  (run-transaction-tests)

  ;; Phase 5: Provider + Wallet
  (run-provider-tests)
  (run-wallet-tests)

  ;; Phase 6: ERC-20
  (run-erc20-tests)

  ;; Phase 7: ERC-721
  (run-erc721-tests)
  (run-erc721-metadata-tests)

  ;; Phase 8: ERC-1155
  (run-erc1155-tests)
  (run-erc1155-metadata-tests)

  ;; Phase 9: Events
  (run-events-tests)
  (run-deploy-tests)
  (run-ens-tests)
  (run-ens-resolver-tests)
  (run-multicall-tests)

  ;; Phase 10: EIP-712
  (run-eip712-tests)

  ;; Phase 11: HD Wallet (BIP-39/BIP-32)
  (run-hdwallet-tests)

  ;; Phase 12: ABI Parser
  (run-abi-parser-tests)

  ;; Phase 13: Contract Abstraction
  (run-contract-tests)

  ;; Phase 14: WebSocket Provider
  (run-ws-provider-tests)

  ;; Phase 15: Gas Utilities
  (run-gas-tests)

  ;; Phase 16: Receipt Parsing
  (run-receipt-tests)

  ;; Phase 17: Signature Utilities
  (run-signature-tests)

  ;; Phase 18: Chain Configs
  (run-chain-tests)

  ;; Phase 19: Block Parsing
  (run-block-tests)

  ;; Phase 20: Units (parseUnits/formatUnits)
  (run-units-tests)

  ;; Phase 21: Blob (EIP-4844)
  (run-blob-tests)

  ;; Phase 22: KZG Commitments
  (run-kzg-tests)

  ;; Phase 23: SIWE (Sign-In with Ethereum)
  (run-siwe-tests)

  ;; Phase 24: Nonce Manager
  (run-nonce-manager-tests)

  ;; Phase 25: Transaction Simulation & Estimation
  (run-simulate-tests)

  ;; Phase 26: Event Log Querying
  (run-logs-tests)

  ;; Phase 27: ERC-4337 Account Abstraction
  (run-erc4337-tests)

  ;; Phase 28: Batch JSON-RPC Provider
  (run-batch-provider-tests)

  ;; Phase 29: EIP-2612 Permit
  (run-permit-tests)

  ;; Phase 30: Revert Reason Decoding
  (run-revert-tests)

  ;; Phase 31: ERC-165 Interface Detection
  (run-erc165-tests)

  ;; Phase 32: Integration Tests (Anvil - skipped unless WEB3_INTEGRATION=1)
  (run-integration-tests)

  (format t "~%========================================~%")
  (format t "Results: ~A passed, ~A failed~%" *tests-passed* *tests-failed*)
  (format t "========================================~%")
  (values *tests-passed* *tests-failed*))
