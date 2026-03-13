;;; EIP-2612 Permit tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; EIP-2612 Permit Tests
;;; =========================================================================

(defun run-permit-tests ()
  (format t "~%=== EIP-2612 Permit Tests ===~%")

  ;;; =========================================================================
  ;;; Function Selector Tests
  ;;; =========================================================================

  (test-case "selector-nonces is 4 bytes"
    (let ((sel (coalton:coalton web3/permit:selector-nonces)))
      (assert (= (length sel) 4))))

  (test-case "selector-nonces matches known value 0x7ecebe00"
    (let ((sel (coalton:coalton web3/permit:selector-nonces)))
      (assert (= (aref sel 0) #x7e))
      (assert (= (aref sel 1) #xce))
      (assert (= (aref sel 2) #xbe))
      (assert (= (aref sel 3) #x00))))

  (test-case "selector-permit is 4 bytes"
    (let ((sel (coalton:coalton web3/permit:selector-permit)))
      (assert (= (length sel) 4))))

  (test-case "selector-permit matches known value 0xd505accf"
    (let ((sel (coalton:coalton web3/permit:selector-permit)))
      (assert (= (aref sel 0) #xd5))
      (assert (= (aref sel 1) #x05))
      (assert (= (aref sel 2) #xac))
      (assert (= (aref sel 3) #xcf))))

  ;;; =========================================================================
  ;;; Sign Permit Tests
  ;;; =========================================================================

  (test-case "sign-permit produces valid signature"
    (let* ((private-key (result-value
                         (coalton:coalton
                          (web3/types:hex-decode
                           "ac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))))
           (token-addr (result-value
                        (coalton:coalton
                         (web3/types:hex-decode "A0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"))))
           (domain (coalton:coalton
                    (web3/eip712:permit-domain
                     "USD Coin"
                     "2"
                     (web3/types:u256-from-integer 1)
                     (coalton:lisp web3/types:Bytes () token-addr))))
           (owner-addr (result-value
                        (coalton:coalton
                         (web3/address:address-from-hex
                          "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (spender-addr (result-value
                          (coalton:coalton
                           (web3/address:address-from-hex
                            "0x70997970C51812dc3A010C7d01b50e0d17dc79C8"))))
           (sig-result (coalton:coalton
                        (web3/permit:sign-permit
                         (coalton:lisp web3/types:Bytes () private-key)
                         (coalton:lisp web3/eip712:EIP712Domain () domain)
                         (coalton:lisp web3/address:Address () owner-addr)
                         (coalton:lisp web3/address:Address () spender-addr)
                         (web3/types:u256-from-integer 1000000)
                         (web3/types:u256-from-integer 0)
                         (web3/types:u256-from-integer 1735689600)))))
      (assert (result-ok-p sig-result))
      ;; Signature should be 65 bytes
      (let ((sig-bytes (coalton:coalton
                        (web3/crypto:signature-to-bytes
                         (coalton:lisp web3/crypto:Signature ()
                           (result-value sig-result))))))
        (assert (= (length sig-bytes) 65)))))

  (test-case "sign-permit signature can recover signer public key"
    (let* ((private-key (result-value
                         (coalton:coalton
                          (web3/types:hex-decode
                           "ac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))))
           (token-addr (result-value
                        (coalton:coalton
                         (web3/types:hex-decode "A0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"))))
           (domain (coalton:coalton
                    (web3/eip712:permit-domain
                     "USD Coin" "2"
                     (web3/types:u256-from-integer 1)
                     (coalton:lisp web3/types:Bytes () token-addr))))
           (owner-addr (result-value
                        (coalton:coalton
                         (web3/address:address-from-hex
                          "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (spender-addr (result-value
                          (coalton:coalton
                           (web3/address:address-from-hex
                            "0x70997970C51812dc3A010C7d01b50e0d17dc79C8"))))
           ;; Get expected public key
           (expected-pubkey (result-value
                             (coalton:coalton
                              (web3/crypto:private-key-to-public-key
                               (coalton:lisp web3/types:Bytes () private-key)))))
           ;; Sign
           (sig (result-value
                 (coalton:coalton
                  (web3/permit:sign-permit
                   (coalton:lisp web3/types:Bytes () private-key)
                   (coalton:lisp web3/eip712:EIP712Domain () domain)
                   (coalton:lisp web3/address:Address () owner-addr)
                   (coalton:lisp web3/address:Address () spender-addr)
                   (web3/types:u256-from-integer 1000000)
                   (web3/types:u256-from-integer 0)
                   (web3/types:u256-from-integer 1735689600)))))
           ;; Recover using EIP-712 typed data recovery
           (domain-sep (coalton:coalton
                        (web3/eip712:domain-separator
                         (coalton:lisp web3/eip712:EIP712Domain () domain))))
           (struct-hash (coalton:coalton
                         (web3/eip712:permit-struct-hash
                          (coalton:lisp web3/types:Bytes ()
                            (coalton:coalton
                             (web3/address:address-bytes
                              (coalton:lisp web3/address:Address () owner-addr))))
                          (coalton:lisp web3/types:Bytes ()
                            (coalton:coalton
                             (web3/address:address-bytes
                              (coalton:lisp web3/address:Address () spender-addr))))
                          (web3/types:u256-from-integer 1000000)
                          (web3/types:u256-from-integer 0)
                          (web3/types:u256-from-integer 1735689600))))
           (recovered (result-value
                       (coalton:coalton
                        (web3/eip712:recover-typed-data-signer
                         (coalton:lisp web3/types:Bytes () domain-sep)
                         (coalton:lisp web3/types:Bytes () struct-hash)
                         (coalton:lisp web3/crypto:Signature () sig))))))
      (assert (equalp recovered expected-pubkey))))

  (test-case "sign-permit different values produce different signatures"
    (let* ((private-key (result-value
                         (coalton:coalton
                          (web3/types:hex-decode
                           "ac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))))
           (token-addr (result-value
                        (coalton:coalton
                         (web3/types:hex-decode "A0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"))))
           (domain (coalton:coalton
                    (web3/eip712:permit-domain
                     "USD Coin" "2"
                     (web3/types:u256-from-integer 1)
                     (coalton:lisp web3/types:Bytes () token-addr))))
           (owner-addr (result-value
                        (coalton:coalton
                         (web3/address:address-from-hex
                          "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (spender-addr (result-value
                          (coalton:coalton
                           (web3/address:address-from-hex
                            "0x70997970C51812dc3A010C7d01b50e0d17dc79C8"))))
           (sig1 (result-value
                  (coalton:coalton
                   (web3/permit:sign-permit
                    (coalton:lisp web3/types:Bytes () private-key)
                    (coalton:lisp web3/eip712:EIP712Domain () domain)
                    (coalton:lisp web3/address:Address () owner-addr)
                    (coalton:lisp web3/address:Address () spender-addr)
                    (web3/types:u256-from-integer 1000000)
                    (web3/types:u256-from-integer 0)
                    (web3/types:u256-from-integer 1735689600)))))
           (sig2 (result-value
                  (coalton:coalton
                   (web3/permit:sign-permit
                    (coalton:lisp web3/types:Bytes () private-key)
                    (coalton:lisp web3/eip712:EIP712Domain () domain)
                    (coalton:lisp web3/address:Address () owner-addr)
                    (coalton:lisp web3/address:Address () spender-addr)
                    (web3/types:u256-from-integer 2000000)
                    (web3/types:u256-from-integer 0)
                    (web3/types:u256-from-integer 1735689600)))))
           (bytes1 (coalton:coalton
                    (web3/crypto:signature-to-bytes
                     (coalton:lisp web3/crypto:Signature () sig1))))
           (bytes2 (coalton:coalton
                    (web3/crypto:signature-to-bytes
                     (coalton:lisp web3/crypto:Signature () sig2)))))
      (assert (not (equalp bytes1 bytes2)))))

  ;;; =========================================================================
  ;;; Permit Calldata Tests
  ;;; =========================================================================

  (test-case "permit-data starts with correct selector"
    (let* ((private-key (result-value
                         (coalton:coalton
                          (web3/types:hex-decode
                           "ac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))))
           (token-addr (result-value
                        (coalton:coalton
                         (web3/types:hex-decode "A0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"))))
           (domain (coalton:coalton
                    (web3/eip712:permit-domain
                     "USD Coin" "2"
                     (web3/types:u256-from-integer 1)
                     (coalton:lisp web3/types:Bytes () token-addr))))
           (owner-addr (result-value
                        (coalton:coalton
                         (web3/address:address-from-hex
                          "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (spender-addr (result-value
                          (coalton:coalton
                           (web3/address:address-from-hex
                            "0x70997970C51812dc3A010C7d01b50e0d17dc79C8"))))
           (sig (result-value
                 (coalton:coalton
                  (web3/permit:sign-permit
                   (coalton:lisp web3/types:Bytes () private-key)
                   (coalton:lisp web3/eip712:EIP712Domain () domain)
                   (coalton:lisp web3/address:Address () owner-addr)
                   (coalton:lisp web3/address:Address () spender-addr)
                   (web3/types:u256-from-integer 1000000)
                   (web3/types:u256-from-integer 0)
                   (web3/types:u256-from-integer 1735689600)))))
           (calldata (coalton:coalton
                      (web3/permit:permit-data
                       (coalton:lisp web3/address:Address () owner-addr)
                       (coalton:lisp web3/address:Address () spender-addr)
                       (web3/types:u256-from-integer 1000000)
                       (web3/types:u256-from-integer 1735689600)
                       (coalton:lisp web3/crypto:Signature () sig)))))
      ;; Starts with permit selector 0xd505accf
      (assert (= (aref calldata 0) #xd5))
      (assert (= (aref calldata 1) #x05))
      (assert (= (aref calldata 2) #xac))
      (assert (= (aref calldata 3) #xcf))
      ;; 4 bytes selector + 7 * 32 bytes params = 228 bytes
      (assert (= (length calldata) 228))))

  (test-case "permit-data encodes owner address correctly"
    (let* ((private-key (result-value
                         (coalton:coalton
                          (web3/types:hex-decode
                           "ac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))))
           (token-addr (result-value
                        (coalton:coalton
                         (web3/types:hex-decode "A0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"))))
           (domain (coalton:coalton
                    (web3/eip712:permit-domain
                     "USD Coin" "2"
                     (web3/types:u256-from-integer 1)
                     (coalton:lisp web3/types:Bytes () token-addr))))
           (owner-addr (result-value
                        (coalton:coalton
                         (web3/address:address-from-hex
                          "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (spender-addr (result-value
                          (coalton:coalton
                           (web3/address:address-from-hex
                            "0x70997970C51812dc3A010C7d01b50e0d17dc79C8"))))
           (sig (result-value
                 (coalton:coalton
                  (web3/permit:sign-permit
                   (coalton:lisp web3/types:Bytes () private-key)
                   (coalton:lisp web3/eip712:EIP712Domain () domain)
                   (coalton:lisp web3/address:Address () owner-addr)
                   (coalton:lisp web3/address:Address () spender-addr)
                   (web3/types:u256-from-integer 1000000)
                   (web3/types:u256-from-integer 0)
                   (web3/types:u256-from-integer 1735689600)))))
           (calldata (coalton:coalton
                      (web3/permit:permit-data
                       (coalton:lisp web3/address:Address () owner-addr)
                       (coalton:lisp web3/address:Address () spender-addr)
                       (web3/types:u256-from-integer 1000000)
                       (web3/types:u256-from-integer 1735689600)
                       (coalton:lisp web3/crypto:Signature () sig)))))
      ;; First param (bytes 4-35): owner address left-padded to 32 bytes
      ;; 12 zero bytes + 20 address bytes
      (assert (= (aref calldata 4) 0))
      (assert (= (aref calldata 15) 0))
      ;; Owner starts at byte 16 (4 + 12): 0xf39F...
      (assert (= (aref calldata 16) #xf3))
      (assert (= (aref calldata 17) #x9f))))

  ;;; =========================================================================
  ;;; erc20-permit (High-level) Tests
  ;;; =========================================================================

  (test-case "erc20-permit returns Ok with valid calldata"
    (let* ((private-key (result-value
                         (coalton:coalton
                          (web3/types:hex-decode
                           "ac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))))
           (token-addr (result-value
                        (coalton:coalton
                         (web3/types:hex-decode "A0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"))))
           (domain (coalton:coalton
                    (web3/eip712:permit-domain
                     "USD Coin" "2"
                     (web3/types:u256-from-integer 1)
                     (coalton:lisp web3/types:Bytes () token-addr))))
           (owner-addr (result-value
                        (coalton:coalton
                         (web3/address:address-from-hex
                          "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (spender-addr (result-value
                          (coalton:coalton
                           (web3/address:address-from-hex
                            "0x70997970C51812dc3A010C7d01b50e0d17dc79C8"))))
           (result (coalton:coalton
                    (web3/permit:erc20-permit
                     (coalton:lisp web3/types:Bytes () private-key)
                     (coalton:lisp web3/eip712:EIP712Domain () domain)
                     (coalton:lisp web3/address:Address () owner-addr)
                     (coalton:lisp web3/address:Address () spender-addr)
                     (web3/types:u256-from-integer 1000000)
                     (web3/types:u256-from-integer 0)
                     (web3/types:u256-from-integer 1735689600)))))
      (assert (result-ok-p result))
      (let ((calldata (result-value result)))
        ;; Correct selector
        (assert (= (aref calldata 0) #xd5))
        (assert (= (aref calldata 1) #x05))
        ;; Correct length: 4 + 7*32 = 228
        (assert (= (length calldata) 228)))))

  (test-case "erc20-permit non-signature params match permit-data"
    (let* ((private-key (result-value
                         (coalton:coalton
                          (web3/types:hex-decode
                           "ac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))))
           (token-addr (result-value
                        (coalton:coalton
                         (web3/types:hex-decode "A0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"))))
           (domain (coalton:coalton
                    (web3/eip712:permit-domain
                     "USD Coin" "2"
                     (web3/types:u256-from-integer 1)
                     (coalton:lisp web3/types:Bytes () token-addr))))
           (owner-addr (result-value
                        (coalton:coalton
                         (web3/address:address-from-hex
                          "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (spender-addr (result-value
                          (coalton:coalton
                           (web3/address:address-from-hex
                            "0x70997970C51812dc3A010C7d01b50e0d17dc79C8"))))
           ;; High-level combined call
           (combined (result-value
                      (coalton:coalton
                       (web3/permit:erc20-permit
                        (coalton:lisp web3/types:Bytes () private-key)
                        (coalton:lisp web3/eip712:EIP712Domain () domain)
                        (coalton:lisp web3/address:Address () owner-addr)
                        (coalton:lisp web3/address:Address () spender-addr)
                        (web3/types:u256-from-integer 1000000)
                        (web3/types:u256-from-integer 0)
                        (web3/types:u256-from-integer 1735689600))))))
      ;; First 132 bytes (selector + owner + spender + value + deadline) are deterministic
      ;; Bytes 0-3: selector, 4-35: owner, 36-67: spender, 68-99: value, 100-131: deadline
      ;; These should match across calls since they don't depend on the signature
      (assert (= (length combined) 228))
      ;; Selector
      (assert (= (aref combined 0) #xd5))
      ;; Owner address at byte 16
      (assert (= (aref combined 16) #xf3))
      (assert (= (aref combined 17) #x9f))
      ;; Spender address at byte 48 (4 + 32 + 12)
      (assert (= (aref combined 48) #x70))
      (assert (= (aref combined 49) #x99))
      ;; Value (1000000 = 0xF4240) at bytes 68-99
      (assert (= (aref combined 99) #x40))
      (assert (= (aref combined 98) #x42))
      (assert (= (aref combined 97) #x0F))))

  ;;; =========================================================================
  ;;; Integration Tests (require WEB3_TEST_RPC_URL)
  ;;; =========================================================================

  (let ((rpc-url (uiop:getenv "WEB3_TEST_RPC_URL")))
    (if rpc-url
        (progn
          (test-case "permit-nonces reads nonce from USDC on mainnet"
            (let* ((provider (coalton:coalton
                              (web3/provider:make-http-provider
                               (coalton:lisp coalton:String () rpc-url))))
                   (usdc (result-value
                          (coalton:coalton
                           (web3/address:address-from-hex
                            "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"))))
                   (owner (result-value
                           (coalton:coalton
                            (web3/address:address-from-hex
                             "0x0000000000000000000000000000000000000001"))))
                   (result (coalton:coalton
                            (web3/permit:permit-nonces
                             (coalton:lisp web3/provider:HttpProvider () provider)
                             (coalton:lisp web3/address:Address () usdc)
                             (coalton:lisp web3/address:Address () owner)))))
              (assert (result-ok-p result))))

          (test-case "permit-domain-separator reads from USDC on mainnet"
            (let* ((provider (coalton:coalton
                              (web3/provider:make-http-provider
                               (coalton:lisp coalton:String () rpc-url))))
                   (usdc (result-value
                          (coalton:coalton
                           (web3/address:address-from-hex
                            "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"))))
                   (result (coalton:coalton
                            (web3/permit:permit-domain-separator
                             (coalton:lisp web3/provider:HttpProvider () provider)
                             (coalton:lisp web3/address:Address () usdc)))))
              (assert (result-ok-p result))
              (assert (= (length (result-value result)) 32)))))
        (format t "~%  Note: Set WEB3_TEST_RPC_URL for permit integration tests~%"))))
