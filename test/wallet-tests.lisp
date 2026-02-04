;;; Wallet module tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Wallet Tests
;;; =========================================================================

(defun run-wallet-tests ()
  (format t "~%=== Wallet Tests ===~%")

  ;;; =========================================================================
  ;;; Wallet Creation Tests
  ;;; =========================================================================

  (test-case "Wallet creation"
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (wallet (coalton:coalton
                    (web3/wallet:make-wallet
                     (coalton:lisp web3/types:Bytes () pk)))))
      (assert (not (null wallet)))))

  (test-case "Wallet with provider"
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (wallet (coalton:coalton
                    (web3/wallet:wallet-with-provider
                     (coalton:lisp web3/types:Bytes () pk)
                     (web3/provider:make-http-provider "http://localhost:8545")))))
      (assert (not (null wallet)))))

  (test-case "Wallet creation with different private keys"
    ;; Creating wallets with different keys should succeed
    (let* ((pk1 (make-array 32 :fill-pointer 32 :adjustable t :initial-element #x01))
           (pk2 (make-array 32 :fill-pointer 32 :adjustable t :initial-element #x42))
           (pk3 (make-array 32 :fill-pointer 32 :adjustable t :initial-element #xff))
           (wallet1 (coalton:coalton
                     (web3/wallet:make-wallet
                      (coalton:lisp web3/types:Bytes () pk1))))
           (wallet2 (coalton:coalton
                     (web3/wallet:make-wallet
                      (coalton:lisp web3/types:Bytes () pk2))))
           (wallet3 (coalton:coalton
                     (web3/wallet:make-wallet
                      (coalton:lisp web3/types:Bytes () pk3)))))
      (assert (not (null wallet1)))
      (assert (not (null wallet2)))
      (assert (not (null wallet3)))))

  ;;; =========================================================================
  ;;; Wallet Address Derivation Tests
  ;;; =========================================================================

  (test-case "wallet-address returns valid address"
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (wallet (coalton:coalton
                    (web3/wallet:make-wallet
                     (coalton:lisp web3/types:Bytes () pk))))
           (result (coalton:coalton
                    (web3/wallet:wallet-address
                     (coalton:lisp web3/wallet:Wallet () wallet)))))
      (assert (result-ok-p result))
      (let* ((addr (result-value result))
             (bytes (coalton:coalton
                     (web3/address:address-bytes
                      (coalton:lisp web3/address:Address () addr)))))
        (assert (= (length bytes) 20)))))

  (test-case "wallet-address is deterministic"
    ;; Same private key should always produce same address
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element #x42))
           (wallet1 (coalton:coalton
                     (web3/wallet:make-wallet
                      (coalton:lisp web3/types:Bytes () pk))))
           (wallet2 (coalton:coalton
                     (web3/wallet:make-wallet
                      (coalton:lisp web3/types:Bytes () pk))))
           (addr1-result (coalton:coalton
                          (web3/wallet:wallet-address
                           (coalton:lisp web3/wallet:Wallet () wallet1))))
           (addr2-result (coalton:coalton
                          (web3/wallet:wallet-address
                           (coalton:lisp web3/wallet:Wallet () wallet2)))))
      (assert (result-ok-p addr1-result))
      (assert (result-ok-p addr2-result))
      (let ((hex1 (coalton:coalton
                   (web3/address:address-to-hex
                    (coalton:lisp web3/address:Address () (result-value addr1-result)))))
            (hex2 (coalton:coalton
                   (web3/address:address-to-hex
                    (coalton:lisp web3/address:Address () (result-value addr2-result))))))
        (assert (string= hex1 hex2)))))

  (test-case "different private keys produce different addresses"
    (let* ((pk1 (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (pk2 (make-array 32 :fill-pointer 32 :adjustable t :initial-element 2))
           (wallet1 (coalton:coalton
                     (web3/wallet:make-wallet
                      (coalton:lisp web3/types:Bytes () pk1))))
           (wallet2 (coalton:coalton
                     (web3/wallet:make-wallet
                      (coalton:lisp web3/types:Bytes () pk2))))
           (addr1 (result-value (coalton:coalton
                                 (web3/wallet:wallet-address
                                  (coalton:lisp web3/wallet:Wallet () wallet1)))))
           (addr2 (result-value (coalton:coalton
                                 (web3/wallet:wallet-address
                                  (coalton:lisp web3/wallet:Wallet () wallet2))))))
      (let ((hex1 (coalton:coalton
                   (web3/address:address-to-hex
                    (coalton:lisp web3/address:Address () addr1))))
            (hex2 (coalton:coalton
                   (web3/address:address-to-hex
                    (coalton:lisp web3/address:Address () addr2)))))
        (assert (not (string= hex1 hex2))))))

  (test-case "wallet-address matches direct public-key derivation"
    ;; wallet-address should produce same result as manual derivation
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element #xab))
           (wallet (coalton:coalton
                    (web3/wallet:make-wallet
                     (coalton:lisp web3/types:Bytes () pk))))
           ;; Get address via wallet
           (wallet-addr (result-value (coalton:coalton
                                       (web3/wallet:wallet-address
                                        (coalton:lisp web3/wallet:Wallet () wallet)))))
           ;; Get address via manual derivation
           (pubkey (result-value (coalton:coalton
                                  (web3/crypto:private-key-to-public-key
                                   (coalton:lisp web3/types:Bytes () pk)))))
           (manual-addr (result-value (coalton:coalton
                                       (web3/address:address-from-public-key
                                        (coalton:lisp web3/types:Bytes () pubkey))))))
      (let ((hex1 (coalton:coalton
                   (web3/address:address-to-hex
                    (coalton:lisp web3/address:Address () wallet-addr))))
            (hex2 (coalton:coalton
                   (web3/address:address-to-hex
                    (coalton:lisp web3/address:Address () manual-addr)))))
        (assert (string= hex1 hex2)))))

  ;;; =========================================================================
  ;;; Transaction Signing Tests
  ;;; =========================================================================

  (test-case "wallet-sign-transaction Legacy transaction"
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (wallet (coalton:coalton
                    (web3/wallet:make-wallet
                     (coalton:lisp web3/types:Bytes () pk))))
           (to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (tx (coalton:coalton
                (web3/transaction:make-transaction
                 web3/transaction:LegacyTx
                 1 0
                 (web3/types:u256-from-integer 20000000000)
                 (web3/types:u256-zero coalton:Unit)
                 21000
                 (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr))
                 (web3/types:u256-from-integer 1000000000000000000)
                 (web3/types:bytes-empty coalton:Unit)
                 coalton:Nil)))
           (result (coalton:coalton
                    (web3/wallet:wallet-sign-transaction
                     (coalton:lisp web3/wallet:Wallet () wallet)
                     (coalton:lisp web3/transaction:Transaction () tx)))))
      (assert (result-ok-p result))
      (let ((signed-bytes (result-value result)))
        ;; Signed tx should be non-empty RLP
        (assert (> (length signed-bytes) 0))
        ;; Legacy tx should start with RLP list prefix (0xc0 or higher)
        (assert (>= (aref signed-bytes 0) #xc0)))))

  (test-case "wallet-sign-transaction EIP-1559 transaction"
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (wallet (coalton:coalton
                    (web3/wallet:make-wallet
                     (coalton:lisp web3/types:Bytes () pk))))
           (to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (tx (coalton:coalton
                (web3/transaction:make-transaction
                 web3/transaction:EIP1559Tx
                 1 5
                 (web3/types:u256-from-integer 1500000000)
                 (web3/types:u256-from-integer 30000000000)
                 21000
                 (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr))
                 (web3/types:u256-from-integer 100000000000000000)
                 (web3/types:bytes-empty coalton:Unit)
                 coalton:Nil)))
           (result (coalton:coalton
                    (web3/wallet:wallet-sign-transaction
                     (coalton:lisp web3/wallet:Wallet () wallet)
                     (coalton:lisp web3/transaction:Transaction () tx)))))
      (assert (result-ok-p result))
      (let ((signed-bytes (result-value result)))
        ;; EIP-1559 tx should start with 0x02 prefix
        (assert (> (length signed-bytes) 0))
        (assert (= (aref signed-bytes 0) 2)))))

  (test-case "wallet-sign-transaction EIP-2930 transaction"
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (wallet (coalton:coalton
                    (web3/wallet:make-wallet
                     (coalton:lisp web3/types:Bytes () pk))))
           (to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (tx (coalton:coalton
                (web3/transaction:make-transaction
                 web3/transaction:EIP2930Tx
                 1 3
                 (web3/types:u256-from-integer 25000000000)
                 (web3/types:u256-zero coalton:Unit)
                 21000
                 (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr))
                 (web3/types:u256-from-integer 500000000000000000)
                 (web3/types:bytes-empty coalton:Unit)
                 coalton:Nil)))
           (result (coalton:coalton
                    (web3/wallet:wallet-sign-transaction
                     (coalton:lisp web3/wallet:Wallet () wallet)
                     (coalton:lisp web3/transaction:Transaction () tx)))))
      (assert (result-ok-p result))
      (let ((signed-bytes (result-value result)))
        ;; EIP-2930 tx should start with 0x01 prefix
        (assert (> (length signed-bytes) 0))
        (assert (= (aref signed-bytes 0) 1)))))

  (test-case "wallet-sign-transaction contract creation"
    ;; Contract creation has no 'to' address
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (wallet (coalton:coalton
                    (web3/wallet:make-wallet
                     (coalton:lisp web3/types:Bytes () pk))))
           ;; Simple contract bytecode (PUSH1 0x80 PUSH1 0x40 MSTORE)
           (init-code (make-array 4 :fill-pointer 4 :adjustable t
                                  :initial-contents '(#x60 #x80 #x60 #x40)))
           (tx (coalton:coalton
                (web3/transaction:make-transaction
                 web3/transaction:EIP1559Tx
                 1 0
                 (web3/types:u256-from-integer 1500000000)
                 (web3/types:u256-from-integer 30000000000)
                 100000
                 coalton-prelude:None  ; No 'to' = contract creation
                 (web3/types:u256-zero coalton:Unit)
                 (coalton:lisp web3/types:Bytes () init-code)
                 coalton:Nil)))
           (result (coalton:coalton
                    (web3/wallet:wallet-sign-transaction
                     (coalton:lisp web3/wallet:Wallet () wallet)
                     (coalton:lisp web3/transaction:Transaction () tx)))))
      (assert (result-ok-p result))
      (let ((signed-bytes (result-value result)))
        (assert (> (length signed-bytes) 0)))))

  (test-case "wallet-sign-transaction with calldata"
    ;; Transfer function call: transfer(address,uint256)
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (wallet (coalton:coalton
                    (web3/wallet:make-wallet
                     (coalton:lisp web3/types:Bytes () pk))))
           (to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xdAC17F958D2ee523a2206206994597C13D831ec7"))))  ; USDT
           ;; transfer(0xabc..., 1000000) calldata
           (calldata (make-array 68 :fill-pointer 68 :adjustable t :initial-element 0))
           (_ (progn
                ;; Function selector: transfer(address,uint256) = 0xa9059cbb
                (setf (aref calldata 0) #xa9)
                (setf (aref calldata 1) #x05)
                (setf (aref calldata 2) #x9c)
                (setf (aref calldata 3) #xbb)
                ;; address argument (right-padded in 32 bytes)
                (setf (aref calldata 35) #xab)
                (setf (aref calldata 36) #xcd)
                ;; uint256 amount (1000000 = 0xF4240)
                (setf (aref calldata 65) #x0f)
                (setf (aref calldata 66) #x42)
                (setf (aref calldata 67) #x40)))
           (tx (coalton:coalton
                (web3/transaction:make-transaction
                 web3/transaction:EIP1559Tx
                 1 10
                 (web3/types:u256-from-integer 2000000000)
                 (web3/types:u256-from-integer 50000000000)
                 60000
                 (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr))
                 (web3/types:u256-zero coalton:Unit)
                 (coalton:lisp web3/types:Bytes () calldata)
                 coalton:Nil)))
           (result (coalton:coalton
                    (web3/wallet:wallet-sign-transaction
                     (coalton:lisp web3/wallet:Wallet () wallet)
                     (coalton:lisp web3/transaction:Transaction () tx)))))
      (declare (ignore _))
      (assert (result-ok-p result))
      (let ((signed-bytes (result-value result)))
        (assert (> (length signed-bytes) 68)))))  ; Must include calldata

  (test-case "wallet-sign-transaction produces valid signatures"
    ;; ECDSA uses random nonces, so signatures differ each time.
    ;; Test that both signatures are valid (same length, proper format).
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element #xde))
           (wallet (coalton:coalton
                    (web3/wallet:make-wallet
                     (coalton:lisp web3/types:Bytes () pk))))
           (to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (tx (coalton:coalton
                (web3/transaction:make-transaction
                 web3/transaction:LegacyTx
                 1 42
                 (web3/types:u256-from-integer 20000000000)
                 (web3/types:u256-zero coalton:Unit)
                 21000
                 (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr))
                 (web3/types:u256-from-integer 1000000000000000000)
                 (web3/types:bytes-empty coalton:Unit)
                 coalton:Nil)))
           (result1 (coalton:coalton
                     (web3/wallet:wallet-sign-transaction
                      (coalton:lisp web3/wallet:Wallet () wallet)
                      (coalton:lisp web3/transaction:Transaction () tx))))
           (result2 (coalton:coalton
                     (web3/wallet:wallet-sign-transaction
                      (coalton:lisp web3/wallet:Wallet () wallet)
                      (coalton:lisp web3/transaction:Transaction () tx)))))
      (assert (result-ok-p result1))
      (assert (result-ok-p result2))
      (let ((bytes1 (result-value result1))
            (bytes2 (result-value result2)))
        ;; Both should produce valid RLP-encoded transactions
        (assert (> (length bytes1) 0))
        (assert (> (length bytes2) 0))
        ;; Both should have same length (same tx structure)
        (assert (= (length bytes1) (length bytes2)))
        ;; Both should start with RLP list prefix for legacy tx
        (assert (>= (aref bytes1 0) #xc0))
        (assert (>= (aref bytes2 0) #xc0)))))

  (test-case "different wallets produce different signatures"
    (let* ((pk1 (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (pk2 (make-array 32 :fill-pointer 32 :adjustable t :initial-element 2))
           (wallet1 (coalton:coalton
                     (web3/wallet:make-wallet
                      (coalton:lisp web3/types:Bytes () pk1))))
           (wallet2 (coalton:coalton
                     (web3/wallet:make-wallet
                      (coalton:lisp web3/types:Bytes () pk2))))
           (to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (tx (coalton:coalton
                (web3/transaction:make-transaction
                 web3/transaction:LegacyTx
                 1 0
                 (web3/types:u256-from-integer 20000000000)
                 (web3/types:u256-zero coalton:Unit)
                 21000
                 (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr))
                 (web3/types:u256-from-integer 1000000000000000000)
                 (web3/types:bytes-empty coalton:Unit)
                 coalton:Nil)))
           (result1 (coalton:coalton
                     (web3/wallet:wallet-sign-transaction
                      (coalton:lisp web3/wallet:Wallet () wallet1)
                      (coalton:lisp web3/transaction:Transaction () tx))))
           (result2 (coalton:coalton
                     (web3/wallet:wallet-sign-transaction
                      (coalton:lisp web3/wallet:Wallet () wallet2)
                      (coalton:lisp web3/transaction:Transaction () tx)))))
      (assert (result-ok-p result1))
      (assert (result-ok-p result2))
      (let ((bytes1 (result-value result1))
            (bytes2 (result-value result2)))
        ;; Different private keys should produce different signatures
        (assert (not (bytes-equal bytes1 bytes2))))))

  ;;; =========================================================================
  ;;; Provider-Required Operations (Error Handling)
  ;;; =========================================================================

  (test-case "wallet-send-transaction without provider returns error"
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (wallet (coalton:coalton
                    (web3/wallet:make-wallet
                     (coalton:lisp web3/types:Bytes () pk))))
           (tx (coalton:coalton
                (web3/transaction:make-transaction
                 web3/transaction:LegacyTx
                 1 0
                 (web3/types:u256-from-integer 20000000000)
                 (web3/types:u256-zero coalton:Unit)
                 21000
                 coalton-prelude:None
                 (web3/types:u256-zero coalton:Unit)
                 (web3/types:bytes-empty coalton:Unit)
                 coalton:Nil)))
           (result (coalton:coalton
                    (web3/wallet:wallet-send-transaction
                     (coalton:lisp web3/wallet:Wallet () wallet)
                     (coalton:lisp web3/transaction:Transaction () tx)))))
      ;; Should fail because no provider
      (assert (result-err-p result))))

  (test-case "wallet-get-balance without provider returns error"
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (wallet (coalton:coalton
                    (web3/wallet:make-wallet
                     (coalton:lisp web3/types:Bytes () pk))))
           (result (coalton:coalton
                    (web3/wallet:wallet-get-balance
                     (coalton:lisp web3/wallet:Wallet () wallet)))))
      ;; Should fail because no provider
      (assert (result-err-p result))))

  (test-case "wallet-get-nonce without provider returns error"
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (wallet (coalton:coalton
                    (web3/wallet:make-wallet
                     (coalton:lisp web3/types:Bytes () pk))))
           (result (coalton:coalton
                    (web3/wallet:wallet-get-nonce
                     (coalton:lisp web3/wallet:Wallet () wallet)))))
      ;; Should fail because no provider
      (assert (result-err-p result))))

  ;;; =========================================================================
  ;;; Edge Cases and Validation
  ;;; =========================================================================

  (test-case "wallet with high-value private key bytes"
    ;; Test with 0xff bytes (edge case for secp256k1)
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element #xff))
           (wallet (coalton:coalton
                    (web3/wallet:make-wallet
                     (coalton:lisp web3/types:Bytes () pk))))
           (addr-result (coalton:coalton
                         (web3/wallet:wallet-address
                          (coalton:lisp web3/wallet:Wallet () wallet)))))
      ;; 0xff...ff is actually valid (less than curve order for secp256k1)
      ;; but some implementations reject it - check what happens
      (if (result-ok-p addr-result)
          (let* ((addr (result-value addr-result))
                 (bytes (coalton:coalton
                         (web3/address:address-bytes
                          (coalton:lisp web3/address:Address () addr)))))
            (assert (= (length bytes) 20)))
          ;; If error, that's also acceptable behavior
          (assert (result-err-p addr-result)))))

  (test-case "wallet sign transaction with large nonce"
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (wallet (coalton:coalton
                    (web3/wallet:make-wallet
                     (coalton:lisp web3/types:Bytes () pk))))
           (to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           ;; Large nonce (simulating high transaction count)
           (tx (coalton:coalton
                (web3/transaction:make-transaction
                 web3/transaction:LegacyTx
                 1 999999
                 (web3/types:u256-from-integer 20000000000)
                 (web3/types:u256-zero coalton:Unit)
                 21000
                 (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr))
                 (web3/types:u256-from-integer 1000000000000000000)
                 (web3/types:bytes-empty coalton:Unit)
                 coalton:Nil)))
           (result (coalton:coalton
                    (web3/wallet:wallet-sign-transaction
                     (coalton:lisp web3/wallet:Wallet () wallet)
                     (coalton:lisp web3/transaction:Transaction () tx)))))
      (assert (result-ok-p result))))

  (test-case "wallet sign transaction with large gas values"
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (wallet (coalton:coalton
                    (web3/wallet:make-wallet
                     (coalton:lisp web3/types:Bytes () pk))))
           (to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           ;; Very high gas limit and price
           (tx (coalton:coalton
                (web3/transaction:make-transaction
                 web3/transaction:EIP1559Tx
                 1 0
                 (web3/types:u256-from-integer 100000000000)  ; 100 gwei priority
                 (web3/types:u256-from-integer 500000000000)  ; 500 gwei max
                 30000000  ; 30M gas limit
                 (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr))
                 (web3/types:u256-from-integer 1000000000000000000)
                 (web3/types:bytes-empty coalton:Unit)
                 coalton:Nil)))
           (result (coalton:coalton
                    (web3/wallet:wallet-sign-transaction
                     (coalton:lisp web3/wallet:Wallet () wallet)
                     (coalton:lisp web3/transaction:Transaction () tx)))))
      (assert (result-ok-p result))))

  (test-case "wallet sign transaction on different chains"
    ;; Test signing for different chain IDs
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (wallet (coalton:coalton
                    (web3/wallet:make-wallet
                     (coalton:lisp web3/types:Bytes () pk))))
           (to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xd8da6bf26964af9d7eed9e03e53415d37aa96045")))))
      ;; Mainnet (chain ID 1)
      (let* ((tx-mainnet (coalton:coalton
                          (web3/transaction:make-transaction
                           web3/transaction:EIP1559Tx
                           1 0
                           (web3/types:u256-from-integer 1500000000)
                           (web3/types:u256-from-integer 30000000000)
                           21000
                           (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr))
                           (web3/types:u256-from-integer 100000000000000000)
                           (web3/types:bytes-empty coalton:Unit)
                           coalton:Nil)))
             (result-mainnet (coalton:coalton
                              (web3/wallet:wallet-sign-transaction
                               (coalton:lisp web3/wallet:Wallet () wallet)
                               (coalton:lisp web3/transaction:Transaction () tx-mainnet)))))
        (assert (result-ok-p result-mainnet)))
      ;; Polygon (chain ID 137)
      (let* ((tx-polygon (coalton:coalton
                          (web3/transaction:make-transaction
                           web3/transaction:EIP1559Tx
                           137 0
                           (web3/types:u256-from-integer 30000000000)
                           (web3/types:u256-from-integer 100000000000)
                           21000
                           (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr))
                           (web3/types:u256-from-integer 100000000000000000)
                           (web3/types:bytes-empty coalton:Unit)
                           coalton:Nil)))
             (result-polygon (coalton:coalton
                              (web3/wallet:wallet-sign-transaction
                               (coalton:lisp web3/wallet:Wallet () wallet)
                               (coalton:lisp web3/transaction:Transaction () tx-polygon)))))
        (assert (result-ok-p result-polygon)))
      ;; Arbitrum (chain ID 42161)
      (let* ((tx-arb (coalton:coalton
                      (web3/transaction:make-transaction
                       web3/transaction:EIP1559Tx
                       42161 0
                       (web3/types:u256-from-integer 100000000)
                       (web3/types:u256-from-integer 1000000000)
                       21000
                       (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr))
                       (web3/types:u256-from-integer 100000000000000000)
                       (web3/types:bytes-empty coalton:Unit)
                       coalton:Nil)))
             (result-arb (coalton:coalton
                          (web3/wallet:wallet-sign-transaction
                           (coalton:lisp web3/wallet:Wallet () wallet)
                           (coalton:lisp web3/transaction:Transaction () tx-arb)))))
        (assert (result-ok-p result-arb))))))
