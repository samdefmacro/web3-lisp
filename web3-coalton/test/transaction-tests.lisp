;;; Transaction module tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Transaction Tests
;;; =========================================================================

(defun run-transaction-tests ()
  (format t "~%=== Transaction Tests ===~%")

  (test-case "Legacy transaction encoding"
    ;; Create a simple legacy transaction
    (let* ((to-result (coalton:coalton
                       (web3/address:address-from-hex
                        "0xd8da6bf26964af9d7eed9e03e53415d37aa96045")))
           (to-addr (result-value to-result))
           (tx (coalton:coalton
                (web3/transaction:make-transaction
                 web3/transaction:LegacyTx
                 1            ; chain-id (mainnet)
                 0            ; nonce
                 (web3/types:u256-from-integer 20000000000) ; gas-price 20 gwei
                 (web3/types:u256-zero coalton:Unit)         ; max-fee (unused for legacy)
                 21000        ; gas-limit
                 (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr)) ; to
                 (web3/types:u256-from-integer 1000000000000000000) ; 1 ether
                 (web3/types:bytes-empty coalton:Unit) ; no data
                 coalton:Nil))) ; no access list
           (encoded (coalton:coalton
                     (web3/transaction:tx-encode-for-signing
                      (coalton:lisp web3/transaction:Transaction () tx)))))
      ;; Should produce valid RLP encoding
      (assert (> (length encoded) 0))
      ;; First byte should be RLP list prefix
      (assert (>= (aref encoded 0) #xc0))))

  (test-case "EIP-1559 transaction encoding"
    (let* ((to-result (coalton:coalton
                       (web3/address:address-from-hex
                        "0xd8da6bf26964af9d7eed9e03e53415d37aa96045")))
           (to-addr (result-value to-result))
           (tx (coalton:coalton
                (web3/transaction:make-transaction
                 web3/transaction:EIP1559Tx
                 1             ; chain-id
                 5             ; nonce
                 (web3/types:u256-from-integer 1500000000) ; max priority fee
                 (web3/types:u256-from-integer 30000000000) ; max fee
                 21000         ; gas-limit
                 (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr))
                 (web3/types:u256-from-integer 100000000000000000) ; 0.1 ether
                 (web3/types:bytes-empty coalton:Unit)
                 coalton:Nil)))
           (encoded (coalton:coalton
                     (web3/transaction:tx-encode-for-signing
                      (coalton:lisp web3/transaction:Transaction () tx)))))
      ;; EIP-1559 should start with 0x02 prefix
      (assert (> (length encoded) 0))
      (assert (= (aref encoded 0) 2))))

  (test-case "Transaction type accessors"
    (let* ((tx (coalton:coalton
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
           (chain-id (coalton:coalton
                      (web3/transaction:tx-chain-id
                       (coalton:lisp web3/transaction:Transaction () tx))))
           (nonce (coalton:coalton
                   (web3/transaction:tx-nonce
                    (coalton:lisp web3/transaction:Transaction () tx))))
           (gas-limit (coalton:coalton
                       (web3/transaction:tx-gas-limit
                        (coalton:lisp web3/transaction:Transaction () tx)))))
      (assert (= chain-id 1))
      (assert (= nonce 0))
      (assert (= gas-limit 21000))))

  (test-case "Contract creation (no to address)"
    (let* ((tx (coalton:coalton
                (web3/transaction:make-transaction
                 web3/transaction:LegacyTx
                 1 0
                 (web3/types:u256-from-integer 20000000000)
                 (web3/types:u256-zero coalton:Unit)
                 100000
                 coalton-prelude:None  ; No to = contract creation
                 (web3/types:u256-zero coalton:Unit)
                 (web3/types:bytes-from-list (coalton:Cons #x60 (coalton:Cons #x80 coalton:Nil)))
                 coalton:Nil)))
           (encoded (coalton:coalton
                     (web3/transaction:tx-encode-for-signing
                      (coalton:lisp web3/transaction:Transaction () tx)))))
      (assert (> (length encoded) 0)))))
