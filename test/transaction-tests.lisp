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
                 web3/types:u256-zero         ; max-fee (unused for legacy)
                 21000        ; gas-limit
                 (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr)) ; to
                 (web3/types:u256-from-integer 1000000000000000000) ; 1 ether
                 web3/types:bytes-empty ; no data
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
                 web3/types:bytes-empty
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
                 web3/types:u256-zero
                 21000
                 coalton-prelude:None
                 web3/types:u256-zero
                 web3/types:bytes-empty
                 coalton:Nil)))
           (chain-id (coalton:coalton
                      (web3/transaction:.tx-chain-id
                       (coalton:lisp web3/transaction:Transaction () tx))))
           (nonce (coalton:coalton
                   (web3/transaction:.tx-nonce
                    (coalton:lisp web3/transaction:Transaction () tx))))
           (gas-limit (coalton:coalton
                       (web3/transaction:.tx-gas-limit
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
                 web3/types:u256-zero
                 100000
                 coalton-prelude:None  ; No to = contract creation
                 web3/types:u256-zero
                 (web3/types:bytes-from-list (coalton:Cons #x60 (coalton:Cons #x80 coalton:Nil)))
                 coalton:Nil)))
           (encoded (coalton:coalton
                     (web3/transaction:tx-encode-for-signing
                      (coalton:lisp web3/transaction:Transaction () tx)))))
      (assert (> (length encoded) 0))))

  ;;; =========================================================================
  ;;; Transaction Signing Tests
  ;;; =========================================================================

  (test-case "tx-sign produces valid signed transaction bytes"
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (tx (coalton:coalton
                (web3/transaction:make-transaction
                 web3/transaction:LegacyTx
                 1 0
                 (web3/types:u256-from-integer 20000000000)
                 web3/types:u256-zero
                 21000
                 (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr))
                 (web3/types:u256-from-integer 1000000000000000000)
                 web3/types:bytes-empty
                 coalton:Nil)))
           (signed-result (coalton:coalton
                           (web3/transaction:tx-sign
                            (coalton:lisp web3/transaction:Transaction () tx)
                            (coalton:lisp web3/types:Bytes () pk)))))
      (assert (result-ok-p signed-result))
      (let ((signed-bytes (result-value signed-result)))
        ;; Signed legacy tx should be > unsigned and start with RLP list prefix
        (assert (> (length signed-bytes) 0))
        (assert (>= (aref signed-bytes 0) #xc0)))))

  (test-case "tx-sign EIP-1559 transaction"
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
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
                 web3/types:bytes-empty
                 coalton:Nil)))
           (signed-result (coalton:coalton
                           (web3/transaction:tx-sign
                            (coalton:lisp web3/transaction:Transaction () tx)
                            (coalton:lisp web3/types:Bytes () pk)))))
      (assert (result-ok-p signed-result))
      (let ((signed-bytes (result-value signed-result)))
        ;; EIP-1559 signed tx should start with 0x02
        (assert (> (length signed-bytes) 0))
        (assert (= (aref signed-bytes 0) 2)))))

  (test-case "tx-sign rejects invalid private key length"
    (let* ((bad-pk (make-array 31 :fill-pointer 31 :adjustable t :initial-element 1))
           (tx (coalton:coalton
                (web3/transaction:make-transaction
                 web3/transaction:LegacyTx
                 1 0
                 (web3/types:u256-from-integer 20000000000)
                 web3/types:u256-zero
                 21000
                 coalton-prelude:None
                 web3/types:u256-zero
                 web3/types:bytes-empty
                 coalton:Nil)))
           (signed-result (coalton:coalton
                           (web3/transaction:tx-sign
                            (coalton:lisp web3/transaction:Transaction () tx)
                            (coalton:lisp web3/types:Bytes () bad-pk)))))
      (assert (result-err-p signed-result))))

  (test-case "tx-sign different keys produce different signatures"
    (let* ((pk1 (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (pk2 (make-array 32 :fill-pointer 32 :adjustable t :initial-element 2))
           (tx (coalton:coalton
                (web3/transaction:make-transaction
                 web3/transaction:LegacyTx
                 1 0
                 (web3/types:u256-from-integer 20000000000)
                 web3/types:u256-zero
                 21000
                 coalton-prelude:None
                 web3/types:u256-zero
                 web3/types:bytes-empty
                 coalton:Nil)))
           (signed1 (result-value (coalton:coalton
                                   (web3/transaction:tx-sign
                                    (coalton:lisp web3/transaction:Transaction () tx)
                                    (coalton:lisp web3/types:Bytes () pk1)))))
           (signed2 (result-value (coalton:coalton
                                   (web3/transaction:tx-sign
                                    (coalton:lisp web3/transaction:Transaction () tx)
                                    (coalton:lisp web3/types:Bytes () pk2))))))
      (assert (not (bytes-equal signed1 signed2)))))

  ;;; =========================================================================
  ;;; Transaction Decode Tests
  ;;; =========================================================================

  (test-case "tx-decode empty data returns error"
    (let* ((empty (make-array 0 :fill-pointer 0 :adjustable t))
           (result (coalton:coalton
                    (web3/transaction:tx-decode
                     (coalton:lisp web3/types:Bytes () empty)))))
      (assert (result-err-p result))))

  (test-case "tx-decode legacy transaction"
    ;; Sign a legacy tx and decode it back
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (tx (coalton:coalton
                (web3/transaction:make-transaction
                 web3/transaction:LegacyTx
                 1 42
                 (web3/types:u256-from-integer 20000000000)
                 web3/types:u256-zero
                 21000
                 (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr))
                 (web3/types:u256-from-integer 1000000000000000000)
                 web3/types:bytes-empty
                 coalton:Nil)))
           (signed (result-value (coalton:coalton
                                  (web3/transaction:tx-sign
                                   (coalton:lisp web3/transaction:Transaction () tx)
                                   (coalton:lisp web3/types:Bytes () pk)))))
           (decoded-result (coalton:coalton
                            (web3/transaction:tx-decode
                             (coalton:lisp web3/types:Bytes () signed)))))
      (assert (result-ok-p decoded-result))
      (let* ((decoded-tx (result-value decoded-result))
             (decoded-nonce (coalton:coalton
                             (web3/transaction:.tx-nonce
                              (coalton:lisp web3/transaction:Transaction () decoded-tx))))
             (decoded-gas-limit (coalton:coalton
                                 (web3/transaction:.tx-gas-limit
                                  (coalton:lisp web3/transaction:Transaction () decoded-tx)))))
        (assert (= decoded-nonce 42))
        (assert (= decoded-gas-limit 21000)))))

  (test-case "tx-decode EIP-1559 transaction"
    ;; Sign an EIP-1559 tx and decode it back
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (tx (coalton:coalton
                (web3/transaction:make-transaction
                 web3/transaction:EIP1559Tx
                 1 99
                 (web3/types:u256-from-integer 1500000000)
                 (web3/types:u256-from-integer 30000000000)
                 50000
                 (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr))
                 (web3/types:u256-from-integer 500000000000000000)
                 web3/types:bytes-empty
                 coalton:Nil)))
           (signed (result-value (coalton:coalton
                                  (web3/transaction:tx-sign
                                   (coalton:lisp web3/transaction:Transaction () tx)
                                   (coalton:lisp web3/types:Bytes () pk)))))
           (decoded-result (coalton:coalton
                            (web3/transaction:tx-decode
                             (coalton:lisp web3/types:Bytes () signed)))))
      (assert (result-ok-p decoded-result))
      (let* ((decoded-tx (result-value decoded-result))
             (decoded-nonce (coalton:coalton
                             (web3/transaction:.tx-nonce
                              (coalton:lisp web3/transaction:Transaction () decoded-tx))))
             (decoded-gas-limit (coalton:coalton
                                 (web3/transaction:.tx-gas-limit
                                  (coalton:lisp web3/transaction:Transaction () decoded-tx))))
             (decoded-chain-id (coalton:coalton
                                (web3/transaction:.tx-chain-id
                                 (coalton:lisp web3/transaction:Transaction () decoded-tx)))))
        (assert (= decoded-nonce 99))
        (assert (= decoded-gas-limit 50000))
        (assert (= decoded-chain-id 1)))))

  (test-case "tx-decode rejects unknown type prefix"
    ;; A transaction starting with 0x05 is not valid
    (let* ((invalid (make-array 10 :fill-pointer 10 :adjustable t :initial-element 0)))
      (setf (aref invalid 0) 5)  ; Invalid type prefix
      (let ((result (coalton:coalton
                     (web3/transaction:tx-decode
                      (coalton:lisp web3/types:Bytes () invalid)))))
        (assert (result-err-p result)))))

  ;;; =========================================================================
  ;;; EIP-2930 Transaction Tests
  ;;; =========================================================================

  (test-case "EIP-2930 transaction encoding"
    (let* ((to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (tx (coalton:coalton
                (web3/transaction:make-transaction
                 web3/transaction:EIP2930Tx
                 1 10
                 (web3/types:u256-from-integer 25000000000)
                 web3/types:u256-zero
                 30000
                 (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr))
                 (web3/types:u256-from-integer 200000000000000000)
                 web3/types:bytes-empty
                 coalton:Nil)))  ; Empty access list
           (encoded (coalton:coalton
                     (web3/transaction:tx-encode-for-signing
                      (coalton:lisp web3/transaction:Transaction () tx)))))
      ;; EIP-2930 should start with 0x01 prefix
      (assert (> (length encoded) 0))
      (assert (= (aref encoded 0) 1))))

  (test-case "tx-sign EIP-2930 transaction"
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (tx (coalton:coalton
                (web3/transaction:make-transaction
                 web3/transaction:EIP2930Tx
                 1 10
                 (web3/types:u256-from-integer 25000000000)
                 web3/types:u256-zero
                 30000
                 (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr))
                 (web3/types:u256-from-integer 200000000000000000)
                 web3/types:bytes-empty
                 coalton:Nil)))
           (signed-result (coalton:coalton
                           (web3/transaction:tx-sign
                            (coalton:lisp web3/transaction:Transaction () tx)
                            (coalton:lisp web3/types:Bytes () pk)))))
      (assert (result-ok-p signed-result))
      (let ((signed-bytes (result-value signed-result)))
        ;; EIP-2930 signed tx should start with 0x01
        (assert (> (length signed-bytes) 0))
        (assert (= (aref signed-bytes 0) 1)))))

  (test-case "tx-decode EIP-2930 transaction"
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (tx (coalton:coalton
                (web3/transaction:make-transaction
                 web3/transaction:EIP2930Tx
                 1 77
                 (web3/types:u256-from-integer 25000000000)
                 web3/types:u256-zero
                 35000
                 (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr))
                 (web3/types:u256-from-integer 300000000000000000)
                 web3/types:bytes-empty
                 coalton:Nil)))
           (signed (result-value (coalton:coalton
                                  (web3/transaction:tx-sign
                                   (coalton:lisp web3/transaction:Transaction () tx)
                                   (coalton:lisp web3/types:Bytes () pk)))))
           (decoded-result (coalton:coalton
                            (web3/transaction:tx-decode
                             (coalton:lisp web3/types:Bytes () signed)))))
      (assert (result-ok-p decoded-result))
      (let* ((decoded-tx (result-value decoded-result))
             (decoded-nonce (coalton:coalton
                             (web3/transaction:.tx-nonce
                              (coalton:lisp web3/transaction:Transaction () decoded-tx))))
             (decoded-gas-limit (coalton:coalton
                                 (web3/transaction:.tx-gas-limit
                                  (coalton:lisp web3/transaction:Transaction () decoded-tx)))))
        (assert (= decoded-nonce 77))
        (assert (= decoded-gas-limit 35000)))))

  ;;; =========================================================================
  ;;; EIP-4844 Blob Transaction Tests
  ;;; =========================================================================

  (test-case "EIP-4844 blob transaction encoding"
    (let* ((to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           ;; Create a dummy blob versioned hash (32 bytes with 0x01 prefix)
           (blob-hash (make-array 32 :fill-pointer 32 :adjustable t :initial-element 0))
           (_ (setf (aref blob-hash 0) 1))  ; Version byte 0x01
           (tx (coalton:coalton
                (web3/transaction:make-blob-transaction
                 1             ; chain-id
                 15            ; nonce
                 (web3/types:u256-from-integer 2000000000)   ; max priority fee
                 (web3/types:u256-from-integer 40000000000)  ; max fee
                 100000        ; gas-limit
                 (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr))
                 (web3/types:u256-from-integer 0)            ; value
                 web3/types:bytes-empty       ; data
                 coalton:Nil   ; access list
                 (web3/types:u256-from-integer 3000000000)   ; max fee per blob gas
                 (coalton:Cons (coalton:lisp web3/types:Bytes () blob-hash) coalton:Nil))))  ; blob hashes
           (encoded (coalton:coalton
                     (web3/transaction:tx-encode-for-signing
                      (coalton:lisp web3/transaction:Transaction () tx)))))
      ;; EIP-4844 should start with 0x03 prefix
      (assert (> (length encoded) 0))
      (assert (= (aref encoded 0) 3))))

  (test-case "tx-sign EIP-4844 blob transaction"
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (blob-hash (make-array 32 :fill-pointer 32 :adjustable t :initial-element 0))
           (_ (setf (aref blob-hash 0) 1))
           (tx (coalton:coalton
                (web3/transaction:make-blob-transaction
                 1 20
                 (web3/types:u256-from-integer 2000000000)
                 (web3/types:u256-from-integer 40000000000)
                 120000
                 (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr))
                 web3/types:u256-zero
                 web3/types:bytes-empty
                 coalton:Nil
                 (web3/types:u256-from-integer 5000000000)
                 (coalton:Cons (coalton:lisp web3/types:Bytes () blob-hash) coalton:Nil))))
           (signed-result (coalton:coalton
                           (web3/transaction:tx-sign
                            (coalton:lisp web3/transaction:Transaction () tx)
                            (coalton:lisp web3/types:Bytes () pk)))))
      (assert (result-ok-p signed-result))
      (let ((signed-bytes (result-value signed-result)))
        ;; EIP-4844 signed tx should start with 0x03
        (assert (> (length signed-bytes) 0))
        (assert (= (aref signed-bytes 0) 3)))))

  (test-case "tx-decode EIP-4844 blob transaction"
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (blob-hash (make-array 32 :fill-pointer 32 :adjustable t :initial-element 0))
           (_ (setf (aref blob-hash 0) 1))
           (tx (coalton:coalton
                (web3/transaction:make-blob-transaction
                 1 88
                 (web3/types:u256-from-integer 2500000000)
                 (web3/types:u256-from-integer 45000000000)
                 150000
                 (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr))
                 web3/types:u256-zero
                 web3/types:bytes-empty
                 coalton:Nil
                 (web3/types:u256-from-integer 6000000000)
                 (coalton:Cons (coalton:lisp web3/types:Bytes () blob-hash) coalton:Nil))))
           (signed (result-value (coalton:coalton
                                  (web3/transaction:tx-sign
                                   (coalton:lisp web3/transaction:Transaction () tx)
                                   (coalton:lisp web3/types:Bytes () pk)))))
           (decoded-result (coalton:coalton
                            (web3/transaction:tx-decode
                             (coalton:lisp web3/types:Bytes () signed)))))
      (assert (result-ok-p decoded-result))
      (let* ((decoded-tx (result-value decoded-result))
             (decoded-nonce (coalton:coalton
                             (web3/transaction:.tx-nonce
                              (coalton:lisp web3/transaction:Transaction () decoded-tx))))
             (decoded-gas-limit (coalton:coalton
                                 (web3/transaction:.tx-gas-limit
                                  (coalton:lisp web3/transaction:Transaction () decoded-tx))))
             (decoded-chain-id (coalton:coalton
                                (web3/transaction:.tx-chain-id
                                 (coalton:lisp web3/transaction:Transaction () decoded-tx)))))
        (assert (= decoded-nonce 88))
        (assert (= decoded-gas-limit 150000))
        (assert (= decoded-chain-id 1)))))

  (test-case "EIP-4844 blob transaction accessors"
    (let* ((to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (blob-hash1 (make-array 32 :fill-pointer 32 :adjustable t :initial-element 0))
           (blob-hash2 (make-array 32 :fill-pointer 32 :adjustable t :initial-element 0))
           (_ (setf (aref blob-hash1 0) 1))
           (_ (setf (aref blob-hash2 0) 1 (aref blob-hash2 1) 1))
           (tx (coalton:coalton
                (web3/transaction:make-blob-transaction
                 1 50
                 (web3/types:u256-from-integer 1000000000)
                 (web3/types:u256-from-integer 20000000000)
                 80000
                 (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr))
                 web3/types:u256-zero
                 web3/types:bytes-empty
                 coalton:Nil
                 (web3/types:u256-from-integer 4000000000)  ; max fee per blob gas
                 (coalton:Cons (coalton:lisp web3/types:Bytes () blob-hash1)
                               (coalton:Cons (coalton:lisp web3/types:Bytes () blob-hash2)
                                             coalton:Nil))))))
      ;; Test blob-specific accessors
      (let* ((blob-hashes (coalton:coalton
                           (web3/transaction:.tx-blob-versioned-hashes
                            (coalton:lisp web3/transaction:Transaction () tx))))
             (hash-count (coalton:coalton
                          (coalton-library/list:length
                           (coalton:lisp (coalton:List web3/types:Bytes) () blob-hashes)))))
        (assert (= hash-count 2)))))

  (test-case "EIP-4844 with multiple blob hashes encode/decode roundtrip"
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           ;; Create 3 blob versioned hashes
           (blob-hash1 (make-array 32 :fill-pointer 32 :adjustable t :initial-element 0))
           (blob-hash2 (make-array 32 :fill-pointer 32 :adjustable t :initial-element 0))
           (blob-hash3 (make-array 32 :fill-pointer 32 :adjustable t :initial-element 0))
           (_ (setf (aref blob-hash1 0) 1))
           (_ (setf (aref blob-hash2 0) 1 (aref blob-hash2 31) #xAB))
           (_ (setf (aref blob-hash3 0) 1 (aref blob-hash3 31) #xCD))
           (tx (coalton:coalton
                (web3/transaction:make-blob-transaction
                 1 100
                 (web3/types:u256-from-integer 3000000000)
                 (web3/types:u256-from-integer 50000000000)
                 200000
                 (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr))
                 web3/types:u256-zero
                 web3/types:bytes-empty
                 coalton:Nil
                 (web3/types:u256-from-integer 7000000000)
                 (coalton:Cons (coalton:lisp web3/types:Bytes () blob-hash1)
                               (coalton:Cons (coalton:lisp web3/types:Bytes () blob-hash2)
                                             (coalton:Cons (coalton:lisp web3/types:Bytes () blob-hash3)
                                                           coalton:Nil))))))
           (signed (result-value (coalton:coalton
                                  (web3/transaction:tx-sign
                                   (coalton:lisp web3/transaction:Transaction () tx)
                                   (coalton:lisp web3/types:Bytes () pk)))))
           (decoded-result (coalton:coalton
                            (web3/transaction:tx-decode
                             (coalton:lisp web3/types:Bytes () signed)))))
      (assert (result-ok-p decoded-result))
      (let* ((decoded-tx (result-value decoded-result))
             (decoded-hashes (coalton:coalton
                              (web3/transaction:.tx-blob-versioned-hashes
                               (coalton:lisp web3/transaction:Transaction () decoded-tx))))
             (hash-count (coalton:coalton
                          (coalton-library/list:length
                           (coalton:lisp (coalton:List web3/types:Bytes) () decoded-hashes)))))
        ;; Should have decoded 3 blob hashes
        (assert (= hash-count 3))))))
