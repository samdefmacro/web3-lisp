;;; Address module tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Address Tests
;;; =========================================================================

(defun run-address-tests ()
  (format t "~%=== Address Tests ===~%")

  (test-case "address from hex"
    (assert (eq (web3-tests:test-address-from-hex coalton:Unit) coalton:True)))

  (test-case "address EIP-55 checksum"
    (assert (eq (web3-tests:test-address-checksum coalton:Unit) coalton:True)))

  (test-case "zero address"
    (let ((zero-addr (coalton:coalton (web3/address:address-zero coalton:Unit))))
      (let ((bytes (coalton:coalton
                    (web3/address:address-bytes
                     (web3/address:address-zero coalton:Unit)))))
        (assert (= (length bytes) 20))
        (assert (every #'zerop (coerce bytes 'list))))))

  (test-case "address to hex and back"
    (let* ((addr-result (coalton:coalton
                         (web3/address:address-from-hex
                          "0xd8da6bf26964af9d7eed9e03e53415d37aa96045")))
           (addr (result-value addr-result))
           (hex (coalton:coalton
                 (web3/address:address-to-hex
                  (coalton:lisp web3/address:Address () addr)))))
      (assert (string= hex "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))

  ;; EIP-55 test vectors from the EIP
  (test-case "EIP-55 all caps"
    (let* ((addr-result (coalton:coalton
                         (web3/address:address-from-hex
                          "0x52908400098527886E0F7030069857D2E4169EE7")))
           (addr (result-value addr-result))
           (checksummed (coalton:coalton
                         (web3/address:address-to-checksum-hex
                          (coalton:lisp web3/address:Address () addr)))))
      (assert (string= checksummed "0x52908400098527886E0F7030069857D2E4169EE7"))))

  (test-case "EIP-55 all lower"
    (let* ((addr-result (coalton:coalton
                         (web3/address:address-from-hex
                          "0xde709f2102306220921060314715629080e2fb77")))
           (addr (result-value addr-result))
           (checksummed (coalton:coalton
                         (web3/address:address-to-checksum-hex
                          (coalton:lisp web3/address:Address () addr)))))
      (assert (string= checksummed "0xde709f2102306220921060314715629080e2fb77"))))

  (test-case "address from invalid hex (wrong length)"
    (let ((result (coalton:coalton (web3/address:address-from-hex "0xdeadbeef"))))
      ;; Should fail - only 4 bytes, not 20
      (assert (result-err-p result))))

  ;;; =========================================================================
  ;;; Address from Public Key Tests
  ;;; =========================================================================

  (test-case "address-from-public-key with uncompressed key"
    ;; Use a known private key, derive public key, then address
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (pubkey-result (coalton:coalton
                           (web3/crypto:private-key-to-public-key
                            (coalton:lisp web3/types:Bytes () pk))))
           (pubkey (result-value pubkey-result))
           (addr-result (coalton:coalton
                         (web3/address:address-from-public-key
                          (coalton:lisp web3/types:Bytes () pubkey)))))
      (assert (result-ok-p addr-result))
      (let* ((addr (result-value addr-result))
             (bytes (coalton:coalton
                     (web3/address:address-bytes
                      (coalton:lisp web3/address:Address () addr)))))
        (assert (= (length bytes) 20)))))

  (test-case "address-from-public-key with 64-byte raw key"
    ;; 64 bytes without 04 prefix
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (pubkey-result (coalton:coalton
                           (web3/crypto:private-key-to-public-key
                            (coalton:lisp web3/types:Bytes () pk))))
           (pubkey (result-value pubkey-result))
           ;; Extract just the 64 bytes (skip the 04 prefix)
           (raw-64 (make-array 64 :fill-pointer 64 :adjustable t)))
      (loop :for i :from 0 :below 64
            :do (setf (aref raw-64 i) (aref pubkey (1+ i))))
      (let ((addr-result (coalton:coalton
                          (web3/address:address-from-public-key
                           (coalton:lisp web3/types:Bytes () raw-64)))))
        (assert (result-ok-p addr-result)))))

  (test-case "address-from-public-key deterministic"
    ;; Same public key should produce same address
    (let* ((pk (make-array 32 :fill-pointer 32 :adjustable t :initial-element #x42))
           (pubkey (result-value (coalton:coalton
                                  (web3/crypto:private-key-to-public-key
                                   (coalton:lisp web3/types:Bytes () pk)))))
           (addr1 (result-value (coalton:coalton
                                 (web3/address:address-from-public-key
                                  (coalton:lisp web3/types:Bytes () pubkey)))))
           (addr2 (result-value (coalton:coalton
                                 (web3/address:address-from-public-key
                                  (coalton:lisp web3/types:Bytes () pubkey)))))
           (hex1 (coalton:coalton
                  (web3/address:address-to-hex
                   (coalton:lisp web3/address:Address () addr1))))
           (hex2 (coalton:coalton
                  (web3/address:address-to-hex
                   (coalton:lisp web3/address:Address () addr2)))))
      (assert (string= hex1 hex2))))

  (test-case "address-from-public-key rejects invalid key"
    (let* ((invalid (make-array 50 :fill-pointer 50 :adjustable t :initial-element 0))
           (result (coalton:coalton
                    (web3/address:address-from-public-key
                     (coalton:lisp web3/types:Bytes () invalid)))))
      (assert (result-err-p result))))

  ;;; =========================================================================
  ;;; Contract Address Computation Tests (CREATE)
  ;;; =========================================================================

  (test-case "compute-contract-address with nonce 0"
    ;; Known test vector: deployer 0x6ac7ea33f8831ea9dcc53393aaa88b25a785dbf0
    ;; nonce 0 -> contract address 0xcd234a471b72ba2f1ccf0a70fcaba648a5eecd8d
    (let* ((deployer-result (coalton:coalton
                             (web3/address:address-from-hex
                              "0x6ac7ea33f8831ea9dcc53393aaa88b25a785dbf0")))
           (deployer (result-value deployer-result))
           (contract-addr (coalton:coalton
                           (web3/address:compute-contract-address
                            (coalton:lisp web3/address:Address () deployer)
                            0)))
           (contract-hex (coalton:coalton
                          (web3/address:address-to-hex
                           (coalton:lisp web3/address:Address () contract-addr)))))
      (assert (string= contract-hex "0xcd234a471b72ba2f1ccf0a70fcaba648a5eecd8d"))))

  (test-case "compute-contract-address with nonce 1"
    ;; Same deployer, nonce 1 -> different address
    (let* ((deployer-result (coalton:coalton
                             (web3/address:address-from-hex
                              "0x6ac7ea33f8831ea9dcc53393aaa88b25a785dbf0")))
           (deployer (result-value deployer-result))
           (addr0 (coalton:coalton
                   (web3/address:compute-contract-address
                    (coalton:lisp web3/address:Address () deployer)
                    0)))
           (addr1 (coalton:coalton
                   (web3/address:compute-contract-address
                    (coalton:lisp web3/address:Address () deployer)
                    1)))
           (hex0 (coalton:coalton
                  (web3/address:address-to-hex
                   (coalton:lisp web3/address:Address () addr0))))
           (hex1 (coalton:coalton
                  (web3/address:address-to-hex
                   (coalton:lisp web3/address:Address () addr1)))))
      ;; Different nonces should produce different addresses
      (assert (not (string= hex0 hex1)))))

  (test-case "compute-contract-address different deployers"
    ;; Different deployers with same nonce -> different addresses
    (let* ((deployer1 (result-value (coalton:coalton
                                     (web3/address:address-from-hex
                                      "0x0000000000000000000000000000000000000001"))))
           (deployer2 (result-value (coalton:coalton
                                     (web3/address:address-from-hex
                                      "0x0000000000000000000000000000000000000002"))))
           (addr1 (coalton:coalton
                   (web3/address:compute-contract-address
                    (coalton:lisp web3/address:Address () deployer1)
                    0)))
           (addr2 (coalton:coalton
                   (web3/address:compute-contract-address
                    (coalton:lisp web3/address:Address () deployer2)
                    0)))
           (hex1 (coalton:coalton
                  (web3/address:address-to-hex
                   (coalton:lisp web3/address:Address () addr1))))
           (hex2 (coalton:coalton
                  (web3/address:address-to-hex
                   (coalton:lisp web3/address:Address () addr2)))))
      (assert (not (string= hex1 hex2)))))

  ;;; =========================================================================
  ;;; Contract Address Computation Tests (CREATE2)
  ;;; =========================================================================

  (test-case "compute-create2-address basic"
    ;; CREATE2 address = keccak256(0xff ++ sender ++ salt ++ keccak256(init_code))[12:]
    (let* ((deployer (result-value (coalton:coalton
                                    (web3/address:address-from-hex
                                     "0x0000000000000000000000000000000000000000"))))
           (salt (make-array 32 :fill-pointer 32 :adjustable t :initial-element 0))
           ;; keccak256 of empty init code
           (init-code-hash (coalton:coalton
                            (web3/crypto:keccak256
                             (web3/types:bytes-empty coalton:Unit))))
           (addr (coalton:coalton
                  (web3/address:compute-create2-address
                   (coalton:lisp web3/address:Address () deployer)
                   (coalton:lisp web3/types:Bytes () salt)
                   (coalton:lisp web3/types:Bytes () init-code-hash))))
           (hex (coalton:coalton
                 (web3/address:address-to-hex
                  (coalton:lisp web3/address:Address () addr)))))
      ;; Should produce a valid 20-byte address
      (assert (= (length hex) 42))  ; "0x" + 40 hex chars
      (assert (string= (subseq hex 0 2) "0x"))))

  (test-case "compute-create2-address different salts"
    ;; Different salts should produce different addresses
    (let* ((deployer (result-value (coalton:coalton
                                    (web3/address:address-from-hex
                                     "0x1234567890123456789012345678901234567890"))))
           (salt1 (make-array 32 :fill-pointer 32 :adjustable t :initial-element 0))
           (salt2 (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (init-code-hash (coalton:coalton
                            (web3/crypto:keccak256
                             (web3/types:bytes-from-list
                              (coalton:Cons #x60 (coalton:Cons #x80 coalton:Nil))))))
           (addr1 (coalton:coalton
                   (web3/address:compute-create2-address
                    (coalton:lisp web3/address:Address () deployer)
                    (coalton:lisp web3/types:Bytes () salt1)
                    (coalton:lisp web3/types:Bytes () init-code-hash))))
           (addr2 (coalton:coalton
                   (web3/address:compute-create2-address
                    (coalton:lisp web3/address:Address () deployer)
                    (coalton:lisp web3/types:Bytes () salt2)
                    (coalton:lisp web3/types:Bytes () init-code-hash))))
           (hex1 (coalton:coalton
                  (web3/address:address-to-hex
                   (coalton:lisp web3/address:Address () addr1))))
           (hex2 (coalton:coalton
                  (web3/address:address-to-hex
                   (coalton:lisp web3/address:Address () addr2)))))
      (assert (not (string= hex1 hex2)))))

  (test-case "compute-create2-address different init codes"
    ;; Different init codes should produce different addresses
    (let* ((deployer (result-value (coalton:coalton
                                    (web3/address:address-from-hex
                                     "0x1234567890123456789012345678901234567890"))))
           (salt (make-array 32 :fill-pointer 32 :adjustable t :initial-element 0))
           (init-code-hash1 (coalton:coalton
                             (web3/crypto:keccak256
                              (web3/types:bytes-from-list
                               (coalton:Cons #x60 coalton:Nil)))))
           (init-code-hash2 (coalton:coalton
                             (web3/crypto:keccak256
                              (web3/types:bytes-from-list
                               (coalton:Cons #x61 coalton:Nil)))))
           (addr1 (coalton:coalton
                   (web3/address:compute-create2-address
                    (coalton:lisp web3/address:Address () deployer)
                    (coalton:lisp web3/types:Bytes () salt)
                    (coalton:lisp web3/types:Bytes () init-code-hash1))))
           (addr2 (coalton:coalton
                   (web3/address:compute-create2-address
                    (coalton:lisp web3/address:Address () deployer)
                    (coalton:lisp web3/types:Bytes () salt)
                    (coalton:lisp web3/types:Bytes () init-code-hash2))))
           (hex1 (coalton:coalton
                  (web3/address:address-to-hex
                   (coalton:lisp web3/address:Address () addr1))))
           (hex2 (coalton:coalton
                  (web3/address:address-to-hex
                   (coalton:lisp web3/address:Address () addr2)))))
      (assert (not (string= hex1 hex2))))))
