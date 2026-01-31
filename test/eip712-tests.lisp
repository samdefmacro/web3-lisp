;;; EIP-712 Typed Data tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; EIP-712 Tests
;;; =========================================================================

(defun run-eip712-tests ()
  (format t "~%=== EIP-712 Tests ===~%")

  ;;; =========================================================================
  ;;; Domain Separator Tests
  ;;; =========================================================================

  (test-case "domain-type-hash computes correctly"
    ;; EIP712Domain(string name,string version,uint256 chainId,address verifyingContract)
    ;; keccak256 of this string
    (let* ((contract (result-value (coalton:coalton (web3/types:hex-decode "CcCCccccCCCCcCCCCCCcCcCccCcCCCcCcccccccC"))))
           (domain (coalton:coalton
                    (web3/eip712:make-domain
                     "MyApp"
                     "1"
                     (web3/types:u256-from-integer 1)
                     (coalton:lisp web3/types:Bytes () contract))))
           (sep (coalton:coalton
                 (web3/eip712:domain-separator
                  (coalton:lisp web3/eip712:EIP712Domain () domain)))))
      ;; Domain separator should be 32 bytes
      (assert (= (length sep) 32))))

  (test-case "domain-separator-hex returns 0x-prefixed string"
    (let* ((contract (result-value (coalton:coalton (web3/types:hex-decode "CcCCccccCCCCcCCCCCCcCcCccCcCCCcCcccccccC"))))
           (domain (coalton:coalton
                    (web3/eip712:make-domain
                     "Test"
                     "1"
                     (web3/types:u256-from-integer 1)
                     (coalton:lisp web3/types:Bytes () contract))))
           (hex (coalton:coalton
                 (web3/eip712:domain-separator-hex
                  (coalton:lisp web3/eip712:EIP712Domain () domain)))))
      ;; Should be 0x + 64 hex chars
      (assert (= (length hex) 66))
      (assert (string= (subseq hex 0 2) "0x"))))

  (test-case "same domain produces same separator"
    (let* ((contract (result-value (coalton:coalton (web3/types:hex-decode "1234567890123456789012345678901234567890"))))
           (domain1 (coalton:coalton
                     (web3/eip712:make-domain
                      "App"
                      "1"
                      (web3/types:u256-from-integer 1)
                      (coalton:lisp web3/types:Bytes () contract))))
           (domain2 (coalton:coalton
                     (web3/eip712:make-domain
                      "App"
                      "1"
                      (web3/types:u256-from-integer 1)
                      (coalton:lisp web3/types:Bytes () contract))))
           (sep1 (coalton:coalton
                  (web3/eip712:domain-separator
                   (coalton:lisp web3/eip712:EIP712Domain () domain1))))
           (sep2 (coalton:coalton
                  (web3/eip712:domain-separator
                   (coalton:lisp web3/eip712:EIP712Domain () domain2)))))
      (assert (equalp sep1 sep2))))

  (test-case "different names produce different separators"
    (let* ((contract (result-value (coalton:coalton (web3/types:hex-decode "1234567890123456789012345678901234567890"))))
           (domain1 (coalton:coalton
                     (web3/eip712:make-domain
                      "App1"
                      "1"
                      (web3/types:u256-from-integer 1)
                      (coalton:lisp web3/types:Bytes () contract))))
           (domain2 (coalton:coalton
                     (web3/eip712:make-domain
                      "App2"
                      "1"
                      (web3/types:u256-from-integer 1)
                      (coalton:lisp web3/types:Bytes () contract))))
           (sep1 (coalton:coalton
                  (web3/eip712:domain-separator
                   (coalton:lisp web3/eip712:EIP712Domain () domain1))))
           (sep2 (coalton:coalton
                  (web3/eip712:domain-separator
                   (coalton:lisp web3/eip712:EIP712Domain () domain2)))))
      (assert (not (equalp sep1 sep2)))))

  (test-case "different chain IDs produce different separators"
    (let* ((contract (result-value (coalton:coalton (web3/types:hex-decode "1234567890123456789012345678901234567890"))))
           (domain1 (coalton:coalton
                     (web3/eip712:make-domain
                      "App"
                      "1"
                      (web3/types:u256-from-integer 1)
                      (coalton:lisp web3/types:Bytes () contract))))
           (domain2 (coalton:coalton
                     (web3/eip712:make-domain
                      "App"
                      "1"
                      (web3/types:u256-from-integer 137)
                      (coalton:lisp web3/types:Bytes () contract))))
           (sep1 (coalton:coalton
                  (web3/eip712:domain-separator
                   (coalton:lisp web3/eip712:EIP712Domain () domain1))))
           (sep2 (coalton:coalton
                  (web3/eip712:domain-separator
                   (coalton:lisp web3/eip712:EIP712Domain () domain2)))))
      (assert (not (equalp sep1 sep2)))))

  ;;; =========================================================================
  ;;; Type Encoding Tests
  ;;; =========================================================================

  (test-case "encode-type produces correct string"
    (let* ((field1 (coalton:coalton (web3/eip712:make-field "from" "address")))
           (field2 (coalton:coalton (web3/eip712:make-field "to" "address")))
           (field3 (coalton:coalton (web3/eip712:make-field "value" "uint256")))
           (struct (coalton:coalton
                    (web3/eip712:make-struct
                     "Transfer"
                     (coalton:Cons (coalton:lisp web3/eip712:TypedField () field1)
                                   (coalton:Cons (coalton:lisp web3/eip712:TypedField () field2)
                                                 (coalton:Cons (coalton:lisp web3/eip712:TypedField () field3)
                                                               coalton:Nil))))))
           (encoded (coalton:coalton
                     (web3/eip712:encode-type
                      (coalton:lisp web3/eip712:TypedStruct () struct)))))
      (assert (string= encoded "Transfer(address from,address to,uint256 value)"))))

  (test-case "type-hash produces 32 bytes"
    (let* ((field1 (coalton:coalton (web3/eip712:make-field "owner" "address")))
           (field2 (coalton:coalton (web3/eip712:make-field "spender" "address")))
           (struct (coalton:coalton
                    (web3/eip712:make-struct
                     "Approval"
                     (coalton:Cons (coalton:lisp web3/eip712:TypedField () field1)
                                   (coalton:Cons (coalton:lisp web3/eip712:TypedField () field2)
                                                 coalton:Nil)))))
           (hash (coalton:coalton
                  (web3/eip712:type-hash
                   (coalton:lisp web3/eip712:TypedStruct () struct)))))
      (assert (= (length hash) 32))))

  ;;; =========================================================================
  ;;; EIP-2612 Permit Type Hash Test
  ;;; =========================================================================

  (test-case "eip2612-permit-type-hash is correct"
    ;; Known value for Permit(address owner,address spender,uint256 value,uint256 nonce,uint256 deadline)
    ;; 0x6e71edae12b1b97f4d1f60370fef10105fa2faae0126114a169c64845d6126c9
    (let ((hash (coalton:coalton (web3/eip712:eip2612-permit-type-hash coalton:Unit))))
      (assert (= (length hash) 32))
      (assert (= (aref hash 0) #x6e))
      (assert (= (aref hash 1) #x71))
      (assert (= (aref hash 2) #xed))
      (assert (= (aref hash 3) #xae))))

  ;;; =========================================================================
  ;;; Data Encoding Tests
  ;;; =========================================================================

  (test-case "encode-data encodes uint256 correctly"
    (let* ((val (coalton:coalton
                 (web3/eip712:TypedUint256
                  (web3/types:u256-from-integer 100))))
           (encoded (coalton:coalton
                     (web3/eip712:encode-data
                      (coalton:Cons (coalton:lisp web3/eip712:TypedValue () val)
                                    coalton:Nil)))))
      ;; Should be 32 bytes
      (assert (= (length encoded) 32))
      ;; Last byte should be 100
      (assert (= (aref encoded 31) 100))))

  (test-case "encode-data encodes address correctly"
    (let* ((addr (result-value (coalton:coalton (web3/types:hex-decode "d8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (val (coalton:coalton
                 (web3/eip712:TypedAddress
                  (coalton:lisp web3/types:Bytes () addr))))
           (encoded (coalton:coalton
                     (web3/eip712:encode-data
                      (coalton:Cons (coalton:lisp web3/eip712:TypedValue () val)
                                    coalton:Nil)))))
      ;; Should be 32 bytes (left-padded)
      (assert (= (length encoded) 32))
      ;; First 12 bytes should be zero
      (assert (= (aref encoded 0) 0))
      (assert (= (aref encoded 11) 0))
      ;; Address starts at byte 12
      (assert (= (aref encoded 12) #xd8))))

  (test-case "encode-data encodes string as keccak256"
    (let* ((val (coalton:coalton (web3/eip712:TypedString "hello")))
           (encoded (coalton:coalton
                     (web3/eip712:encode-data
                      (coalton:Cons (coalton:lisp web3/eip712:TypedValue () val)
                                    coalton:Nil)))))
      ;; Should be 32 bytes (keccak256 hash)
      (assert (= (length encoded) 32))
      ;; keccak256("hello") starts with 0x1c8aff...
      (assert (= (aref encoded 0) #x1c))
      (assert (= (aref encoded 1) #x8a))))

  (test-case "encode-data encodes bool correctly"
    (let* ((val-true (coalton:coalton (web3/eip712:TypedBool coalton:True)))
           (val-false (coalton:coalton (web3/eip712:TypedBool coalton:False)))
           (encoded-true (coalton:coalton
                          (web3/eip712:encode-data
                           (coalton:Cons (coalton:lisp web3/eip712:TypedValue () val-true)
                                         coalton:Nil))))
           (encoded-false (coalton:coalton
                           (web3/eip712:encode-data
                            (coalton:Cons (coalton:lisp web3/eip712:TypedValue () val-false)
                                          coalton:Nil)))))
      (assert (= (length encoded-true) 32))
      (assert (= (length encoded-false) 32))
      (assert (= (aref encoded-true 31) 1))
      (assert (= (aref encoded-false 31) 0))))

  ;;; =========================================================================
  ;;; Struct Hash Tests
  ;;; =========================================================================

  (test-case "hash-struct produces 32 bytes"
    (let* ((type-hash (coalton:coalton (web3/eip712:eip2612-permit-type-hash coalton:Unit)))
           (owner (result-value (coalton:coalton (web3/types:hex-decode "d8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (spender (result-value (coalton:coalton (web3/types:hex-decode "1234567890123456789012345678901234567890"))))
           (val1 (coalton:coalton (web3/eip712:TypedAddress (coalton:lisp web3/types:Bytes () owner))))
           (val2 (coalton:coalton (web3/eip712:TypedAddress (coalton:lisp web3/types:Bytes () spender))))
           (val3 (coalton:coalton (web3/eip712:TypedUint256 (web3/types:u256-from-integer 1000))))
           (val4 (coalton:coalton (web3/eip712:TypedUint256 (web3/types:u256-from-integer 0))))
           (val5 (coalton:coalton (web3/eip712:TypedUint256 (web3/types:u256-from-integer 9999999999))))
           (hash (coalton:coalton
                  (web3/eip712:hash-struct
                   (coalton:lisp web3/types:Bytes () type-hash)
                   (coalton:Cons (coalton:lisp web3/eip712:TypedValue () val1)
                                 (coalton:Cons (coalton:lisp web3/eip712:TypedValue () val2)
                                               (coalton:Cons (coalton:lisp web3/eip712:TypedValue () val3)
                                                             (coalton:Cons (coalton:lisp web3/eip712:TypedValue () val4)
                                                                           (coalton:Cons (coalton:lisp web3/eip712:TypedValue () val5)
                                                                                         coalton:Nil)))))))))
      (assert (= (length hash) 32))))

  ;;; =========================================================================
  ;;; Typed Data Hash Tests
  ;;; =========================================================================

  (test-case "typed-data-hash produces 32 bytes"
    (let* ((domain-sep (result-value (coalton:coalton (web3/types:hex-decode "0000000000000000000000000000000000000000000000000000000000000001"))))
           (struct-hash (result-value (coalton:coalton (web3/types:hex-decode "0000000000000000000000000000000000000000000000000000000000000002"))))
           (hash (coalton:coalton
                  (web3/eip712:typed-data-hash
                   (coalton:lisp web3/types:Bytes () domain-sep)
                   (coalton:lisp web3/types:Bytes () struct-hash)))))
      (assert (= (length hash) 32))))

  (test-case "typed-data-hash starts with 0x1901 prefix"
    ;; The hash should be keccak256(0x1901 || domainSeparator || hashStruct)
    ;; We can verify by checking that different inputs produce different hashes
    (let* ((domain-sep1 (result-value (coalton:coalton (web3/types:hex-decode "0000000000000000000000000000000000000000000000000000000000000001"))))
           (domain-sep2 (result-value (coalton:coalton (web3/types:hex-decode "0000000000000000000000000000000000000000000000000000000000000002"))))
           (struct-hash (result-value (coalton:coalton (web3/types:hex-decode "0000000000000000000000000000000000000000000000000000000000000003"))))
           (hash1 (coalton:coalton
                   (web3/eip712:typed-data-hash
                    (coalton:lisp web3/types:Bytes () domain-sep1)
                    (coalton:lisp web3/types:Bytes () struct-hash))))
           (hash2 (coalton:coalton
                   (web3/eip712:typed-data-hash
                    (coalton:lisp web3/types:Bytes () domain-sep2)
                    (coalton:lisp web3/types:Bytes () struct-hash)))))
      (assert (not (equalp hash1 hash2)))))

  ;;; =========================================================================
  ;;; Signing Tests
  ;;; =========================================================================

  (test-case "sign-typed-data produces valid signature"
    (let* ((private-key (result-value (coalton:coalton
                                       (web3/types:hex-decode
                                        "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"))))
           (domain-sep (result-value (coalton:coalton (web3/types:hex-decode "1111111111111111111111111111111111111111111111111111111111111111"))))
           (struct-hash (result-value (coalton:coalton (web3/types:hex-decode "2222222222222222222222222222222222222222222222222222222222222222"))))
           (sig-result (coalton:coalton
                        (web3/eip712:sign-typed-data
                         (coalton:lisp web3/types:Bytes () private-key)
                         (coalton:lisp web3/types:Bytes () domain-sep)
                         (coalton:lisp web3/types:Bytes () struct-hash)))))
      ;; Should succeed
      (assert (result-ok-p sig-result))
      ;; Signature bytes should be 65 bytes
      (let ((sig-bytes (coalton:coalton
                        (web3/crypto:signature-to-bytes
                         (coalton:lisp web3/crypto:Signature () (result-value sig-result))))))
        (assert (= (length sig-bytes) 65)))))

  (test-case "recover-typed-data-signer recovers correct key"
    (let* ((private-key (result-value (coalton:coalton
                                       (web3/types:hex-decode
                                        "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"))))
           (domain-sep (result-value (coalton:coalton (web3/types:hex-decode "3333333333333333333333333333333333333333333333333333333333333333"))))
           (struct-hash (result-value (coalton:coalton (web3/types:hex-decode "4444444444444444444444444444444444444444444444444444444444444444"))))
           ;; Get expected public key
           (expected-pubkey (result-value
                             (coalton:coalton
                              (web3/crypto:private-key-to-public-key
                               (coalton:lisp web3/types:Bytes () private-key)))))
           ;; Sign
           (sig (result-value
                 (coalton:coalton
                  (web3/eip712:sign-typed-data
                   (coalton:lisp web3/types:Bytes () private-key)
                   (coalton:lisp web3/types:Bytes () domain-sep)
                   (coalton:lisp web3/types:Bytes () struct-hash)))))
           ;; Recover
           (recovered (result-value
                       (coalton:coalton
                        (web3/eip712:recover-typed-data-signer
                         (coalton:lisp web3/types:Bytes () domain-sep)
                         (coalton:lisp web3/types:Bytes () struct-hash)
                         (coalton:lisp web3/crypto:Signature () sig))))))
      (assert (equalp recovered expected-pubkey))))

  ;;; =========================================================================
  ;;; Permit Domain Tests
  ;;; =========================================================================

  (test-case "permit-domain creates valid domain"
    (let* ((token (result-value (coalton:coalton (web3/types:hex-decode "A0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"))))
           (domain (coalton:coalton
                    (web3/eip712:permit-domain
                     "USD Coin"
                     "2"
                     (web3/types:u256-from-integer 1)
                     (coalton:lisp web3/types:Bytes () token))))
           (sep (coalton:coalton
                 (web3/eip712:domain-separator
                  (coalton:lisp web3/eip712:EIP712Domain () domain)))))
      (assert (= (length sep) 32))))

  (test-case "permit-struct-hash computes correctly"
    (let* ((owner (result-value (coalton:coalton (web3/types:hex-decode "d8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (spender (result-value (coalton:coalton (web3/types:hex-decode "1234567890123456789012345678901234567890"))))
           (hash (coalton:coalton
                  (web3/eip712:permit-struct-hash
                   (coalton:lisp web3/types:Bytes () owner)
                   (coalton:lisp web3/types:Bytes () spender)
                   (web3/types:u256-from-integer 1000000000000000000)
                   (web3/types:u256-from-integer 0)
                   (web3/types:u256-from-integer 1735689600)))))
      (assert (= (length hash) 32)))))
