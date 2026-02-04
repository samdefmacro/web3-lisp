;;; Contract deployment tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; CREATE Address Tests
;;; =========================================================================

(defun run-deploy-tests ()
  (format t "~%=== Deploy Tests ===~%")

  ;; Test CREATE address computation
  ;; Test vectors from Ethereum documentation and known deployments

  (test-case "compute-create-address nonce 0"
    ;; When address 0x6ac7ea33f8831ea9dcc53393aaa88b25a785dbf0 deploys with nonce 0
    ;; the contract address is 0xcd234a471b72ba2f1ccf0a70fcaba648a5eecd8d
    (let* ((deployer-hex "0x6ac7ea33f8831ea9dcc53393aaa88b25a785dbf0")
           (result (coalton:coalton
                    (web3/deploy:compute-create-address-from-hex
                     (coalton:lisp coalton:String () deployer-hex)
                     0))))
      (assert (is-ok result))
      (let ((addr-hex (coalton:coalton
                       (web3/address:address-to-hex
                        (coalton:lisp web3/address:Address () (result-value result))))))
        (assert (string-equal (string-downcase addr-hex)
                              "0xcd234a471b72ba2f1ccf0a70fcaba648a5eecd8d")))))

  (test-case "compute-create-address nonce 1"
    ;; Same deployer with nonce 1 gives different address
    (let* ((deployer-hex "0x6ac7ea33f8831ea9dcc53393aaa88b25a785dbf0")
           (result (coalton:coalton
                    (web3/deploy:compute-create-address-from-hex
                     (coalton:lisp coalton:String () deployer-hex)
                     1))))
      (assert (is-ok result))
      (let ((addr-hex (coalton:coalton
                       (web3/address:address-to-hex
                        (coalton:lisp web3/address:Address () (result-value result))))))
        (assert (string-equal (string-downcase addr-hex)
                              "0x343c43a37d37dff08ae8c4a11544c718abb4fcf8")))))

  (test-case "compute-create-address high nonce"
    ;; Test with a higher nonce value
    (let* ((deployer-hex "0x6ac7ea33f8831ea9dcc53393aaa88b25a785dbf0")
           (result (coalton:coalton
                    (web3/deploy:compute-create-address-from-hex
                     (coalton:lisp coalton:String () deployer-hex)
                     127))))
      (assert (is-ok result))))

  (test-case "compute-create-address nonce 128 (multi-byte RLP)"
    ;; Nonce >= 128 requires multiple bytes in RLP encoding
    (let* ((deployer-hex "0x6ac7ea33f8831ea9dcc53393aaa88b25a785dbf0")
           (result (coalton:coalton
                    (web3/deploy:compute-create-address-from-hex
                     (coalton:lisp coalton:String () deployer-hex)
                     128))))
      (assert (is-ok result))))

  ;;; =========================================================================
  ;;; CREATE2 Address Tests
  ;;; =========================================================================

  (test-case "compute-create2-address with zero salt"
    ;; Test CREATE2 with all-zero salt
    (let* ((deployer-hex "0x0000000000000000000000000000000000000000")
           (salt (coalton:coalton (web3/types:bytes-from-list coalton:Nil)))
           (init-code (coalton:coalton (web3/types:bytes-from-list coalton:Nil)))
           (result (coalton:coalton
                    (web3/deploy:compute-create2-address-from-hex
                     (coalton:lisp coalton:String () deployer-hex)
                     (coalton:lisp web3/types:Bytes () salt)
                     (coalton:lisp web3/types:Bytes () init-code)))))
      (assert (is-ok result))
      ;; CREATE2 with zero factory, zero salt, empty init code
      ;; = keccak256(0xff ++ 0x00..00 ++ 0x00..00 ++ keccak256(""))[12:]
      (let ((addr-hex (coalton:coalton
                       (web3/address:address-to-hex
                        (coalton:lisp web3/address:Address () (result-value result))))))
        (assert (= (length addr-hex) 42)))))  ;; 0x + 40 hex chars

  (test-case "compute-create2-address Uniswap V2 style"
    ;; Uniswap V2 uses CREATE2 for pair deployment
    ;; Factory address and init code hash determine the pair address
    (let* ((factory-hex "0x5C69bEe701ef814a2B6a3EDD4B1652CB9cc5aA6f")
           ;; A sample salt (usually hash of token0 and token1)
           (salt (result-value (coalton:coalton
                                (web3/types:hex-decode
                                 "0000000000000000000000000000000000000000000000000000000000000001"))))
           ;; Simple init code for testing
           (init-code (result-value (coalton:coalton (web3/types:hex-decode "6000"))))
           (result (coalton:coalton
                    (web3/deploy:compute-create2-address-from-hex
                     (coalton:lisp coalton:String () factory-hex)
                     (coalton:lisp web3/types:Bytes () salt)
                     (coalton:lisp web3/types:Bytes () init-code)))))
      (assert (is-ok result))
      ;; Just verify we get a valid address back
      (let ((addr-hex (coalton:coalton
                       (web3/address:address-to-hex
                        (coalton:lisp web3/address:Address () (result-value result))))))
        (assert (= (length addr-hex) 42)))))

  (test-case "CREATE2 is deterministic"
    ;; Same inputs should always produce the same address
    (let* ((deployer-hex "0x1234567890123456789012345678901234567890")
           (salt (result-value (coalton:coalton
                                (web3/types:hex-decode
                                 "000000000000000000000000000000000000000000000000000000000000002a"))))
           (init-code (result-value (coalton:coalton (web3/types:hex-decode "deadbeef"))))
           (result1 (coalton:coalton
                     (web3/deploy:compute-create2-address-from-hex
                      (coalton:lisp coalton:String () deployer-hex)
                      (coalton:lisp web3/types:Bytes () salt)
                      (coalton:lisp web3/types:Bytes () init-code))))
           (result2 (coalton:coalton
                     (web3/deploy:compute-create2-address-from-hex
                      (coalton:lisp coalton:String () deployer-hex)
                      (coalton:lisp web3/types:Bytes () salt)
                      (coalton:lisp web3/types:Bytes () init-code)))))
      (assert (is-ok result1))
      (assert (is-ok result2))
      (let ((addr1 (coalton:coalton
                    (web3/address:address-to-hex
                     (coalton:lisp web3/address:Address () (result-value result1)))))
            (addr2 (coalton:coalton
                    (web3/address:address-to-hex
                     (coalton:lisp web3/address:Address () (result-value result2))))))
        (assert (string-equal addr1 addr2)))))

  ;;; =========================================================================
  ;;; Deployment Data Tests
  ;;; =========================================================================

  (test-case "deployment-data returns bytecode unchanged"
    (let* ((bytecode (result-value (coalton:coalton (web3/types:hex-decode "6080604052"))))
           (data (coalton:coalton
                  (web3/deploy:deployment-data
                   (coalton:lisp web3/types:Bytes () bytecode)))))
      (assert (coalton:coalton
               (web3/types:bytes-equal?
                (coalton:lisp web3/types:Bytes () data)
                (coalton:lisp web3/types:Bytes () bytecode))))))

  (test-case "deployment-data-with-constructor appends encoded args"
    (let* ((bytecode (result-value (coalton:coalton (web3/types:hex-decode "6080604052"))))
           ;; Constructor with a single uint256 argument (value = 42)
           (data (coalton:coalton
                  (web3/deploy:deployment-data-with-constructor
                   (coalton:lisp web3/types:Bytes () bytecode)
                   (coalton:Cons
                    (web3/abi:AbiUintVal (web3/types:u256-from-integer 42))
                    coalton:Nil)))))
      ;; Result should be bytecode (5 bytes) + encoded uint256 (32 bytes) = 37 bytes
      (assert (= (length data) 37))
      ;; First 5 bytes should be the bytecode
      (assert (= (aref data 0) #x60))
      (assert (= (aref data 1) #x80))
      ;; Last byte should be 42 (from the encoded uint256)
      (assert (= (aref data 36) 42))))

  (test-case "deployment-data-with-constructor handles empty args"
    (let* ((bytecode (result-value (coalton:coalton (web3/types:hex-decode "6080604052"))))
           (data (coalton:coalton
                  (web3/deploy:deployment-data-with-constructor
                   (coalton:lisp web3/types:Bytes () bytecode)
                   coalton:Nil))))
      ;; With empty args, should just return bytecode
      (assert (= (length data) 5))))

  ;;; =========================================================================
  ;;; Additional CREATE Address Tests
  ;;; =========================================================================

  (test-case "compute-create-address with zero address"
    ;; Zero address deploying with nonce 0
    (let* ((deployer-hex "0x0000000000000000000000000000000000000000")
           (result (coalton:coalton
                    (web3/deploy:compute-create-address-from-hex
                     (coalton:lisp coalton:String () deployer-hex)
                     0))))
      (assert (is-ok result))
      ;; Verify we get a valid address
      (let ((addr-hex (coalton:coalton
                       (web3/address:address-to-hex
                        (coalton:lisp web3/address:Address () (result-value result))))))
        (assert (= (length addr-hex) 42)))))

  (test-case "compute-create-address nonce 255"
    ;; Nonce 255 (0xff) - single byte edge case
    (let* ((deployer-hex "0x6ac7ea33f8831ea9dcc53393aaa88b25a785dbf0")
           (result (coalton:coalton
                    (web3/deploy:compute-create-address-from-hex
                     (coalton:lisp coalton:String () deployer-hex)
                     255))))
      (assert (is-ok result))))

  (test-case "compute-create-address nonce 256"
    ;; Nonce 256 (0x0100) - two byte encoding
    (let* ((deployer-hex "0x6ac7ea33f8831ea9dcc53393aaa88b25a785dbf0")
           (result (coalton:coalton
                    (web3/deploy:compute-create-address-from-hex
                     (coalton:lisp coalton:String () deployer-hex)
                     256))))
      (assert (is-ok result))))

  (test-case "compute-create-address large nonce"
    ;; Very large nonce to test multi-byte RLP encoding
    (let* ((deployer-hex "0x6ac7ea33f8831ea9dcc53393aaa88b25a785dbf0")
           (result (coalton:coalton
                    (web3/deploy:compute-create-address-from-hex
                     (coalton:lisp coalton:String () deployer-hex)
                     1000000))))
      (assert (is-ok result))))

  (test-case "CREATE addresses are deterministic"
    ;; Same deployer + nonce always produces same address
    (let* ((deployer-hex "0x1234567890123456789012345678901234567890")
           (result1 (coalton:coalton
                     (web3/deploy:compute-create-address-from-hex
                      (coalton:lisp coalton:String () deployer-hex)
                      42)))
           (result2 (coalton:coalton
                     (web3/deploy:compute-create-address-from-hex
                      (coalton:lisp coalton:String () deployer-hex)
                      42))))
      (assert (is-ok result1))
      (assert (is-ok result2))
      (let ((addr1 (coalton:coalton
                    (web3/address:address-to-hex
                     (coalton:lisp web3/address:Address () (result-value result1)))))
            (addr2 (coalton:coalton
                    (web3/address:address-to-hex
                     (coalton:lisp web3/address:Address () (result-value result2))))))
        (assert (string-equal addr1 addr2)))))

  (test-case "CREATE addresses differ by nonce"
    ;; Different nonces produce different addresses
    (let* ((deployer-hex "0x6ac7ea33f8831ea9dcc53393aaa88b25a785dbf0")
           (result1 (coalton:coalton
                     (web3/deploy:compute-create-address-from-hex
                      (coalton:lisp coalton:String () deployer-hex)
                      10)))
           (result2 (coalton:coalton
                     (web3/deploy:compute-create-address-from-hex
                      (coalton:lisp coalton:String () deployer-hex)
                      11))))
      (assert (is-ok result1))
      (assert (is-ok result2))
      (let ((addr1 (coalton:coalton
                    (web3/address:address-to-hex
                     (coalton:lisp web3/address:Address () (result-value result1)))))
            (addr2 (coalton:coalton
                    (web3/address:address-to-hex
                     (coalton:lisp web3/address:Address () (result-value result2))))))
        (assert (not (string-equal addr1 addr2))))))

  (test-case "compute-create-address-from-hex rejects invalid hex"
    (let ((result (coalton:coalton
                   (web3/deploy:compute-create-address-from-hex
                    "not-valid-hex"
                    0))))
      (assert (not (is-ok result)))))

  ;;; =========================================================================
  ;;; Additional CREATE2 Address Tests
  ;;; =========================================================================

  (test-case "CREATE2 with 32-byte salt"
    ;; Standard 32-byte salt
    (let* ((deployer-hex "0x1234567890123456789012345678901234567890")
           (salt (result-value (coalton:coalton
                                (web3/types:hex-decode
                                 "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"))))
           (init-code (result-value (coalton:coalton (web3/types:hex-decode "600160005260206000f3"))))
           (result (coalton:coalton
                    (web3/deploy:compute-create2-address-from-hex
                     (coalton:lisp coalton:String () deployer-hex)
                     (coalton:lisp web3/types:Bytes () salt)
                     (coalton:lisp web3/types:Bytes () init-code)))))
      (assert (is-ok result))))

  (test-case "CREATE2 addresses differ by salt"
    ;; Different salts produce different addresses
    (let* ((deployer-hex "0x1234567890123456789012345678901234567890")
           (salt1 (result-value (coalton:coalton
                                 (web3/types:hex-decode
                                  "0000000000000000000000000000000000000000000000000000000000000001"))))
           (salt2 (result-value (coalton:coalton
                                 (web3/types:hex-decode
                                  "0000000000000000000000000000000000000000000000000000000000000002"))))
           (init-code (result-value (coalton:coalton (web3/types:hex-decode "6000"))))
           (result1 (coalton:coalton
                     (web3/deploy:compute-create2-address-from-hex
                      (coalton:lisp coalton:String () deployer-hex)
                      (coalton:lisp web3/types:Bytes () salt1)
                      (coalton:lisp web3/types:Bytes () init-code))))
           (result2 (coalton:coalton
                     (web3/deploy:compute-create2-address-from-hex
                      (coalton:lisp coalton:String () deployer-hex)
                      (coalton:lisp web3/types:Bytes () salt2)
                      (coalton:lisp web3/types:Bytes () init-code)))))
      (assert (is-ok result1))
      (assert (is-ok result2))
      (let ((addr1 (coalton:coalton
                    (web3/address:address-to-hex
                     (coalton:lisp web3/address:Address () (result-value result1)))))
            (addr2 (coalton:coalton
                    (web3/address:address-to-hex
                     (coalton:lisp web3/address:Address () (result-value result2))))))
        (assert (not (string-equal addr1 addr2))))))

  (test-case "CREATE2 addresses differ by init code"
    ;; Different init code produces different addresses
    (let* ((deployer-hex "0x1234567890123456789012345678901234567890")
           (salt (result-value (coalton:coalton
                                (web3/types:hex-decode
                                 "0000000000000000000000000000000000000000000000000000000000000001"))))
           (init-code1 (result-value (coalton:coalton (web3/types:hex-decode "6000"))))
           (init-code2 (result-value (coalton:coalton (web3/types:hex-decode "6001"))))
           (result1 (coalton:coalton
                     (web3/deploy:compute-create2-address-from-hex
                      (coalton:lisp coalton:String () deployer-hex)
                      (coalton:lisp web3/types:Bytes () salt)
                      (coalton:lisp web3/types:Bytes () init-code1))))
           (result2 (coalton:coalton
                     (web3/deploy:compute-create2-address-from-hex
                      (coalton:lisp coalton:String () deployer-hex)
                      (coalton:lisp web3/types:Bytes () salt)
                      (coalton:lisp web3/types:Bytes () init-code2)))))
      (assert (is-ok result1))
      (assert (is-ok result2))
      (let ((addr1 (coalton:coalton
                    (web3/address:address-to-hex
                     (coalton:lisp web3/address:Address () (result-value result1)))))
            (addr2 (coalton:coalton
                    (web3/address:address-to-hex
                     (coalton:lisp web3/address:Address () (result-value result2))))))
        (assert (not (string-equal addr1 addr2))))))

  (test-case "compute-create2-address-from-hex rejects invalid hex"
    (let* ((salt (result-value (coalton:coalton
                                (web3/types:hex-decode
                                 "0000000000000000000000000000000000000000000000000000000000000001"))))
           (init-code (result-value (coalton:coalton (web3/types:hex-decode "6000"))))
           (result (coalton:coalton
                    (web3/deploy:compute-create2-address-from-hex
                     "invalid-address"
                     (coalton:lisp web3/types:Bytes () salt)
                     (coalton:lisp web3/types:Bytes () init-code)))))
      (assert (not (is-ok result)))))

  ;;; =========================================================================
  ;;; Additional Constructor Encoding Tests
  ;;; =========================================================================

  (test-case "deployment-data-with-constructor multiple uint256 args"
    (let* ((bytecode (result-value (coalton:coalton (web3/types:hex-decode "6080604052"))))
           (data (coalton:coalton
                  (web3/deploy:deployment-data-with-constructor
                   (coalton:lisp web3/types:Bytes () bytecode)
                   (coalton:Cons
                    (web3/abi:AbiUintVal (web3/types:u256-from-integer 100))
                    (coalton:Cons
                     (web3/abi:AbiUintVal (web3/types:u256-from-integer 200))
                     coalton:Nil))))))
      ;; 5 bytes bytecode + 32 bytes + 32 bytes = 69 bytes
      (assert (= (length data) 69))))

  (test-case "deployment-data-with-constructor address argument"
    (let* ((bytecode (result-value (coalton:coalton (web3/types:hex-decode "6080604052"))))
           (addr (result-value (coalton:coalton
                                (web3/address:address-from-hex
                                 "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           ;; Convert Address to Bytes for AbiAddressVal
           (addr-bytes (coalton:coalton
                        (web3/address:address-bytes
                         (coalton:lisp web3/address:Address () addr))))
           (data (coalton:coalton
                  (web3/deploy:deployment-data-with-constructor
                   (coalton:lisp web3/types:Bytes () bytecode)
                   (coalton:Cons
                    (web3/abi:AbiAddressVal (coalton:lisp web3/types:Bytes () addr-bytes))
                    coalton:Nil)))))
      ;; 5 bytes bytecode + 32 bytes address = 37 bytes
      (assert (= (length data) 37))))

  (test-case "deployment-data-with-constructor bool argument"
    (let* ((bytecode (result-value (coalton:coalton (web3/types:hex-decode "6080604052"))))
           (data (coalton:coalton
                  (web3/deploy:deployment-data-with-constructor
                   (coalton:lisp web3/types:Bytes () bytecode)
                   (coalton:Cons
                    (web3/abi:AbiBoolVal coalton:True)
                    coalton:Nil)))))
      ;; 5 bytes bytecode + 32 bytes bool = 37 bytes
      (assert (= (length data) 37))
      ;; Last byte should be 1 (true)
      (assert (= (aref data 36) 1))))

  (test-case "deployment-data with empty bytecode"
    (let* ((bytecode (result-value (coalton:coalton (web3/types:hex-decode ""))))
           (data (coalton:coalton
                  (web3/deploy:deployment-data
                   (coalton:lisp web3/types:Bytes () bytecode)))))
      (assert (= (length data) 0))))

  (test-case "deployment-data preserves bytecode exactly"
    (let* ((bytecode (result-value (coalton:coalton
                                    (web3/types:hex-decode
                                     "608060405234801561001057600080fd5b50"))))
           (data (coalton:coalton
                  (web3/deploy:deployment-data
                   (coalton:lisp web3/types:Bytes () bytecode)))))
      (assert (= (length data) (length bytecode)))
      ;; Verify first few bytes
      (assert (= (aref data 0) #x60))
      (assert (= (aref data 1) #x80))
      (assert (= (aref data 2) #x60))
      (assert (= (aref data 3) #x40)))))
