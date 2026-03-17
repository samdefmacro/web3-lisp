;;; SIWE module tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; SIWE Tests
;;; =========================================================================

(defun run-siwe-tests ()
  (format t "~%=== SIWE Tests ===~%")

  ;;; =========================================================================
  ;;; Nonce Generation Tests
  ;;; =========================================================================

  (test-case "generate-siwe-nonce produces 16 characters"
    (let ((nonce (coalton:coalton (web3/siwe:generate-siwe-nonce coalton:Unit))))
      (assert (= (length nonce) 16))))

  (test-case "generate-siwe-nonce produces alphanumeric"
    (let ((nonce (coalton:coalton (web3/siwe:generate-siwe-nonce coalton:Unit))))
      (assert (every #'alphanumericp nonce))))

  (test-case "generate-siwe-nonce produces different values"
    (let ((nonce1 (coalton:coalton (web3/siwe:generate-siwe-nonce coalton:Unit)))
          (nonce2 (coalton:coalton (web3/siwe:generate-siwe-nonce coalton:Unit))))
      ;; While theoretically possible for these to be equal, it's astronomically unlikely
      (assert (not (string= nonce1 nonce2)))))

  ;;; =========================================================================
  ;;; Message Creation Tests
  ;;; =========================================================================

  (test-case "create-siwe-message includes domain"
    (let* ((addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (msg (coalton:coalton
                 (web3/siwe:make-siwe-message
                  "example.com"
                  (coalton:lisp web3/address:Address () addr)
                  "https://example.com"
                  1
                  "abc12345"
                  "2024-01-15T12:00:00Z")))
           (message-str (coalton:coalton
                         (web3/siwe:create-siwe-message
                          (coalton:lisp web3/siwe:SiweMessage () msg)))))
      (assert (search "example.com wants you to sign in" message-str))))

  (test-case "create-siwe-message includes address"
    (let* ((addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (msg (coalton:coalton
                 (web3/siwe:make-siwe-message
                  "example.com"
                  (coalton:lisp web3/address:Address () addr)
                  "https://example.com"
                  1
                  "abc12345"
                  "2024-01-15T12:00:00Z")))
           (message-str (coalton:coalton
                         (web3/siwe:create-siwe-message
                          (coalton:lisp web3/siwe:SiweMessage () msg)))))
      (assert (search "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266" message-str))))

  (test-case "create-siwe-message includes required fields"
    (let* ((addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (msg (coalton:coalton
                 (web3/siwe:make-siwe-message
                  "example.com"
                  (coalton:lisp web3/address:Address () addr)
                  "https://example.com"
                  1
                  "abc12345"
                  "2024-01-15T12:00:00Z")))
           (message-str (coalton:coalton
                         (web3/siwe:create-siwe-message
                          (coalton:lisp web3/siwe:SiweMessage () msg)))))
      (assert (search "URI: https://example.com" message-str))
      (assert (search "Version: 1" message-str))
      (assert (search "Chain ID: 1" message-str))
      (assert (search "Nonce: abc12345" message-str))
      (assert (search "Issued At: 2024-01-15T12:00:00Z" message-str))))

  ;;; =========================================================================
  ;;; Message Parsing Tests
  ;;; =========================================================================

  (test-case "parse-siwe-message parses basic message"
    (let* ((addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (original (coalton:coalton
                      (web3/siwe:make-siwe-message
                       "example.com"
                       (coalton:lisp web3/address:Address () addr)
                       "https://example.com"
                       1
                       "abc12345"
                       "2024-01-15T12:00:00Z")))
           (message-str (coalton:coalton
                         (web3/siwe:create-siwe-message
                          (coalton:lisp web3/siwe:SiweMessage () original))))
           (parsed-result (coalton:coalton
                           (web3/siwe:parse-siwe-message
                            (coalton:lisp coalton:String () message-str)))))
      (assert (result-ok-p parsed-result))))

  (test-case "parse-siwe-message recovers domain"
    (let* ((addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (original (coalton:coalton
                      (web3/siwe:make-siwe-message
                       "example.com"
                       (coalton:lisp web3/address:Address () addr)
                       "https://example.com"
                       1
                       "abc12345"
                       "2024-01-15T12:00:00Z")))
           (message-str (coalton:coalton
                         (web3/siwe:create-siwe-message
                          (coalton:lisp web3/siwe:SiweMessage () original))))
           (parsed-result (coalton:coalton
                           (web3/siwe:parse-siwe-message
                            (coalton:lisp coalton:String () message-str)))))
      (assert (result-ok-p parsed-result))
      (let* ((parsed (result-value parsed-result))
             (domain (coalton:coalton
                      (web3/siwe:.siwe-domain
                       (coalton:lisp web3/siwe:SiweMessage () parsed)))))
        (assert (string= domain "example.com")))))

  (test-case "parse-siwe-message rejects invalid format"
    (let ((result (coalton:coalton
                   (web3/siwe:parse-siwe-message "not a valid siwe message"))))
      (assert (result-err-p result))))

  ;;; =========================================================================
  ;;; Create/Parse Roundtrip Tests
  ;;; =========================================================================

  (test-case "create and parse siwe message roundtrip"
    (let* ((addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (original (coalton:coalton
                      (web3/siwe:make-siwe-message
                       "test.example.com"
                       (coalton:lisp web3/address:Address () addr)
                       "https://test.example.com/login"
                       137
                       "randomnonce123"
                       "2024-06-15T10:30:00Z")))
           (message-str (coalton:coalton
                         (web3/siwe:create-siwe-message
                          (coalton:lisp web3/siwe:SiweMessage () original))))
           (parsed-result (coalton:coalton
                           (web3/siwe:parse-siwe-message
                            (coalton:lisp coalton:String () message-str)))))
      (assert (result-ok-p parsed-result))
      (let* ((parsed (result-value parsed-result))
             (parsed-domain (coalton:coalton
                             (web3/siwe:.siwe-domain
                              (coalton:lisp web3/siwe:SiweMessage () parsed))))
             (parsed-uri (coalton:coalton
                          (web3/siwe:.siwe-uri
                           (coalton:lisp web3/siwe:SiweMessage () parsed))))
             (parsed-chain-id (coalton:coalton
                               (web3/siwe:.siwe-chain-id
                                (coalton:lisp web3/siwe:SiweMessage () parsed))))
             (parsed-nonce (coalton:coalton
                            (web3/siwe:.siwe-nonce
                             (coalton:lisp web3/siwe:SiweMessage () parsed)))))
        (assert (string= parsed-domain "test.example.com"))
        (assert (string= parsed-uri "https://test.example.com/login"))
        (assert (= parsed-chain-id 137))
        (assert (string= parsed-nonce "randomnonce123")))))

  ;;; =========================================================================
  ;;; Time Validation Tests
  ;;; =========================================================================

  (test-case "siwe-message-expired? returns false for no expiration"
    (let* ((addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (msg (coalton:coalton
                 (web3/siwe:make-siwe-message
                  "example.com"
                  (coalton:lisp web3/address:Address () addr)
                  "https://example.com"
                  1
                  "abc12345"
                  "2024-01-15T12:00:00Z"))))
      ;; Message created with make-siwe-message has no expiration
      (assert (eq (coalton:coalton
                   (web3/siwe:siwe-message-expired?
                    (coalton:lisp web3/siwe:SiweMessage () msg)))
                  coalton:False))))

  (test-case "siwe-message-not-yet-valid? returns false for no not-before"
    (let* ((addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (msg (coalton:coalton
                 (web3/siwe:make-siwe-message
                  "example.com"
                  (coalton:lisp web3/address:Address () addr)
                  "https://example.com"
                  1
                  "abc12345"
                  "2024-01-15T12:00:00Z"))))
      ;; Message created with make-siwe-message has no not-before
      (assert (eq (coalton:coalton
                   (web3/siwe:siwe-message-not-yet-valid?
                    (coalton:lisp web3/siwe:SiweMessage () msg)))
                  coalton:False))))

  ;;; =========================================================================
  ;;; Signature Validation Tests
  ;;; =========================================================================

  (test-case "validate-siwe-signature with valid signature"
    ;; Sign a SIWE message and verify it
    (let* ((private-key (result-value
                         (coalton:coalton
                          (web3/types:hex-decode
                           "ac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))))
           (addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (msg (coalton:coalton
                 (web3/siwe:make-siwe-message
                  "example.com"
                  (coalton:lisp web3/address:Address () addr)
                  "https://example.com"
                  1
                  "abc12345"
                  "2024-01-15T12:00:00Z")))
           (message-str (coalton:coalton
                         (web3/siwe:create-siwe-message
                          (coalton:lisp web3/siwe:SiweMessage () msg))))
           ;; Convert message to bytes for signing (proper adjustable array)
           (message-bytes (let* ((len (length message-str))
                                 (arr (make-array len :fill-pointer len :adjustable t)))
                            (loop :for i :from 0 :below len
                                  :do (setf (aref arr i) (char-code (char message-str i))))
                            arr))
           (sign-result (coalton:coalton
                         (web3/signature:personal-sign
                          (coalton:lisp web3/types:Bytes () message-bytes)
                          (coalton:lisp web3/types:Bytes () private-key)))))
      (assert (result-ok-p sign-result))
      (let* ((sig (result-value sign-result))
             (valid (coalton:coalton
                     (web3/siwe:validate-siwe-signature
                      (coalton:lisp coalton:String () message-str)
                      (coalton:lisp web3/signature:Signature () sig)
                      (coalton:lisp web3/address:Address () addr)))))
        (assert (eq valid coalton:True)))))

  (test-case "validate-siwe-signature rejects wrong signer"
    (let* ((private-key (result-value
                         (coalton:coalton
                          (web3/types:hex-decode
                           "ac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))))
           (signing-addr (result-value
                          (coalton:coalton
                           (web3/address:address-from-hex
                            "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (wrong-addr (result-value
                        (coalton:coalton
                         (web3/address:address-from-hex
                          "0x0000000000000000000000000000000000000001"))))
           (msg (coalton:coalton
                 (web3/siwe:make-siwe-message
                  "example.com"
                  (coalton:lisp web3/address:Address () signing-addr)
                  "https://example.com"
                  1
                  "abc12345"
                  "2024-01-15T12:00:00Z")))
           (message-str (coalton:coalton
                         (web3/siwe:create-siwe-message
                          (coalton:lisp web3/siwe:SiweMessage () msg))))
           (message-bytes (let* ((len (length message-str))
                                 (arr (make-array len :fill-pointer len :adjustable t)))
                            (loop :for i :from 0 :below len
                                  :do (setf (aref arr i) (char-code (char message-str i))))
                            arr))
           (sign-result (coalton:coalton
                         (web3/signature:personal-sign
                          (coalton:lisp web3/types:Bytes () message-bytes)
                          (coalton:lisp web3/types:Bytes () private-key)))))
      (assert (result-ok-p sign-result))
      (let* ((sig (result-value sign-result))
             (valid (coalton:coalton
                     (web3/siwe:validate-siwe-signature
                      (coalton:lisp coalton:String () message-str)
                      (coalton:lisp web3/signature:Signature () sig)
                      (coalton:lisp web3/address:Address () wrong-addr)))))
        (assert (eq valid coalton:False)))))

  ;;; =========================================================================
  ;;; Full Verification Tests
  ;;; =========================================================================

  (test-case "verify-siwe-message succeeds for valid message and signature"
    (let* ((private-key (result-value
                         (coalton:coalton
                          (web3/types:hex-decode
                           "ac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))))
           (addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (msg (coalton:coalton
                 (web3/siwe:make-siwe-message
                  "example.com"
                  (coalton:lisp web3/address:Address () addr)
                  "https://example.com"
                  1
                  "abc12345"
                  "2024-01-15T12:00:00Z")))
           (message-str (coalton:coalton
                         (web3/siwe:create-siwe-message
                          (coalton:lisp web3/siwe:SiweMessage () msg))))
           (message-bytes (let* ((len (length message-str))
                                 (arr (make-array len :fill-pointer len :adjustable t)))
                            (loop :for i :from 0 :below len
                                  :do (setf (aref arr i) (char-code (char message-str i))))
                            arr))
           (sign-result (coalton:coalton
                         (web3/signature:personal-sign
                          (coalton:lisp web3/types:Bytes () message-bytes)
                          (coalton:lisp web3/types:Bytes () private-key)))))
      (assert (result-ok-p sign-result))
      (let* ((sig (result-value sign-result))
             (verify-result (coalton:coalton
                             (web3/siwe:verify-siwe-message
                              (coalton:lisp web3/siwe:SiweMessage () msg)
                              (coalton:lisp web3/signature:Signature () sig)))))
        (assert (result-ok-p verify-result)))))

  ;;; =========================================================================
  ;;; Message with Optional Statement Tests
  ;;; =========================================================================

  (test-case "create-siwe-message includes statement when present"
    (let* ((addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (msg (coalton:coalton
                 (web3/siwe:SiweMessage
                  "example.com"
                  (coalton:lisp web3/address:Address () addr)
                  (coalton-prelude:Some "I accept the Terms of Service")
                  "https://example.com"
                  "1"
                  1
                  "abc12345"
                  "2024-01-15T12:00:00Z"
                  coalton-prelude:None
                  coalton-prelude:None
                  coalton-prelude:None
                  coalton-prelude:None)))
           (message-str (coalton:coalton
                         (web3/siwe:create-siwe-message
                          (coalton:lisp web3/siwe:SiweMessage () msg)))))
      (assert (search "I accept the Terms of Service" message-str))))

  (test-case "parse-siwe-message recovers statement"
    (let* ((addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (original (coalton:coalton
                      (web3/siwe:SiweMessage
                       "example.com"
                       (coalton:lisp web3/address:Address () addr)
                       (coalton-prelude:Some "Sign to prove ownership")
                       "https://example.com"
                       "1"
                       1
                       "abc12345"
                       "2024-01-15T12:00:00Z"
                       coalton-prelude:None
                       coalton-prelude:None
                       coalton-prelude:None
                       coalton-prelude:None)))
           (message-str (coalton:coalton
                         (web3/siwe:create-siwe-message
                          (coalton:lisp web3/siwe:SiweMessage () original))))
           (parsed-result (coalton:coalton
                           (web3/siwe:parse-siwe-message
                            (coalton:lisp coalton:String () message-str)))))
      (assert (result-ok-p parsed-result))
      (let* ((parsed (result-value parsed-result))
             (statement-opt (coalton:coalton
                             (web3/siwe:.siwe-statement
                              (coalton:lisp web3/siwe:SiweMessage () parsed)))))
        (assert (optional-some-p statement-opt))
        (assert (string= (result-value statement-opt)
                         "Sign to prove ownership")))))

  ;;; =========================================================================
  ;;; Message with Resources Tests
  ;;; =========================================================================

  (test-case "create-siwe-message includes resources when present"
    (let* ((addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (msg (coalton:coalton
                 (web3/siwe:SiweMessage
                  "example.com"
                  (coalton:lisp web3/address:Address () addr)
                  coalton-prelude:None
                  "https://example.com"
                  "1"
                  1
                  "abc12345"
                  "2024-01-15T12:00:00Z"
                  coalton-prelude:None
                  coalton-prelude:None
                  coalton-prelude:None
                  (coalton-prelude:Some (coalton:make-list "https://example.com/terms"
                                                           "https://example.com/privacy")))))
           (message-str (coalton:coalton
                         (web3/siwe:create-siwe-message
                          (coalton:lisp web3/siwe:SiweMessage () msg)))))
      (assert (search "Resources:" message-str))
      (assert (search "- https://example.com/terms" message-str))
      (assert (search "- https://example.com/privacy" message-str))))

  ;;; =========================================================================
  ;;; Expiration Time Validation Tests
  ;;; =========================================================================

  (test-case "siwe-message-expired? returns true for past expiration"
    (let* ((addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           ;; Create message with expiration in the past (year 2020)
           (msg (coalton:coalton
                 (web3/siwe:SiweMessage
                  "example.com"
                  (coalton:lisp web3/address:Address () addr)
                  coalton-prelude:None
                  "https://example.com"
                  "1"
                  1
                  "abc12345"
                  "2020-01-01T00:00:00Z"
                  (coalton-prelude:Some "2020-01-02T00:00:00Z")  ;; expired
                  coalton-prelude:None
                  coalton-prelude:None
                  coalton-prelude:None))))
      (assert (eq (coalton:coalton
                   (web3/siwe:siwe-message-expired?
                    (coalton:lisp web3/siwe:SiweMessage () msg)))
                  coalton:True))))

  (test-case "siwe-message-expired? returns false for future expiration"
    (let* ((addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           ;; Create message with expiration far in the future (year 2099)
           (msg (coalton:coalton
                 (web3/siwe:SiweMessage
                  "example.com"
                  (coalton:lisp web3/address:Address () addr)
                  coalton-prelude:None
                  "https://example.com"
                  "1"
                  1
                  "abc12345"
                  "2024-01-01T00:00:00Z"
                  (coalton-prelude:Some "2099-12-31T23:59:59Z")  ;; far future
                  coalton-prelude:None
                  coalton-prelude:None
                  coalton-prelude:None))))
      (assert (eq (coalton:coalton
                   (web3/siwe:siwe-message-expired?
                    (coalton:lisp web3/siwe:SiweMessage () msg)))
                  coalton:False))))

  ;;; =========================================================================
  ;;; Not-Before Time Validation Tests
  ;;; =========================================================================

  (test-case "siwe-message-not-yet-valid? returns true for future not-before"
    (let* ((addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           ;; Create message with not-before in the future (year 2099)
           (msg (coalton:coalton
                 (web3/siwe:SiweMessage
                  "example.com"
                  (coalton:lisp web3/address:Address () addr)
                  coalton-prelude:None
                  "https://example.com"
                  "1"
                  1
                  "abc12345"
                  "2024-01-01T00:00:00Z"
                  coalton-prelude:None
                  (coalton-prelude:Some "2099-01-01T00:00:00Z")  ;; future not-before
                  coalton-prelude:None
                  coalton-prelude:None))))
      (assert (eq (coalton:coalton
                   (web3/siwe:siwe-message-not-yet-valid?
                    (coalton:lisp web3/siwe:SiweMessage () msg)))
                  coalton:True))))

  (test-case "siwe-message-not-yet-valid? returns false for past not-before"
    (let* ((addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           ;; Create message with not-before in the past (year 2020)
           (msg (coalton:coalton
                 (web3/siwe:SiweMessage
                  "example.com"
                  (coalton:lisp web3/address:Address () addr)
                  coalton-prelude:None
                  "https://example.com"
                  "1"
                  1
                  "abc12345"
                  "2024-01-01T00:00:00Z"
                  coalton-prelude:None
                  (coalton-prelude:Some "2020-01-01T00:00:00Z")  ;; past not-before
                  coalton-prelude:None
                  coalton-prelude:None))))
      (assert (eq (coalton:coalton
                   (web3/siwe:siwe-message-not-yet-valid?
                    (coalton:lisp web3/siwe:SiweMessage () msg)))
                  coalton:False))))

  ;;; =========================================================================
  ;;; verify-siwe-message with Expiration Tests
  ;;; =========================================================================

  (test-case "verify-siwe-message rejects expired message"
    (let* ((private-key (result-value
                         (coalton:coalton
                          (web3/types:hex-decode
                           "ac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))))
           (addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           ;; Create expired message
           (msg (coalton:coalton
                 (web3/siwe:SiweMessage
                  "example.com"
                  (coalton:lisp web3/address:Address () addr)
                  coalton-prelude:None
                  "https://example.com"
                  "1"
                  1
                  "abc12345"
                  "2020-01-01T00:00:00Z"
                  (coalton-prelude:Some "2020-01-02T00:00:00Z")  ;; expired
                  coalton-prelude:None
                  coalton-prelude:None
                  coalton-prelude:None)))
           (message-str (coalton:coalton
                         (web3/siwe:create-siwe-message
                          (coalton:lisp web3/siwe:SiweMessage () msg))))
           (message-bytes (let* ((len (length message-str))
                                 (arr (make-array len :fill-pointer len :adjustable t)))
                            (loop :for i :from 0 :below len
                                  :do (setf (aref arr i) (char-code (char message-str i))))
                            arr))
           (sign-result (coalton:coalton
                         (web3/signature:personal-sign
                          (coalton:lisp web3/types:Bytes () message-bytes)
                          (coalton:lisp web3/types:Bytes () private-key)))))
      (assert (result-ok-p sign-result))
      (let* ((sig (result-value sign-result))
             (verify-result (coalton:coalton
                             (web3/siwe:verify-siwe-message
                              (coalton:lisp web3/siwe:SiweMessage () msg)
                              (coalton:lisp web3/signature:Signature () sig)))))
        ;; Should fail due to expiration
        (assert (result-err-p verify-result)))))

  (test-case "verify-siwe-message rejects not-yet-valid message"
    (let* ((private-key (result-value
                         (coalton:coalton
                          (web3/types:hex-decode
                           "ac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))))
           (addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           ;; Create message not yet valid
           (msg (coalton:coalton
                 (web3/siwe:SiweMessage
                  "example.com"
                  (coalton:lisp web3/address:Address () addr)
                  coalton-prelude:None
                  "https://example.com"
                  "1"
                  1
                  "abc12345"
                  "2024-01-01T00:00:00Z"
                  coalton-prelude:None
                  (coalton-prelude:Some "2099-01-01T00:00:00Z")  ;; future not-before
                  coalton-prelude:None
                  coalton-prelude:None)))
           (message-str (coalton:coalton
                         (web3/siwe:create-siwe-message
                          (coalton:lisp web3/siwe:SiweMessage () msg))))
           (message-bytes (let* ((len (length message-str))
                                 (arr (make-array len :fill-pointer len :adjustable t)))
                            (loop :for i :from 0 :below len
                                  :do (setf (aref arr i) (char-code (char message-str i))))
                            arr))
           (sign-result (coalton:coalton
                         (web3/signature:personal-sign
                          (coalton:lisp web3/types:Bytes () message-bytes)
                          (coalton:lisp web3/types:Bytes () private-key)))))
      (assert (result-ok-p sign-result))
      (let* ((sig (result-value sign-result))
             (verify-result (coalton:coalton
                             (web3/siwe:verify-siwe-message
                              (coalton:lisp web3/siwe:SiweMessage () msg)
                              (coalton:lisp web3/signature:Signature () sig)))))
        ;; Should fail due to not-before
        (assert (result-err-p verify-result)))))

  ;;; =========================================================================
  ;;; Different Chain ID Tests
  ;;; =========================================================================

  (test-case "create-siwe-message with different chain IDs"
    (let* ((addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")))))
      ;; Test Polygon (137)
      (let* ((msg (coalton:coalton
                   (web3/siwe:make-siwe-message
                    "polygon.example.com"
                    (coalton:lisp web3/address:Address () addr)
                    "https://polygon.example.com"
                    137
                    "polygonnonce"
                    "2024-01-15T12:00:00Z")))
             (message-str (coalton:coalton
                           (web3/siwe:create-siwe-message
                            (coalton:lisp web3/siwe:SiweMessage () msg)))))
        (assert (search "Chain ID: 137" message-str)))
      ;; Test Arbitrum (42161)
      (let* ((msg (coalton:coalton
                   (web3/siwe:make-siwe-message
                    "arbitrum.example.com"
                    (coalton:lisp web3/address:Address () addr)
                    "https://arbitrum.example.com"
                    42161
                    "arbitrumnonce"
                    "2024-01-15T12:00:00Z")))
             (message-str (coalton:coalton
                           (web3/siwe:create-siwe-message
                            (coalton:lisp web3/siwe:SiweMessage () msg)))))
        (assert (search "Chain ID: 42161" message-str)))))

  ;;; =========================================================================
  ;;; Parse Error Edge Cases
  ;;; =========================================================================

  (test-case "parse-siwe-message rejects empty string"
    (let ((result (coalton:coalton
                   (web3/siwe:parse-siwe-message ""))))
      (assert (result-err-p result))))

  (test-case "parse-siwe-message rejects missing address"
    (let ((result (coalton:coalton
                   (web3/siwe:parse-siwe-message
                    "example.com wants you to sign in with your Ethereum account:"))))
      (assert (result-err-p result))))

  (test-case "parse-siwe-message rejects invalid address"
    (let ((result (coalton:coalton
                   (web3/siwe:parse-siwe-message
                    "example.com wants you to sign in with your Ethereum account:
not-an-address

URI: https://example.com
Version: 1
Chain ID: 1
Nonce: abc12345
Issued At: 2024-01-15T12:00:00Z"))))
      (assert (result-err-p result)))))
