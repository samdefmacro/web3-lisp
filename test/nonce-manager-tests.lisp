;;; Nonce Manager module tests - Pure Common Lisp
;;; Note: Some tests require a running Ethereum node

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Nonce Manager Tests
;;; =========================================================================

(defun run-nonce-manager-tests ()
  (format t "~%=== Nonce Manager Tests ===~%")

  ;;; =========================================================================
  ;;; Constructor Tests
  ;;; =========================================================================

  (test-case "make-nonce-manager creates manager"
    ;; Create a dummy provider (won't make actual calls in these tests)
    (let* ((provider (coalton:coalton (web3/provider:make-http-provider "http://localhost:8545")))
           (nm (coalton:coalton
                (web3/nonce-manager:make-nonce-manager
                 (coalton:lisp web3/provider:HttpProvider () provider)))))
      ;; Just verify we can create it
      (assert nm)))

  (test-case "nonce-manager-provider returns provider"
    (let* ((provider (coalton:coalton (web3/provider:make-http-provider "http://localhost:8545")))
           (nm (coalton:coalton
                (web3/nonce-manager:make-nonce-manager
                 (coalton:lisp web3/provider:HttpProvider () provider))))
           (retrieved (coalton:coalton
                       (web3/nonce-manager:nonce-manager-provider
                        (coalton:lisp web3/nonce-manager:NonceManager () nm)))))
      (assert retrieved)))

  ;;; =========================================================================
  ;;; nonce-peek Tests (no network calls)
  ;;; =========================================================================

  (test-case "nonce-peek returns None when not cached"
    (let* ((provider (coalton:coalton (web3/provider:make-http-provider "http://localhost:8545")))
           (nm (coalton:coalton
                (web3/nonce-manager:make-nonce-manager
                 (coalton:lisp web3/provider:HttpProvider () provider))))
           (addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (result (coalton:coalton
                    (web3/nonce-manager:nonce-peek
                     (coalton:lisp web3/address:Address () addr)
                     1
                     (coalton:lisp web3/nonce-manager:NonceManager () nm)))))
      ;; Should be None since nothing is cached
      (assert (typep result 'coalton-library/classes::optional/none))))

  ;;; =========================================================================
  ;;; nonce-reset Tests (no network calls)
  ;;; =========================================================================

  (test-case "nonce-reset clears cache"
    (let* ((provider (coalton:coalton (web3/provider:make-http-provider "http://localhost:8545")))
           (nm (coalton:coalton
                (web3/nonce-manager:make-nonce-manager
                 (coalton:lisp web3/provider:HttpProvider () provider))))
           (addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")))))
      ;; Reset should complete without error
      (coalton:coalton
       (web3/nonce-manager:nonce-reset
        (coalton:lisp web3/address:Address () addr)
        1
        (coalton:lisp web3/nonce-manager:NonceManager () nm)))
      ;; Peek should still return None
      (let ((result (coalton:coalton
                     (web3/nonce-manager:nonce-peek
                      (coalton:lisp web3/address:Address () addr)
                      1
                      (coalton:lisp web3/nonce-manager:NonceManager () nm)))))
        (assert (typep result 'coalton-library/classes::optional/none)))))

  ;;; =========================================================================
  ;;; Multi-Address Tests
  ;;; =========================================================================

  (test-case "nonce-peek handles different addresses independently"
    (let* ((provider (coalton:coalton (web3/provider:make-http-provider "http://localhost:8545")))
           (nm (coalton:coalton
                (web3/nonce-manager:make-nonce-manager
                 (coalton:lisp web3/provider:HttpProvider () provider))))
           (addr1 (result-value
                   (coalton:coalton
                    (web3/address:address-from-hex
                     "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (addr2 (result-value
                   (coalton:coalton
                    (web3/address:address-from-hex
                     "0x70997970C51812dc3A010C7d01b50e0d17dc79C8")))))
      ;; Both should return None
      (let ((result1 (coalton:coalton
                      (web3/nonce-manager:nonce-peek
                       (coalton:lisp web3/address:Address () addr1)
                       1
                       (coalton:lisp web3/nonce-manager:NonceManager () nm))))
            (result2 (coalton:coalton
                      (web3/nonce-manager:nonce-peek
                       (coalton:lisp web3/address:Address () addr2)
                       1
                       (coalton:lisp web3/nonce-manager:NonceManager () nm)))))
        (assert (typep result1 'coalton-library/classes::optional/none))
        (assert (typep result2 'coalton-library/classes::optional/none)))))

  ;;; =========================================================================
  ;;; Multi-Chain Tests
  ;;; =========================================================================

  (test-case "nonce-peek handles different chains independently"
    (let* ((provider (coalton:coalton (web3/provider:make-http-provider "http://localhost:8545")))
           (nm (coalton:coalton
                (web3/nonce-manager:make-nonce-manager
                 (coalton:lisp web3/provider:HttpProvider () provider))))
           (addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")))))
      ;; Same address on different chains should be independent
      (let ((result-mainnet (coalton:coalton
                             (web3/nonce-manager:nonce-peek
                              (coalton:lisp web3/address:Address () addr)
                              1  ;; Mainnet
                              (coalton:lisp web3/nonce-manager:NonceManager () nm))))
            (result-polygon (coalton:coalton
                             (web3/nonce-manager:nonce-peek
                              (coalton:lisp web3/address:Address () addr)
                              137  ;; Polygon
                              (coalton:lisp web3/nonce-manager:NonceManager () nm)))))
        (assert (typep result-mainnet 'coalton-library/classes::optional/none))
        (assert (typep result-polygon 'coalton-library/classes::optional/none)))))

  ;;; =========================================================================
  ;;; Network-Dependent Tests (commented out - require running node)
  ;;; =========================================================================

  ;; These tests require a running Ethereum node (e.g., Hardhat, Anvil, or actual node)
  ;; Uncomment and modify provider URL to test with an actual node

  ;; (test-case "nonce-get fetches from network"
  ;;   (let* ((provider (coalton:coalton (web3/provider:make-http-provider "http://localhost:8545")))
  ;;          (nm (coalton:coalton
  ;;               (web3/nonce-manager:make-nonce-manager
  ;;                (coalton:lisp web3/provider:HttpProvider () provider))))
  ;;          (addr (result-value
  ;;                 (coalton:coalton
  ;;                  (web3/address:address-from-hex
  ;;                   "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
  ;;          (result (coalton:coalton
  ;;                   (web3/nonce-manager:nonce-get
  ;;                    (coalton:lisp web3/address:Address () addr)
  ;;                    1
  ;;                    (coalton:lisp web3/nonce-manager:NonceManager () nm)))))
  ;;     (assert (result-ok-p result))))

  ;; (test-case "nonce-consume increments local nonce"
  ;;   (let* ((provider (coalton:coalton (web3/provider:make-http-provider "http://localhost:8545")))
  ;;          (nm (coalton:coalton
  ;;               (web3/nonce-manager:make-nonce-manager
  ;;                (coalton:lisp web3/provider:HttpProvider () provider))))
  ;;          (addr (result-value
  ;;                 (coalton:coalton
  ;;                  (web3/address:address-from-hex
  ;;                   "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")))))
  ;;     ;; First consume
  ;;     (let ((first-result (coalton:coalton
  ;;                          (web3/nonce-manager:nonce-consume
  ;;                           (coalton:lisp web3/address:Address () addr)
  ;;                           1
  ;;                           (coalton:lisp web3/nonce-manager:NonceManager () nm)))))
  ;;       (assert (result-ok-p first-result))
  ;;       (let ((first-nonce (result-value first-result)))
  ;;         ;; Second consume should be first + 1
  ;;         (let ((second-result (coalton:coalton
  ;;                               (web3/nonce-manager:nonce-consume
  ;;                                (coalton:lisp web3/address:Address () addr)
  ;;                                1
  ;;                                (coalton:lisp web3/nonce-manager:NonceManager () nm)))))
  ;;           (assert (result-ok-p second-result))
  ;;           (assert (= (result-value second-result) (1+ first-nonce))))))))

  ;; (test-case "nonce-sync resets delta"
  ;;   (let* ((provider (coalton:coalton (web3/provider:make-http-provider "http://localhost:8545")))
  ;;          (nm (coalton:coalton
  ;;               (web3/nonce-manager:make-nonce-manager
  ;;                (coalton:lisp web3/provider:HttpProvider () provider))))
  ;;          (addr (result-value
  ;;                 (coalton:coalton
  ;;                  (web3/address:address-from-hex
  ;;                   "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")))))
  ;;     ;; Consume a few nonces
  ;;     (coalton:coalton
  ;;      (web3/nonce-manager:nonce-consume
  ;;       (coalton:lisp web3/address:Address () addr)
  ;;       1
  ;;       (coalton:lisp web3/nonce-manager:NonceManager () nm)))
  ;;     (coalton:coalton
  ;;      (web3/nonce-manager:nonce-consume
  ;;       (coalton:lisp web3/address:Address () addr)
  ;;       1
  ;;       (coalton:lisp web3/nonce-manager:NonceManager () nm)))
  ;;     ;; Sync should reset to network value
  ;;     (let ((sync-result (coalton:coalton
  ;;                         (web3/nonce-manager:nonce-sync
  ;;                          (coalton:lisp web3/address:Address () addr)
  ;;                          1
  ;;                          (coalton:lisp web3/nonce-manager:NonceManager () nm)))))
  ;;       (assert (result-ok-p sync-result)))))

  (format t "  Note: Network-dependent tests require running Ethereum node~%"))
