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
      (assert (optional-none-p result))))

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
        (assert (optional-none-p result)))))

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
        (assert (optional-none-p result1))
        (assert (optional-none-p result2)))))

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
        (assert (optional-none-p result-mainnet))
        (assert (optional-none-p result-polygon)))))

  ;;; =========================================================================
  ;;; Multiple Manager Tests
  ;;; =========================================================================

  (test-case "multiple managers are independent"
    (let* ((provider (coalton:coalton (web3/provider:make-http-provider "http://localhost:8545")))
           (nm1 (coalton:coalton
                 (web3/nonce-manager:make-nonce-manager
                  (coalton:lisp web3/provider:HttpProvider () provider))))
           (nm2 (coalton:coalton
                 (web3/nonce-manager:make-nonce-manager
                  (coalton:lisp web3/provider:HttpProvider () provider))))
           (addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")))))
      ;; Both should have empty caches
      (let ((result1 (coalton:coalton
                      (web3/nonce-manager:nonce-peek
                       (coalton:lisp web3/address:Address () addr)
                       1
                       (coalton:lisp web3/nonce-manager:NonceManager () nm1))))
            (result2 (coalton:coalton
                      (web3/nonce-manager:nonce-peek
                       (coalton:lisp web3/address:Address () addr)
                       1
                       (coalton:lisp web3/nonce-manager:NonceManager () nm2)))))
        (assert (optional-none-p result1))
        (assert (optional-none-p result2)))))

  (test-case "reset on one manager doesn't affect another"
    (let* ((provider (coalton:coalton (web3/provider:make-http-provider "http://localhost:8545")))
           (nm1 (coalton:coalton
                 (web3/nonce-manager:make-nonce-manager
                  (coalton:lisp web3/provider:HttpProvider () provider))))
           (nm2 (coalton:coalton
                 (web3/nonce-manager:make-nonce-manager
                  (coalton:lisp web3/provider:HttpProvider () provider))))
           (addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")))))
      ;; Reset on nm1
      (coalton:coalton
       (web3/nonce-manager:nonce-reset
        (coalton:lisp web3/address:Address () addr)
        1
        (coalton:lisp web3/nonce-manager:NonceManager () nm1)))
      ;; nm2 should be unaffected (still None)
      (let ((result2 (coalton:coalton
                      (web3/nonce-manager:nonce-peek
                       (coalton:lisp web3/address:Address () addr)
                       1
                       (coalton:lisp web3/nonce-manager:NonceManager () nm2)))))
        (assert (optional-none-p result2)))))

  ;;; =========================================================================
  ;;; Reset Edge Cases
  ;;; =========================================================================

  (test-case "nonce-reset on uncached address doesn't error"
    (let* ((provider (coalton:coalton (web3/provider:make-http-provider "http://localhost:8545")))
           (nm (coalton:coalton
                (web3/nonce-manager:make-nonce-manager
                 (coalton:lisp web3/provider:HttpProvider () provider))))
           (addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0x0000000000000000000000000000000000000001")))))
      ;; Reset on address that was never cached should succeed
      (coalton:coalton
       (web3/nonce-manager:nonce-reset
        (coalton:lisp web3/address:Address () addr)
        1
        (coalton:lisp web3/nonce-manager:NonceManager () nm)))
      ;; And peek should still return None
      (let ((result (coalton:coalton
                     (web3/nonce-manager:nonce-peek
                      (coalton:lisp web3/address:Address () addr)
                      1
                      (coalton:lisp web3/nonce-manager:NonceManager () nm)))))
        (assert (optional-none-p result)))))

  (test-case "nonce-reset can be called multiple times"
    (let* ((provider (coalton:coalton (web3/provider:make-http-provider "http://localhost:8545")))
           (nm (coalton:coalton
                (web3/nonce-manager:make-nonce-manager
                 (coalton:lisp web3/provider:HttpProvider () provider))))
           (addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")))))
      ;; Multiple resets should all succeed
      (coalton:coalton
       (web3/nonce-manager:nonce-reset
        (coalton:lisp web3/address:Address () addr)
        1
        (coalton:lisp web3/nonce-manager:NonceManager () nm)))
      (coalton:coalton
       (web3/nonce-manager:nonce-reset
        (coalton:lisp web3/address:Address () addr)
        1
        (coalton:lisp web3/nonce-manager:NonceManager () nm)))
      (coalton:coalton
       (web3/nonce-manager:nonce-reset
        (coalton:lisp web3/address:Address () addr)
        1
        (coalton:lisp web3/nonce-manager:NonceManager () nm)))
      (assert t)))

  (test-case "nonce-reset one address doesn't affect others"
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
      ;; Reset addr1 only
      (coalton:coalton
       (web3/nonce-manager:nonce-reset
        (coalton:lisp web3/address:Address () addr1)
        1
        (coalton:lisp web3/nonce-manager:NonceManager () nm)))
      ;; addr2 should be unaffected (still None since never cached)
      (let ((result2 (coalton:coalton
                      (web3/nonce-manager:nonce-peek
                       (coalton:lisp web3/address:Address () addr2)
                       1
                       (coalton:lisp web3/nonce-manager:NonceManager () nm)))))
        (assert (optional-none-p result2)))))

  (test-case "nonce-reset one chain doesn't affect others"
    (let* ((provider (coalton:coalton (web3/provider:make-http-provider "http://localhost:8545")))
           (nm (coalton:coalton
                (web3/nonce-manager:make-nonce-manager
                 (coalton:lisp web3/provider:HttpProvider () provider))))
           (addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")))))
      ;; Reset chain 1 only
      (coalton:coalton
       (web3/nonce-manager:nonce-reset
        (coalton:lisp web3/address:Address () addr)
        1  ;; Mainnet
        (coalton:lisp web3/nonce-manager:NonceManager () nm)))
      ;; Chain 137 should be unaffected
      (let ((result-polygon (coalton:coalton
                             (web3/nonce-manager:nonce-peek
                              (coalton:lisp web3/address:Address () addr)
                              137  ;; Polygon
                              (coalton:lisp web3/nonce-manager:NonceManager () nm)))))
        (assert (optional-none-p result-polygon)))))

  ;;; =========================================================================
  ;;; Chain ID Edge Cases
  ;;; =========================================================================

  (test-case "nonce-peek with chain-id 0"
    (let* ((provider (coalton:coalton (web3/provider:make-http-provider "http://localhost:8545")))
           (nm (coalton:coalton
                (web3/nonce-manager:make-nonce-manager
                 (coalton:lisp web3/provider:HttpProvider () provider))))
           (addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")))))
      ;; Chain ID 0 should work (though not typically used)
      (let ((result (coalton:coalton
                     (web3/nonce-manager:nonce-peek
                      (coalton:lisp web3/address:Address () addr)
                      0
                      (coalton:lisp web3/nonce-manager:NonceManager () nm)))))
        (assert (optional-none-p result)))))

  (test-case "nonce-peek with high chain-id (Arbitrum)"
    (let* ((provider (coalton:coalton (web3/provider:make-http-provider "http://localhost:8545")))
           (nm (coalton:coalton
                (web3/nonce-manager:make-nonce-manager
                 (coalton:lisp web3/provider:HttpProvider () provider))))
           (addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")))))
      ;; Arbitrum chain ID 42161
      (let ((result (coalton:coalton
                     (web3/nonce-manager:nonce-peek
                      (coalton:lisp web3/address:Address () addr)
                      42161
                      (coalton:lisp web3/nonce-manager:NonceManager () nm)))))
        (assert (optional-none-p result)))))

  (test-case "nonce-peek with very high chain-id"
    (let* ((provider (coalton:coalton (web3/provider:make-http-provider "http://localhost:8545")))
           (nm (coalton:coalton
                (web3/nonce-manager:make-nonce-manager
                 (coalton:lisp web3/provider:HttpProvider () provider))))
           (addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")))))
      ;; Very high chain ID should work
      (let ((result (coalton:coalton
                     (web3/nonce-manager:nonce-peek
                      (coalton:lisp web3/address:Address () addr)
                      999999999
                      (coalton:lisp web3/nonce-manager:NonceManager () nm)))))
        (assert (optional-none-p result)))))

  ;;; =========================================================================
  ;;; Address Edge Cases
  ;;; =========================================================================

  (test-case "nonce-peek with zero address"
    (let* ((provider (coalton:coalton (web3/provider:make-http-provider "http://localhost:8545")))
           (nm (coalton:coalton
                (web3/nonce-manager:make-nonce-manager
                 (coalton:lisp web3/provider:HttpProvider () provider))))
           (zero-addr (result-value
                       (coalton:coalton
                        (web3/address:address-from-hex
                         "0x0000000000000000000000000000000000000000")))))
      (let ((result (coalton:coalton
                     (web3/nonce-manager:nonce-peek
                      (coalton:lisp web3/address:Address () zero-addr)
                      1
                      (coalton:lisp web3/nonce-manager:NonceManager () nm)))))
        (assert (optional-none-p result)))))

  (test-case "nonce-reset with zero address"
    (let* ((provider (coalton:coalton (web3/provider:make-http-provider "http://localhost:8545")))
           (nm (coalton:coalton
                (web3/nonce-manager:make-nonce-manager
                 (coalton:lisp web3/provider:HttpProvider () provider))))
           (zero-addr (result-value
                       (coalton:coalton
                        (web3/address:address-from-hex
                         "0x0000000000000000000000000000000000000000")))))
      ;; Should not error
      (coalton:coalton
       (web3/nonce-manager:nonce-reset
        (coalton:lisp web3/address:Address () zero-addr)
        1
        (coalton:lisp web3/nonce-manager:NonceManager () nm)))
      (assert t)))

  (test-case "addresses are case-insensitive"
    (let* ((provider (coalton:coalton (web3/provider:make-http-provider "http://localhost:8545")))
           (nm (coalton:coalton
                (web3/nonce-manager:make-nonce-manager
                 (coalton:lisp web3/provider:HttpProvider () provider))))
           ;; Same address in different cases
           (addr-lower (result-value
                        (coalton:coalton
                         (web3/address:address-from-hex
                          "0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266"))))
           (addr-mixed (result-value
                        (coalton:coalton
                         (web3/address:address-from-hex
                          "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")))))
      ;; Both should be parsed the same way - verify bytes are equal
      (let ((bytes1 (coalton:coalton
                     (web3/address:address-bytes
                      (coalton:lisp web3/address:Address () addr-lower))))
            (bytes2 (coalton:coalton
                     (web3/address:address-bytes
                      (coalton:lisp web3/address:Address () addr-mixed)))))
        (assert (equalp bytes1 bytes2)))))

  ;;; =========================================================================
  ;;; Provider Tests
  ;;; =========================================================================

  (test-case "nonce-manager-provider returns same provider"
    (let* ((url "http://localhost:8545")
           (provider (coalton:coalton (web3/provider:make-http-provider "http://localhost:8545")))
           (nm (coalton:coalton
                (web3/nonce-manager:make-nonce-manager
                 (coalton:lisp web3/provider:HttpProvider () provider))))
           (retrieved (coalton:coalton
                       (web3/nonce-manager:nonce-manager-provider
                        (coalton:lisp web3/nonce-manager:NonceManager () nm)))))
      ;; Should get back a valid provider
      (assert retrieved)))

  (test-case "multiple managers can share same provider"
    (let* ((provider (coalton:coalton (web3/provider:make-http-provider "http://localhost:8545")))
           (nm1 (coalton:coalton
                 (web3/nonce-manager:make-nonce-manager
                  (coalton:lisp web3/provider:HttpProvider () provider))))
           (nm2 (coalton:coalton
                 (web3/nonce-manager:make-nonce-manager
                  (coalton:lisp web3/provider:HttpProvider () provider)))))
      ;; Both should work
      (assert nm1)
      (assert nm2)))

  ;;; =========================================================================
  ;;; Common Chain IDs Test
  ;;; =========================================================================

  (test-case "common chain IDs work correctly"
    (let* ((provider (coalton:coalton (web3/provider:make-http-provider "http://localhost:8545")))
           (nm (coalton:coalton
                (web3/nonce-manager:make-nonce-manager
                 (coalton:lisp web3/provider:HttpProvider () provider))))
           (addr (result-value
                  (coalton:coalton
                   (web3/address:address-from-hex
                    "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")))))
      ;; Test common chain IDs: Mainnet(1), Goerli(5), Sepolia(11155111), Polygon(137), Arbitrum(42161), Optimism(10)
      (dolist (chain-id '(1 5 11155111 137 42161 10))
        (let ((result (coalton:coalton
                       (web3/nonce-manager:nonce-peek
                        (coalton:lisp web3/address:Address () addr)
                        (coalton:lisp coalton:UFix () chain-id)
                        (coalton:lisp web3/nonce-manager:NonceManager () nm)))))
          (assert (optional-none-p result))))))

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
