;;; HD Wallet (BIP-39/BIP-32) tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; HD Wallet Tests
;;; =========================================================================

(defun run-hdwallet-tests ()
  (format t "~%=== HD Wallet Tests ===~%")

  ;; Ensure wordlist is loaded before running tests
  (handler-case
      (web3/hdwallet::ensure-wordlist)
    (error (e)
      (format t "  WARNING: Could not load BIP-39 wordlist: ~A~%" e)
      (format t "  Skipping HD wallet tests that require wordlist.~%")
      (return-from run-hdwallet-tests)))

  ;;; =========================================================================
  ;;; BIP-39 Mnemonic Tests
  ;;; =========================================================================

  (test-case "generate-mnemonic produces 12 words"
    (let ((result (coalton:coalton (web3/hdwallet:generate-mnemonic 12))))
      (assert (result-ok-p result))
      (let* ((mnemonic (result-value result))
             (words (uiop:split-string mnemonic :separator " ")))
        (assert (= (length words) 12)))))

  (test-case "generate-mnemonic produces 24 words"
    (let ((result (coalton:coalton (web3/hdwallet:generate-mnemonic 24))))
      (assert (result-ok-p result))
      (let* ((mnemonic (result-value result))
             (words (uiop:split-string mnemonic :separator " ")))
        (assert (= (length words) 24)))))

  (test-case "generate-mnemonic rejects invalid word count"
    (let ((result (coalton:coalton (web3/hdwallet:generate-mnemonic 13))))
      (assert (result-err-p result))))

  (test-case "validate-mnemonic accepts valid mnemonic"
    ;; Standard BIP-39 test vector
    ;; Note: Coalton's True/False map to CL's T/NIL
    (let ((valid (coalton:coalton
                  (web3/hdwallet:validate-mnemonic
                   "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"))))
      (assert (eq valid t))))

  (test-case "validate-mnemonic rejects invalid word"
    (let ((invalid (coalton:coalton
                    (web3/hdwallet:validate-mnemonic
                     "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon notaword"))))
      (assert (eq invalid nil))))

  (test-case "validate-mnemonic rejects wrong word count"
    (let ((invalid (coalton:coalton
                    (web3/hdwallet:validate-mnemonic
                     "abandon abandon abandon"))))
      (assert (eq invalid nil))))

  ;;; =========================================================================
  ;;; Mnemonic to Seed Tests
  ;;; =========================================================================

  (test-case "mnemonic-to-seed produces 64 bytes"
    (let ((seed (coalton:coalton
                 (web3/hdwallet:mnemonic-to-seed
                  "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
                  ""))))
      (assert (= (length seed) 64))))

  (test-case "mnemonic-to-seed with passphrase differs"
    (let* ((seed1 (coalton:coalton
                   (web3/hdwallet:mnemonic-to-seed
                    "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
                    "")))
           (seed2 (coalton:coalton
                   (web3/hdwallet:mnemonic-to-seed
                    "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
                    "mypassword"))))
      (assert (not (equalp seed1 seed2)))))

  ;;; =========================================================================
  ;;; BIP-32 Master Key Tests
  ;;; =========================================================================

  (test-case "master-key-from-seed produces valid key"
    (let* ((seed (coalton:coalton
                  (web3/hdwallet:mnemonic-to-seed
                   "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
                   "")))
           (result (coalton:coalton
                    (web3/hdwallet:master-key-from-seed
                     (coalton:lisp web3/types:Bytes () seed)))))
      (assert (result-ok-p result))
      (let ((key (result-value result)))
        ;; Private key should be 32 bytes
        (let ((priv (coalton:coalton
                     (web3/hdwallet:.hd-private-key
                      (coalton:lisp web3/hdwallet:HDKey () key)))))
          (assert (= (length priv) 32)))
        ;; Public key should be 33 bytes (compressed)
        (let ((pub (coalton:coalton
                    (web3/hdwallet:.hd-public-key
                     (coalton:lisp web3/hdwallet:HDKey () key)))))
          (assert (= (length pub) 33)))
        ;; Chain code should be 32 bytes
        (let ((chain (coalton:coalton
                      (web3/hdwallet:.hd-chain-code
                       (coalton:lisp web3/hdwallet:HDKey () key)))))
          (assert (= (length chain) 32))))))

  (test-case "master-key-from-seed rejects short seed"
    (let* ((short-seed (make-array 10 :fill-pointer 10 :adjustable t :initial-element 1))
           (result (coalton:coalton
                    (web3/hdwallet:master-key-from-seed
                     (coalton:lisp web3/types:Bytes () short-seed)))))
      (assert (result-err-p result))))

  ;;; =========================================================================
  ;;; Key Derivation Tests
  ;;; =========================================================================

  (test-case "derive-child produces different keys for different indices"
    (let* ((seed (coalton:coalton
                  (web3/hdwallet:mnemonic-to-seed
                   "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
                   "")))
           (master (result-value
                    (coalton:coalton
                     (web3/hdwallet:master-key-from-seed
                      (coalton:lisp web3/types:Bytes () seed)))))
           (child0 (result-value
                    (coalton:coalton
                     (web3/hdwallet:derive-child
                      (coalton:lisp web3/hdwallet:HDKey () master)
                      0
                      coalton:False))))
           (child1 (result-value
                    (coalton:coalton
                     (web3/hdwallet:derive-child
                      (coalton:lisp web3/hdwallet:HDKey () master)
                      1
                      coalton:False))))
           (priv0 (coalton:coalton
                   (web3/hdwallet:.hd-private-key
                    (coalton:lisp web3/hdwallet:HDKey () child0))))
           (priv1 (coalton:coalton
                   (web3/hdwallet:.hd-private-key
                    (coalton:lisp web3/hdwallet:HDKey () child1)))))
      (assert (not (equalp priv0 priv1)))))

  (test-case "derive-child hardened differs from normal"
    (let* ((seed (coalton:coalton
                  (web3/hdwallet:mnemonic-to-seed
                   "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
                   "")))
           (master (result-value
                    (coalton:coalton
                     (web3/hdwallet:master-key-from-seed
                      (coalton:lisp web3/types:Bytes () seed)))))
           (child-normal (result-value
                          (coalton:coalton
                           (web3/hdwallet:derive-child
                            (coalton:lisp web3/hdwallet:HDKey () master)
                            0
                            coalton:False))))
           (child-hardened (result-value
                            (coalton:coalton
                             (web3/hdwallet:derive-child
                              (coalton:lisp web3/hdwallet:HDKey () master)
                              0
                              coalton:True))))
           (priv-normal (coalton:coalton
                         (web3/hdwallet:.hd-private-key
                          (coalton:lisp web3/hdwallet:HDKey () child-normal))))
           (priv-hardened (coalton:coalton
                           (web3/hdwallet:.hd-private-key
                            (coalton:lisp web3/hdwallet:HDKey () child-hardened)))))
      (assert (not (equalp priv-normal priv-hardened)))))

  ;;; =========================================================================
  ;;; Path Derivation Tests
  ;;; =========================================================================

  (test-case "derive-path parses standard Ethereum path"
    (let* ((seed (coalton:coalton
                  (web3/hdwallet:mnemonic-to-seed
                   "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
                   "")))
           (master (result-value
                    (coalton:coalton
                     (web3/hdwallet:master-key-from-seed
                      (coalton:lisp web3/types:Bytes () seed)))))
           (derived (coalton:coalton
                     (web3/hdwallet:derive-path
                      (coalton:lisp web3/hdwallet:HDKey () master)
                      "m/44'/60'/0'/0/0"))))
      (assert (result-ok-p derived))))

  (test-case "derive-path produces consistent results"
    (let* ((seed (coalton:coalton
                  (web3/hdwallet:mnemonic-to-seed
                   "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
                   "")))
           (master (result-value
                    (coalton:coalton
                     (web3/hdwallet:master-key-from-seed
                      (coalton:lisp web3/types:Bytes () seed)))))
           (key1 (result-value
                  (coalton:coalton
                   (web3/hdwallet:derive-path
                    (coalton:lisp web3/hdwallet:HDKey () master)
                    "m/44'/60'/0'/0/0"))))
           (key2 (result-value
                  (coalton:coalton
                   (web3/hdwallet:derive-path
                    (coalton:lisp web3/hdwallet:HDKey () master)
                    "m/44'/60'/0'/0/0"))))
           (priv1 (coalton:coalton
                   (web3/hdwallet:.hd-private-key
                    (coalton:lisp web3/hdwallet:HDKey () key1))))
           (priv2 (coalton:coalton
                   (web3/hdwallet:.hd-private-key
                    (coalton:lisp web3/hdwallet:HDKey () key2)))))
      (assert (equalp priv1 priv2))))

  ;;; =========================================================================
  ;;; Ethereum Helper Tests
  ;;; =========================================================================

  (test-case "ethereum-path generates correct path"
    (let ((path (coalton:coalton (web3/hdwallet:ethereum-path 0))))
      (assert (string= path "m/44'/60'/0'/0/0")))
    (let ((path (coalton:coalton (web3/hdwallet:ethereum-path 5))))
      (assert (string= path "m/44'/60'/0'/0/5"))))

  (test-case "ethereum-ledger-path generates correct path"
    (let ((path (coalton:coalton (web3/hdwallet:ethereum-ledger-path 0))))
      (assert (string= path "m/44'/60'/0'/0/0")))
    (let ((path (coalton:coalton (web3/hdwallet:ethereum-ledger-path 3))))
      (assert (string= path "m/44'/60'/3'/0/0"))))

  (test-case "derive-ethereum-key succeeds"
    (let* ((seed (coalton:coalton
                  (web3/hdwallet:mnemonic-to-seed
                   "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
                   "")))
           (result (coalton:coalton
                    (web3/hdwallet:derive-ethereum-key
                     (coalton:lisp web3/types:Bytes () seed)
                     0))))
      (assert (result-ok-p result))))

  (test-case "derive-ethereum-address produces valid address"
    (let* ((seed (coalton:coalton
                  (web3/hdwallet:mnemonic-to-seed
                   "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
                   "")))
           (result (coalton:coalton
                    (web3/hdwallet:derive-ethereum-address
                     (coalton:lisp web3/types:Bytes () seed)
                     0))))
      (assert (result-ok-p result))
      (let* ((addr (result-value result))
             (bytes (coalton:coalton
                     (web3/address:address-bytes
                      (coalton:lisp web3/address:Address () addr)))))
        ;; Ethereum address is 20 bytes
        (assert (= (length bytes) 20)))))

  (test-case "mnemonic-to-private-key succeeds"
    (let ((result (coalton:coalton
                   (web3/hdwallet:mnemonic-to-private-key
                    "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
                    0))))
      (assert (result-ok-p result))
      (let ((priv-key (result-value result)))
        (assert (= (length priv-key) 32)))))

  (test-case "mnemonic-to-address succeeds"
    (let ((result (coalton:coalton
                   (web3/hdwallet:mnemonic-to-address
                    "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
                    0))))
      (assert (result-ok-p result))))

  (test-case "different account indices produce different addresses"
    (let* ((result0 (coalton:coalton
                     (web3/hdwallet:mnemonic-to-address
                      "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
                      0)))
           (result1 (coalton:coalton
                     (web3/hdwallet:mnemonic-to-address
                      "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
                      1)))
           (addr0 (result-value result0))
           (addr1 (result-value result1))
           (bytes0 (coalton:coalton
                    (web3/address:address-bytes
                     (coalton:lisp web3/address:Address () addr0))))
           (bytes1 (coalton:coalton
                    (web3/address:address-bytes
                     (coalton:lisp web3/address:Address () addr1)))))
      (assert (not (equalp bytes0 bytes1))))))
