;;; ENS Resolver tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Unit Tests (no network required)
;;; =========================================================================

(defun run-ens-resolver-tests ()
  (format t "~%=== ENS Resolver Tests ===~%")

  ;; Test %is-zero-address helper
  (test-case "is-zero-address: zero address returns true"
    (let ((result (coalton:coalton
                   (web3/ens-resolver::%is-zero-address
                    web3/address:address-zero))))
      (assert (eq result coalton:True))))

  (test-case "is-zero-address: non-zero address returns false"
    (let* ((addr-result (coalton:coalton
                         (web3/address:address-from-hex
                          "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045")))
           (addr (result-value addr-result)))
      (let ((result (coalton:coalton
                     (web3/ens-resolver::%is-zero-address
                      (coalton:lisp web3/address:Address () addr)))))
        (assert (eq result coalton:False)))))

  (test-case "is-zero-address: address with single non-zero byte"
    (let* ((addr-result (coalton:coalton
                         (web3/address:address-from-hex
                          "0x0000000000000000000000000000000000000001")))
           (addr (result-value addr-result)))
      (let ((result (coalton:coalton
                     (web3/ens-resolver::%is-zero-address
                      (coalton:lisp web3/address:Address () addr)))))
        (assert (eq result coalton:False)))))

  ;;; =========================================================================
  ;;; Coin Type Constants
  ;;; =========================================================================

  (test-case "coin-type-btc = 0"
    (let ((val (coalton:coalton
                (web3/types:u256-to-integer web3/ens-resolver:coin-type-btc))))
      (assert (= val 0))))

  (test-case "coin-type-eth = 60"
    (let ((val (coalton:coalton
                (web3/types:u256-to-integer web3/ens-resolver:coin-type-eth))))
      (assert (= val 60))))

  (test-case "coin-type-sol = 501"
    (let ((val (coalton:coalton
                (web3/types:u256-to-integer web3/ens-resolver:coin-type-sol))))
      (assert (= val 501))))

  (test-case "coin-type-matic = 966"
    (let ((val (coalton:coalton
                (web3/types:u256-to-integer web3/ens-resolver:coin-type-matic))))
      (assert (= val 966))))

  ;;; =========================================================================
  ;;; Multi-chain Selector Test
  ;;; =========================================================================

  (test-case "addr(bytes32,uint256) selector = 0xf1cb7e06"
    (let ((selector (coalton:coalton web3/ens-resolver::resolver-addr-multichain-selector)))
      (assert (= (length selector) 4))
      (assert (= (aref selector 0) #xf1))
      (assert (= (aref selector 1) #xcb))
      (assert (= (aref selector 2) #x7e))
      (assert (= (aref selector 3) #x06))))

  ;;; =========================================================================
  ;;; Network-Dependent Tests (require WEB3_ENS_RPC env var)
  ;;; =========================================================================

  (let ((rpc-url (uiop:getenv "WEB3_ENS_RPC")))
    (if (and rpc-url (plusp (length rpc-url)))
        (progn
          (format t "~%  --- Live ENS tests (using ~A) ---~%" rpc-url)

          (let ((provider (coalton:coalton
                           (web3/provider:make-http-provider
                            (coalton:lisp coalton:String () rpc-url)))))

            ;; Forward resolution: vitalik.eth -> known address
            (test-case "resolve-name vitalik.eth"
              (let ((result (coalton:coalton
                             (web3/ens-resolver:resolve-name
                              (coalton:lisp web3/provider:HttpProvider () provider)
                              "vitalik.eth"))))
                (assert (is-ok result))
                (let ((opt (result-value result)))
                  (assert (optional-some-p opt))
                  (let* ((addr (result-value opt))
                         (hex (coalton:coalton
                               (web3/address:address-to-hex
                                (coalton:lisp web3/address:Address () addr)))))
                    (assert (string-equal (string-downcase hex)
                                          "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"))))))

            ;; Nonexistent name -> None
            (test-case "resolve-name nonexistent name"
              (let ((result (coalton:coalton
                             (web3/ens-resolver:resolve-name
                              (coalton:lisp web3/provider:HttpProvider () provider)
                              "nonexistent-name-xyz123456789.eth"))))
                (assert (is-ok result))
                (let ((opt (result-value result)))
                  (assert (optional-none-p opt)))))

            ;; Get resolver for vitalik.eth
            (test-case "get-resolver vitalik.eth"
              (let ((result (coalton:coalton
                             (web3/ens-resolver:get-resolver
                              (coalton:lisp web3/provider:HttpProvider () provider)
                              "vitalik.eth"))))
                (assert (is-ok result))
                (let ((opt (result-value result)))
                  (assert (optional-some-p opt)))))

            ;; Reverse resolution with forward confirmation: address -> vitalik.eth
            (test-case "lookup-address for vitalik (with forward confirmation)"
              (let* ((addr-result (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045")))
                     (addr (result-value addr-result))
                     (result (coalton:coalton
                              (web3/ens-resolver:lookup-address
                               (coalton:lisp web3/provider:HttpProvider () provider)
                               (coalton:lisp web3/address:Address () addr)))))
                (assert (is-ok result))
                (let ((opt (result-value result)))
                  (assert (optional-some-p opt))
                  (let ((name (result-value opt)))
                    (assert (string= name "vitalik.eth"))))))

            ;; Reverse resolution without confirmation
            (test-case "lookup-address-unchecked for vitalik"
              (let* ((addr-result (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045")))
                     (addr (result-value addr-result))
                     (result (coalton:coalton
                              (web3/ens-resolver:lookup-address-unchecked
                               (coalton:lisp web3/provider:HttpProvider () provider)
                               (coalton:lisp web3/address:Address () addr)))))
                (assert (is-ok result))
                (let ((opt (result-value result)))
                  (assert (optional-some-p opt))
                  (let ((name (result-value opt)))
                    (assert (string= name "vitalik.eth"))))))

            ;; Reverse resolution for unknown address -> None
            (test-case "lookup-address for unknown address"
              (let* ((addr-result (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0x0000000000000000000000000000000000000001")))
                     (addr (result-value addr-result))
                     (result (coalton:coalton
                              (web3/ens-resolver:lookup-address
                               (coalton:lisp web3/provider:HttpProvider () provider)
                               (coalton:lisp web3/address:Address () addr)))))
                (assert (is-ok result))
                (let ((opt (result-value result)))
                  (assert (optional-none-p opt)))))

            ;; Multi-coin: resolve ETH address for vitalik.eth
            (test-case "resolve-address vitalik.eth ETH coin type"
              (let ((result (coalton:coalton
                             (web3/ens-resolver:resolve-address
                              (coalton:lisp web3/provider:HttpProvider () provider)
                              "vitalik.eth"
                              web3/ens-resolver:coin-type-eth))))
                (assert (is-ok result))
                (let ((opt (result-value result)))
                  ;; ETH address should be set for vitalik.eth
                  (assert (optional-some-p opt))
                  (let ((addr-bytes (result-value opt)))
                    ;; ETH address is 20 bytes
                    (assert (= (length addr-bytes) 20))))))

            ;; Text record
            (test-case "get-text-record vitalik.eth url"
              (let ((result (coalton:coalton
                             (web3/ens-resolver:get-text-record
                              (coalton:lisp web3/provider:HttpProvider () provider)
                              "vitalik.eth"
                              "url"))))
                (assert (is-ok result))
                (let ((opt (result-value result)))
                  ;; url may or may not be set, just verify no error
                  (assert t))))))

        ;; No RPC URL - skip network tests
        (test-case "Note: Live ENS resolution tests require WEB3_ENS_RPC"
          (assert t)))))
