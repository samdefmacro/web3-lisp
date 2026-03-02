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
                    (web3/address:address-zero)))))
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

            ;; Reverse resolution: address -> vitalik.eth
            (test-case "lookup-address for vitalik"
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
