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
      (assert (result-err-p result)))))
