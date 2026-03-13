;;; Revert reason decoding tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Revert Reason Decoding Tests
;;; =========================================================================

(defun run-revert-tests ()
  (format t "~%=== Revert Reason Decoding Tests ===~%")

  ;;; =========================================================================
  ;;; Selector Tests
  ;;; =========================================================================

  (test-case "error-selector is 0x08c379a2"
    (let ((sel (coalton:coalton web3/revert:error-selector)))
      (assert (= (length sel) 4))
      (assert (= (aref sel 0) #x08))
      (assert (= (aref sel 1) #xc3))
      (assert (= (aref sel 2) #x79))
      (assert (= (aref sel 3) #xa2))))

  (test-case "panic-selector is 0x4e487b71"
    (let ((sel (coalton:coalton web3/revert:panic-selector)))
      (assert (= (length sel) 4))
      (assert (= (aref sel 0) #x4e))
      (assert (= (aref sel 1) #x48))
      (assert (= (aref sel 2) #x7b))
      (assert (= (aref sel 3) #x71))))

  ;;; =========================================================================
  ;;; Error(string) Decoding Tests
  ;;; =========================================================================

  (test-case "decode-revert-reason decodes Error(string)"
    ;; 0x08c379a2 + ABI-encoded "Insufficient balance"
    ;; Build: selector + offset(32) + length(32) + string data(32)
    (let* ((hex-data (concatenate 'string
                       ;; Error(string) selector
                       "08c379a2"
                       ;; offset to string data (32 bytes = 0x20)
                       "0000000000000000000000000000000000000000000000000000000000000020"
                       ;; string length (20 = "Insufficient balance")
                       "0000000000000000000000000000000000000000000000000000000000000014"
                       ;; "Insufficient balance" padded to 32 bytes
                       "496e73756666696369656e742062616c616e6365000000000000000000000000"))
           (data (result-value (coalton:coalton (web3/types:hex-decode
                                                 (coalton:lisp coalton:String () hex-data)))))
           (reason (coalton:coalton
                    (web3/revert:decode-revert-reason
                     (coalton:lisp web3/types:Bytes () data))))
           (msg (coalton:coalton
                 (web3/revert:revert-reason-message
                  (coalton:lisp web3/revert:RevertReason () reason)))))
      (assert (string= msg "Insufficient balance"))))

  (test-case "decode-revert-reason decodes short Error(string)"
    (let* ((hex-data (concatenate 'string
                       "08c379a2"
                       "0000000000000000000000000000000000000000000000000000000000000020"
                       "0000000000000000000000000000000000000000000000000000000000000002"
                       ;; "no" padded
                       "6e6f000000000000000000000000000000000000000000000000000000000000"))
           (data (result-value (coalton:coalton (web3/types:hex-decode
                                                 (coalton:lisp coalton:String () hex-data)))))
           (reason (coalton:coalton
                    (web3/revert:decode-revert-reason
                     (coalton:lisp web3/types:Bytes () data))))
           (msg (coalton:coalton
                 (web3/revert:revert-reason-message
                  (coalton:lisp web3/revert:RevertReason () reason)))))
      (assert (string= msg "no"))))

  ;;; =========================================================================
  ;;; Panic(uint256) Decoding Tests
  ;;; =========================================================================

  (test-case "decode-revert-reason decodes Panic(1) assertion failure"
    (let* ((hex-data (concatenate 'string
                       "4e487b71"
                       ;; uint256 = 1 (assertion failure)
                       "0000000000000000000000000000000000000000000000000000000000000001"))
           (data (result-value (coalton:coalton (web3/types:hex-decode
                                                 (coalton:lisp coalton:String () hex-data)))))
           (reason (coalton:coalton
                    (web3/revert:decode-revert-reason
                     (coalton:lisp web3/types:Bytes () data))))
           (msg (coalton:coalton
                 (web3/revert:revert-reason-message
                  (coalton:lisp web3/revert:RevertReason () reason)))))
      (assert (search "assertion failure" msg))))

  (test-case "decode-revert-reason decodes Panic(17) overflow"
    (let* ((hex-data (concatenate 'string
                       "4e487b71"
                       ;; uint256 = 17 (0x11) overflow
                       "0000000000000000000000000000000000000000000000000000000000000011"))
           (data (result-value (coalton:coalton (web3/types:hex-decode
                                                 (coalton:lisp coalton:String () hex-data)))))
           (reason (coalton:coalton
                    (web3/revert:decode-revert-reason
                     (coalton:lisp web3/types:Bytes () data))))
           (msg (coalton:coalton
                 (web3/revert:revert-reason-message
                  (coalton:lisp web3/revert:RevertReason () reason)))))
      (assert (search "overflow" msg))))

  (test-case "decode-revert-reason decodes Panic(18) division by zero"
    (let* ((hex-data (concatenate 'string
                       "4e487b71"
                       ;; uint256 = 18 (0x12) division by zero
                       "0000000000000000000000000000000000000000000000000000000000000012"))
           (data (result-value (coalton:coalton (web3/types:hex-decode
                                                 (coalton:lisp coalton:String () hex-data)))))
           (reason (coalton:coalton
                    (web3/revert:decode-revert-reason
                     (coalton:lisp web3/types:Bytes () data))))
           (msg (coalton:coalton
                 (web3/revert:revert-reason-message
                  (coalton:lisp web3/revert:RevertReason () reason)))))
      (assert (search "division" msg))
      (assert (search "zero" msg))))

  (test-case "decode-revert-reason decodes Panic(50) array out of bounds"
    (let* ((hex-data (concatenate 'string
                       "4e487b71"
                       ;; uint256 = 50 (0x32)
                       "0000000000000000000000000000000000000000000000000000000000000032"))
           (data (result-value (coalton:coalton (web3/types:hex-decode
                                                 (coalton:lisp coalton:String () hex-data)))))
           (reason (coalton:coalton
                    (web3/revert:decode-revert-reason
                     (coalton:lisp web3/types:Bytes () data))))
           (msg (coalton:coalton
                 (web3/revert:revert-reason-message
                  (coalton:lisp web3/revert:RevertReason () reason)))))
      (assert (search "out of bounds" msg))))

  ;;; =========================================================================
  ;;; Panic Code Description Tests
  ;;; =========================================================================

  (test-case "panic-code-description covers all standard codes"
    (let ((codes-and-keywords '((0 "compiler") (1 "assertion") (17 "overflow")
                                (18 "division") (33 "enum") (34 "storage")
                                (49 "pop") (50 "out of bounds") (65 "memory")
                                (81 "uninitialized"))))
      (dolist (pair codes-and-keywords)
        (let* ((code (first pair))
               (keyword (second pair))
               (desc (coalton:coalton
                      (web3/revert:panic-code-description
                       (web3/types:u256-from-integer
                        (coalton:lisp coalton:Integer () code))))))
          (assert (search keyword desc))))))

  ;;; =========================================================================
  ;;; Edge Cases
  ;;; =========================================================================

  (test-case "decode-revert-reason returns RevertEmpty for empty data"
    (let* ((data (coalton:coalton (web3/types:make-bytes 0)))
           (reason (coalton:coalton
                    (web3/revert:decode-revert-reason
                     (coalton:lisp web3/types:Bytes () data))))
           (msg (coalton:coalton
                 (web3/revert:revert-reason-message
                  (coalton:lisp web3/revert:RevertReason () reason)))))
      (assert (string= msg "execution reverted"))))

  (test-case "decode-revert-reason returns RevertEmpty for data < 4 bytes"
    (let* ((short-data (result-value (coalton:coalton (web3/types:hex-decode "abcdef"))))
           (reason (coalton:coalton
                    (web3/revert:decode-revert-reason
                     (coalton:lisp web3/types:Bytes () short-data))))
           (msg (coalton:coalton
                 (web3/revert:revert-reason-message
                  (coalton:lisp web3/revert:RevertReason () reason)))))
      (assert (string= msg "execution reverted"))))

  (test-case "decode-revert-reason returns RevertCustom for unknown selector"
    (let* ((hex-data (concatenate 'string
                       "deadbeef"
                       "0000000000000000000000000000000000000000000000000000000000000001"))
           (data (result-value (coalton:coalton (web3/types:hex-decode
                                                 (coalton:lisp coalton:String () hex-data)))))
           (reason (coalton:coalton
                    (web3/revert:decode-revert-reason
                     (coalton:lisp web3/types:Bytes () data))))
           (msg (coalton:coalton
                 (web3/revert:revert-reason-message
                  (coalton:lisp web3/revert:RevertReason () reason)))))
      (assert (search "custom error" msg))
      (assert (search "deadbeef" msg))))

  ;;; =========================================================================
  ;;; Hex String Decoding Tests
  ;;; =========================================================================

  (test-case "decode-revert-hex handles 0x prefix"
    (let* ((hex-str (concatenate 'string
                      "0x08c379a2"
                      "0000000000000000000000000000000000000000000000000000000000000020"
                      "0000000000000000000000000000000000000000000000000000000000000005"
                      ;; "hello" padded
                      "68656c6c6f000000000000000000000000000000000000000000000000000000"))
           (reason (coalton:coalton
                    (web3/revert:decode-revert-hex
                     (coalton:lisp coalton:String () hex-str))))
           (msg (coalton:coalton
                 (web3/revert:revert-reason-message
                  (coalton:lisp web3/revert:RevertReason () reason)))))
      (assert (string= msg "hello"))))

  (test-case "decode-revert-hex handles invalid hex"
    (let* ((reason (coalton:coalton
                    (web3/revert:decode-revert-hex "not-hex-data")))
           (msg (coalton:coalton
                 (web3/revert:revert-reason-message
                  (coalton:lisp web3/revert:RevertReason () reason)))))
      (assert (string= msg "execution reverted"))))

  (test-case "decode-revert-hex handles empty string"
    (let* ((reason (coalton:coalton
                    (web3/revert:decode-revert-hex "")))
           (msg (coalton:coalton
                 (web3/revert:revert-reason-message
                  (coalton:lisp web3/revert:RevertReason () reason)))))
      (assert (string= msg "execution reverted")))))
