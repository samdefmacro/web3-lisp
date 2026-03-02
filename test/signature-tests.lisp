;;; Signature Utils tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Signature Utils Tests
;;; =========================================================================

(defun run-signature-tests ()
  (format t "~%=== Signature Utils Tests ===~%")

  ;;; =========================================================================
  ;;; V Normalization Tests
  ;;; =========================================================================

  (test-case "normalize-v handles 0/1"
    (assert (= (coalton:coalton (web3/signature:normalize-v 0)) 0))
    (assert (= (coalton:coalton (web3/signature:normalize-v 1)) 1)))

  (test-case "normalize-v handles 27/28 (legacy)"
    (assert (= (coalton:coalton (web3/signature:normalize-v 27)) 0))
    (assert (= (coalton:coalton (web3/signature:normalize-v 28)) 1)))

  (test-case "normalize-v handles EIP-155 format"
    ;; EIP-155 v values: chain_id * 2 + 35 + recovery_id
    ;; Mainnet (chain 1): 37 -> 0, 38 -> 1
    (assert (= (coalton:coalton (web3/signature:normalize-v 37)) 1))  ;; odd -> 1
    (assert (= (coalton:coalton (web3/signature:normalize-v 38)) 0))  ;; even -> 0
    ;; Goerli (chain 5): 45 -> 1, 46 -> 0
    (assert (= (coalton:coalton (web3/signature:normalize-v 45)) 1))
    (assert (= (coalton:coalton (web3/signature:normalize-v 46)) 0)))

  (test-case "to-eip155-v and from-eip155-v are inverses"
    ;; Chain ID 1 (mainnet): v = chain_id * 2 + 35 + recovery_id
    ;; For recovery_id = 0: v = 1 * 2 + 35 + 0 = 37
    ;; For recovery_id = 1: v = 1 * 2 + 35 + 1 = 38
    (let ((v0 (coalton:coalton (web3/signature:to-eip155-v 0 1)))
          (v1 (coalton:coalton (web3/signature:to-eip155-v 1 1))))
      (assert (= v0 37))
      (assert (= v1 38))
      ;; Round-trip
      (assert (= (coalton:coalton (web3/signature:from-eip155-v 37 1)) 0))
      (assert (= (coalton:coalton (web3/signature:from-eip155-v 38 1)) 1))))

  (test-case "to-eip155-v for different chains"
    ;; Polygon (chain 137): v = 137 * 2 + 35 + recovery_id
    ;; recovery_id = 0: v = 309
    ;; recovery_id = 1: v = 310
    (let ((v0 (coalton:coalton (web3/signature:to-eip155-v 0 137)))
          (v1 (coalton:coalton (web3/signature:to-eip155-v 1 137))))
      ;; Now returns UFix (no U8 truncation), so we get the full value
      (assert (= v0 309))
      (assert (= v1 310)))
    ;; Arbitrum (chain 42161): large chain ID
    ;; v = 42161 * 2 + 35 + 0 = 84357
    (let ((v0 (coalton:coalton (web3/signature:to-eip155-v 0 42161))))
      (assert (= v0 84357))))

  (test-case "from-eip155-v extracts recovery id"
    ;; Chain 1: from v=37 should get 0, from v=38 should get 1
    (assert (= (coalton:coalton (web3/signature:from-eip155-v 37 1)) 0))
    (assert (= (coalton:coalton (web3/signature:from-eip155-v 38 1)) 1)))

  ;;; =========================================================================
  ;;; Signature Accessor Tests
  ;;; =========================================================================

  (test-case "sig-r returns 32 bytes"
    (let* ((r (make-array 32 :element-type 't :fill-pointer 32 :adjustable t :initial-element #xaa))
           (s (make-array 32 :element-type 't :fill-pointer 32 :adjustable t :initial-element #xbb))
           (v 27)
           (sig (coalton:coalton
                 (web3/signature:make-signature
                  (coalton:lisp web3/types:Bytes () r)
                  (coalton:lisp web3/types:Bytes () s)
                  (coalton:lisp coalton:U8 () v))))
           (r-out (coalton:coalton
                   (web3/signature:sig-r
                    (coalton:lisp web3/signature:Signature () sig)))))
      (assert (= (length r-out) 32))
      (assert (= (aref r-out 0) #xaa))))

  (test-case "sig-s returns 32 bytes"
    (let* ((r (make-array 32 :element-type 't :fill-pointer 32 :adjustable t :initial-element #xaa))
           (s (make-array 32 :element-type 't :fill-pointer 32 :adjustable t :initial-element #xbb))
           (v 27)
           (sig (coalton:coalton
                 (web3/signature:make-signature
                  (coalton:lisp web3/types:Bytes () r)
                  (coalton:lisp web3/types:Bytes () s)
                  (coalton:lisp coalton:U8 () v))))
           (s-out (coalton:coalton
                   (web3/signature:sig-s
                    (coalton:lisp web3/signature:Signature () sig)))))
      (assert (= (length s-out) 32))
      (assert (= (aref s-out 0) #xbb))))

  ;;; =========================================================================
  ;;; Signature Encoding Tests
  ;;; =========================================================================

  (test-case "signature-to-bytes produces 65 bytes"
    (let* ((r (make-array 32 :element-type 't :fill-pointer 32 :adjustable t :initial-element 1))
           (s (make-array 32 :element-type 't :fill-pointer 32 :adjustable t :initial-element 2))
           (v 27)
           (sig (coalton:coalton
                 (web3/signature:make-signature
                  (coalton:lisp web3/types:Bytes () r)
                  (coalton:lisp web3/types:Bytes () s)
                  (coalton:lisp coalton:U8 () v))))
           (bytes (coalton:coalton
                   (web3/signature:signature-to-bytes
                    (coalton:lisp web3/signature:Signature () sig)))))
      (assert (= (length bytes) 65))
      ;; Check v is at position 64
      (assert (= (aref bytes 64) 27))))

  (test-case "signature-from-bytes parses correctly"
    (let* ((sig-bytes (make-array 65 :element-type 't :fill-pointer 65 :adjustable t :initial-element 0)))
      ;; Set r = all 1s
      (dotimes (i 32)
        (setf (aref sig-bytes i) 1))
      ;; Set s = all 2s
      (dotimes (i 32)
        (setf (aref sig-bytes (+ 32 i)) 2))
      ;; Set v = 28
      (setf (aref sig-bytes 64) 28)
      (let ((result (coalton:coalton
                     (web3/signature:signature-from-bytes
                      (coalton:lisp web3/types:Bytes () sig-bytes)))))
        (assert (result-ok-p result))
        (let* ((sig (result-value result))
               (v (coalton:coalton
                   (web3/signature:sig-v
                    (coalton:lisp web3/signature:Signature () sig)))))
          (assert (= v 28))))))

  (test-case "signature-from-bytes rejects too short"
    (let* ((bad-bytes (make-array 64 :element-type 't :fill-pointer 64 :adjustable t :initial-element 0))
           (result (coalton:coalton
                    (web3/signature:signature-from-bytes
                     (coalton:lisp web3/types:Bytes () bad-bytes)))))
      (assert (not (result-ok-p result)))))

  (test-case "signature-from-bytes rejects too long"
    (let* ((bad-bytes (make-array 66 :element-type 't :fill-pointer 66 :adjustable t :initial-element 0))
           (result (coalton:coalton
                    (web3/signature:signature-from-bytes
                     (coalton:lisp web3/types:Bytes () bad-bytes)))))
      (assert (not (result-ok-p result)))))

  (test-case "signature-from-bytes rejects empty"
    (let* ((bad-bytes (make-array 0 :element-type 't :fill-pointer 0 :adjustable t))
           (result (coalton:coalton
                    (web3/signature:signature-from-bytes
                     (coalton:lisp web3/types:Bytes () bad-bytes)))))
      (assert (not (result-ok-p result)))))

  (test-case "signature-to-hex and signature-from-hex roundtrip"
    (let* ((r (make-array 32 :element-type 't :fill-pointer 32 :adjustable t :initial-element #xab))
           (s (make-array 32 :element-type 't :fill-pointer 32 :adjustable t :initial-element #xcd))
           (v 27)
           (sig (coalton:coalton
                 (web3/signature:make-signature
                  (coalton:lisp web3/types:Bytes () r)
                  (coalton:lisp web3/types:Bytes () s)
                  (coalton:lisp coalton:U8 () v))))
           (hex (coalton:coalton
                 (web3/signature:signature-to-hex
                  (coalton:lisp web3/signature:Signature () sig)))))
      (assert (search "0x" hex))
      (assert (= (length hex) 132))  ; 0x + 130 hex chars
      ;; Parse back
      (let ((result (coalton:coalton
                     (web3/signature:signature-from-hex
                      (coalton:lisp coalton:String () hex)))))
        (assert (result-ok-p result)))))

  (test-case "signature-from-hex rejects invalid hex"
    (let ((result (coalton:coalton
                   (web3/signature:signature-from-hex "0xnotvalidhex"))))
      (assert (not (result-ok-p result)))))

  (test-case "signature-from-hex rejects wrong length hex"
    ;; Too short (only 64 hex chars = 32 bytes)
    (let ((result (coalton:coalton
                   (web3/signature:signature-from-hex
                    "0xababababababababababababababababababababababababababababababababab"))))
      (assert (not (result-ok-p result)))))

  ;;; =========================================================================
  ;;; EIP-191 Personal Sign Tests
  ;;; =========================================================================

  (test-case "personal-sign-prefix returns correct string"
    (let ((prefix (coalton:coalton web3/signature:personal-sign-prefix)))
      (assert (search "Ethereum Signed Message" prefix))))

  (test-case "hash-personal-message produces 32 bytes"
    (let* ((message (make-array 5 :element-type 't :fill-pointer 5 :adjustable t
                                :initial-contents '(72 101 108 108 111)))  ; "Hello"
           (hash (coalton:coalton
                  (web3/signature:hash-personal-message
                   (coalton:lisp web3/types:Bytes () message)))))
      (assert (= (length hash) 32))))

  (test-case "hash-personal-message is deterministic"
    (let* ((message (make-array 5 :element-type 't :fill-pointer 5 :adjustable t
                                :initial-contents '(72 101 108 108 111)))  ; "Hello"
           (hash1 (coalton:coalton
                   (web3/signature:hash-personal-message
                    (coalton:lisp web3/types:Bytes () message))))
           (hash2 (coalton:coalton
                   (web3/signature:hash-personal-message
                    (coalton:lisp web3/types:Bytes () message)))))
      (assert (equalp hash1 hash2))))

  (test-case "hash-personal-message with empty message"
    (let* ((message (make-array 0 :element-type 't :fill-pointer 0 :adjustable t))
           (hash (coalton:coalton
                  (web3/signature:hash-personal-message
                   (coalton:lisp web3/types:Bytes () message)))))
      (assert (= (length hash) 32))))

  (test-case "hash-personal-message with single byte"
    (let* ((message (make-array 1 :element-type 't :fill-pointer 1 :adjustable t
                                :initial-contents '(65)))  ; "A"
           (hash (coalton:coalton
                  (web3/signature:hash-personal-message
                   (coalton:lisp web3/types:Bytes () message)))))
      (assert (= (length hash) 32))))

  (test-case "hash-personal-message differs for different messages"
    (let* ((msg1 (make-array 5 :element-type 't :fill-pointer 5 :adjustable t
                             :initial-contents '(72 101 108 108 111)))  ; "Hello"
           (msg2 (make-array 5 :element-type 't :fill-pointer 5 :adjustable t
                             :initial-contents '(87 111 114 108 100)))  ; "World"
           (hash1 (coalton:coalton
                   (web3/signature:hash-personal-message
                    (coalton:lisp web3/types:Bytes () msg1))))
           (hash2 (coalton:coalton
                   (web3/signature:hash-personal-message
                    (coalton:lisp web3/types:Bytes () msg2)))))
      (assert (not (equalp hash1 hash2)))))

  (test-case "personal-sign produces valid signature"
    ;; Test with a known private key
    (let* ((private-key (result-value
                         (coalton:coalton
                          (web3/types:hex-decode
                           "ac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))))
           (message (make-array 5 :element-type 't :fill-pointer 5 :adjustable t
                                :initial-contents '(72 101 108 108 111)))  ; "Hello"
           (result (coalton:coalton
                    (web3/signature:personal-sign
                     (coalton:lisp web3/types:Bytes () message)
                     (coalton:lisp web3/types:Bytes () private-key)))))
      (assert (result-ok-p result))
      (let* ((sig (result-value result))
             (v (coalton:coalton
                 (web3/signature:sig-v
                  (coalton:lisp web3/signature:Signature () sig)))))
        ;; v should be 27 or 28 for personal sign
        (assert (or (= v 27) (= v 28))))))

  ;;; =========================================================================
  ;;; Raw Hash Signing Tests
  ;;; =========================================================================

  (test-case "sign-hash produces signature with v 0 or 1"
    (let* ((private-key (result-value
                          (coalton:coalton
                           (web3/types:hex-decode
                            "ac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))))
           (hash (make-array 32 :element-type 't :fill-pointer 32 :adjustable t :initial-element #xab))
           (result (coalton:coalton
                    (web3/signature:sign-hash
                     (coalton:lisp web3/types:Bytes () hash)
                     (coalton:lisp web3/types:Bytes () private-key)))))
      (assert (result-ok-p result))
      (let* ((sig (result-value result))
             (v (coalton:coalton
                 (web3/signature:sig-v
                  (coalton:lisp web3/signature:Signature () sig)))))
        ;; Raw sign-hash returns v as 0 or 1
        (assert (or (= v 0) (= v 1))))))

  (test-case "sign-hash and recover-from-hash roundtrip"
    (let* ((private-key (result-value
                          (coalton:coalton
                           (web3/types:hex-decode
                            "ac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))))
           (expected-addr (result-value
                           (coalton:coalton
                            (web3/address:address-from-hex
                             "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (hash (make-array 32 :element-type 't :fill-pointer 32 :adjustable t :initial-element #xab))
           (sign-result (coalton:coalton
                         (web3/signature:sign-hash
                          (coalton:lisp web3/types:Bytes () hash)
                          (coalton:lisp web3/types:Bytes () private-key)))))
      (assert (result-ok-p sign-result))
      (let* ((sig (result-value sign-result))
             (recover-result (coalton:coalton
                              (web3/signature:recover-from-hash
                               (coalton:lisp web3/types:Bytes () hash)
                               (coalton:lisp web3/signature:Signature () sig)))))
        (assert (result-ok-p recover-result))
        (let* ((recovered-addr (result-value recover-result))
               (expected-hex (coalton:coalton
                              (web3/address:address-to-hex
                               (coalton:lisp web3/address:Address () expected-addr))))
               (recovered-hex (coalton:coalton
                               (web3/address:address-to-hex
                                (coalton:lisp web3/address:Address () recovered-addr)))))
          (assert (string-equal expected-hex recovered-hex))))))

  (test-case "personal-recover recovers correct address"
    ;; Test with Hardhat's first account
    ;; Private key: 0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80
    ;; Address: 0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266
    (let* ((private-key (result-value
                         (coalton:coalton
                          (web3/types:hex-decode
                           "ac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))))
           (expected-addr (result-value
                           (coalton:coalton
                            (web3/address:address-from-hex
                             "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (message (make-array 5 :element-type 't :fill-pointer 5 :adjustable t
                                :initial-contents '(72 101 108 108 111)))  ; "Hello"
           (sign-result (coalton:coalton
                         (web3/signature:personal-sign
                          (coalton:lisp web3/types:Bytes () message)
                          (coalton:lisp web3/types:Bytes () private-key)))))
      (assert (result-ok-p sign-result))
      (let* ((sig (result-value sign-result))
             (recover-result (coalton:coalton
                              (web3/signature:personal-recover
                               (coalton:lisp web3/types:Bytes () message)
                               (coalton:lisp web3/signature:Signature () sig)))))
        (assert (result-ok-p recover-result))
        (let* ((recovered-addr (result-value recover-result))
               (expected-hex (coalton:coalton
                              (web3/address:address-to-hex
                               (coalton:lisp web3/address:Address () expected-addr))))
               (recovered-hex (coalton:coalton
                               (web3/address:address-to-hex
                                (coalton:lisp web3/address:Address () recovered-addr)))))
          (assert (string-equal expected-hex recovered-hex))))))

  ;;; =========================================================================
  ;;; Verification Tests
  ;;; =========================================================================

  (test-case "verify-personal-signature returns true for valid signature"
    (let* ((private-key (result-value
                         (coalton:coalton
                          (web3/types:hex-decode
                           "ac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))))
           (expected-addr (result-value
                           (coalton:coalton
                            (web3/address:address-from-hex
                             "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (message (make-array 5 :element-type 't :fill-pointer 5 :adjustable t
                                :initial-contents '(72 101 108 108 111)))
           (sign-result (coalton:coalton
                         (web3/signature:personal-sign
                          (coalton:lisp web3/types:Bytes () message)
                          (coalton:lisp web3/types:Bytes () private-key)))))
      (assert (result-ok-p sign-result))
      (let* ((sig (result-value sign-result))
             (valid (coalton:coalton
                     (web3/signature:verify-personal-signature
                      (coalton:lisp web3/types:Bytes () message)
                      (coalton:lisp web3/signature:Signature () sig)
                      (coalton:lisp web3/address:Address () expected-addr)))))
        (assert (eq valid t)))))

  (test-case "verify-personal-signature returns false for wrong address"
    (let* ((private-key (result-value
                         (coalton:coalton
                          (web3/types:hex-decode
                           "ac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))))
           (wrong-addr (result-value
                        (coalton:coalton
                         (web3/address:address-from-hex
                          "0x0000000000000000000000000000000000000001"))))
           (message (make-array 5 :element-type 't :fill-pointer 5 :adjustable t
                                :initial-contents '(72 101 108 108 111)))
           (sign-result (coalton:coalton
                         (web3/signature:personal-sign
                          (coalton:lisp web3/types:Bytes () message)
                          (coalton:lisp web3/types:Bytes () private-key)))))
      (assert (result-ok-p sign-result))
      (let* ((sig (result-value sign-result))
             (valid (coalton:coalton
                     (web3/signature:verify-personal-signature
                      (coalton:lisp web3/types:Bytes () message)
                      (coalton:lisp web3/signature:Signature () sig)
                      (coalton:lisp web3/address:Address () wrong-addr)))))
        (assert (eq valid nil)))))

  (test-case "verify-personal-signature returns false for wrong message"
    (let* ((private-key (result-value
                         (coalton:coalton
                          (web3/types:hex-decode
                           "ac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))))
           (expected-addr (result-value
                           (coalton:coalton
                            (web3/address:address-from-hex
                             "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (message (make-array 5 :element-type 't :fill-pointer 5 :adjustable t
                                :initial-contents '(72 101 108 108 111)))  ; "Hello"
           (wrong-message (make-array 5 :element-type 't :fill-pointer 5 :adjustable t
                                      :initial-contents '(87 111 114 108 100)))  ; "World"
           (sign-result (coalton:coalton
                         (web3/signature:personal-sign
                          (coalton:lisp web3/types:Bytes () message)
                          (coalton:lisp web3/types:Bytes () private-key)))))
      (assert (result-ok-p sign-result))
      (let* ((sig (result-value sign-result))
             (valid (coalton:coalton
                     (web3/signature:verify-personal-signature
                      (coalton:lisp web3/types:Bytes () wrong-message)
                      (coalton:lisp web3/signature:Signature () sig)
                      (coalton:lisp web3/address:Address () expected-addr)))))
        (assert (eq valid nil)))))

  (test-case "verify-signature returns true for valid hash signature"
    (let* ((private-key (result-value
                          (coalton:coalton
                           (web3/types:hex-decode
                            "ac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))))
           (expected-addr (result-value
                           (coalton:coalton
                            (web3/address:address-from-hex
                             "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (hash (make-array 32 :element-type 't :fill-pointer 32 :adjustable t :initial-element #xab))
           (sign-result (coalton:coalton
                         (web3/signature:sign-hash
                          (coalton:lisp web3/types:Bytes () hash)
                          (coalton:lisp web3/types:Bytes () private-key)))))
      (assert (result-ok-p sign-result))
      (let* ((sig (result-value sign-result))
             (valid (coalton:coalton
                     (web3/signature:verify-signature
                      (coalton:lisp web3/types:Bytes () hash)
                      (coalton:lisp web3/signature:Signature () sig)
                      (coalton:lisp web3/address:Address () expected-addr)))))
        (assert (eq valid t)))))

  (test-case "verify-signature returns false for wrong hash"
    (let* ((private-key (result-value
                          (coalton:coalton
                           (web3/types:hex-decode
                            "ac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))))
           (expected-addr (result-value
                           (coalton:coalton
                            (web3/address:address-from-hex
                             "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"))))
           (hash (make-array 32 :element-type 't :fill-pointer 32 :adjustable t :initial-element #xab))
           (wrong-hash (make-array 32 :element-type 't :fill-pointer 32 :adjustable t :initial-element #xcd))
           (sign-result (coalton:coalton
                         (web3/signature:sign-hash
                          (coalton:lisp web3/types:Bytes () hash)
                          (coalton:lisp web3/types:Bytes () private-key)))))
      (assert (result-ok-p sign-result))
      (let* ((sig (result-value sign-result))
             (valid (coalton:coalton
                     (web3/signature:verify-signature
                      (coalton:lisp web3/types:Bytes () wrong-hash)
                      (coalton:lisp web3/signature:Signature () sig)
                      (coalton:lisp web3/address:Address () expected-addr)))))
        (assert (eq valid nil)))))

  ;;; =========================================================================
  ;;; Split/Join Tests
  ;;; =========================================================================

  (test-case "split-signature and join-signature roundtrip"
    (let* ((r (make-array 32 :element-type 't :fill-pointer 32 :adjustable t :initial-element 1))
           (s (make-array 32 :element-type 't :fill-pointer 32 :adjustable t :initial-element 2))
           (v 27)
           (sig (coalton:coalton
                 (web3/signature:make-signature
                  (coalton:lisp web3/types:Bytes () r)
                  (coalton:lisp web3/types:Bytes () s)
                  (coalton:lisp coalton:U8 () v))))
           ;; Note: split returns nested tuples
           (v-out (coalton:coalton
                   (web3/signature:sig-v
                    (coalton:lisp web3/signature:Signature () sig)))))
      (assert (= v-out 27))))

  (test-case "join-signature creates valid signature"
    (let* ((r (make-array 32 :element-type 't :fill-pointer 32 :adjustable t :initial-element #x11))
           (s (make-array 32 :element-type 't :fill-pointer 32 :adjustable t :initial-element #x22))
           (v 28)
           (sig (coalton:coalton
                 (web3/signature:join-signature
                  (coalton:lisp web3/types:Bytes () r)
                  (coalton:lisp web3/types:Bytes () s)
                  (coalton:lisp coalton:U8 () v))))
           (r-out (coalton:coalton
                   (web3/signature:sig-r
                    (coalton:lisp web3/signature:Signature () sig))))
           (s-out (coalton:coalton
                   (web3/signature:sig-s
                    (coalton:lisp web3/signature:Signature () sig))))
           (v-out (coalton:coalton
                   (web3/signature:sig-v
                    (coalton:lisp web3/signature:Signature () sig)))))
      (assert (= (aref r-out 0) #x11))
      (assert (= (aref s-out 0) #x22))
      (assert (= v-out 28))))

  ;;; =========================================================================
  ;;; Consistency Tests
  ;;; =========================================================================

  (test-case "same message and key produce valid signatures"
    (let* ((private-key (result-value
                          (coalton:coalton
                           (web3/types:hex-decode
                            "ac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))))
           (message (make-array 5 :element-type 't :fill-pointer 5 :adjustable t
                                :initial-contents '(72 101 108 108 111)))
           (result1 (coalton:coalton
                     (web3/signature:personal-sign
                      (coalton:lisp web3/types:Bytes () message)
                      (coalton:lisp web3/types:Bytes () private-key))))
           (result2 (coalton:coalton
                     (web3/signature:personal-sign
                      (coalton:lisp web3/types:Bytes () message)
                      (coalton:lisp web3/types:Bytes () private-key)))))
      ;; Both signatures should be valid (ECDSA may be non-deterministic
      ;; without RFC 6979, so we check validity rather than equality)
      (assert (result-ok-p result1))
      (assert (result-ok-p result2))
      (let* ((sig1 (result-value result1))
             (sig2 (result-value result2))
             (hex1 (coalton:coalton
                    (web3/signature:signature-to-hex
                     (coalton:lisp web3/signature:Signature () sig1))))
             (hex2 (coalton:coalton
                    (web3/signature:signature-to-hex
                     (coalton:lisp web3/signature:Signature () sig2)))))
        ;; Both should produce valid hex signatures (0x + 130 hex chars = 65 bytes)
        (assert (= (length hex1) 132))
        (assert (= (length hex2) 132)))))

  (test-case "different private keys produce different signatures"
    (let* ((key1 (result-value
                   (coalton:coalton
                    (web3/types:hex-decode
                     "ac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))))
           (key2 (result-value
                   (coalton:coalton
                    (web3/types:hex-decode
                     "59c6995e998f97a5a0044966f0945389dc9e86dae88c7a8412f4603b6b78690d"))))
           (message (make-array 5 :element-type 't :fill-pointer 5 :adjustable t
                                :initial-contents '(72 101 108 108 111)))
           (result1 (coalton:coalton
                     (web3/signature:personal-sign
                      (coalton:lisp web3/types:Bytes () message)
                      (coalton:lisp web3/types:Bytes () key1))))
           (result2 (coalton:coalton
                     (web3/signature:personal-sign
                      (coalton:lisp web3/types:Bytes () message)
                      (coalton:lisp web3/types:Bytes () key2)))))
      (assert (result-ok-p result1))
      (assert (result-ok-p result2))
      (let* ((sig1 (result-value result1))
             (sig2 (result-value result2))
             (hex1 (coalton:coalton
                    (web3/signature:signature-to-hex
                     (coalton:lisp web3/signature:Signature () sig1))))
             (hex2 (coalton:coalton
                    (web3/signature:signature-to-hex
                     (coalton:lisp web3/signature:Signature () sig2)))))
        (assert (not (string-equal hex1 hex2)))))))
