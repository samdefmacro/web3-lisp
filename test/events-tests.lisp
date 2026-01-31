;;; Event log parsing tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Event Signature Tests
;;; =========================================================================

(defun run-events-tests ()
  (format t "~%=== Events Tests ===~%")

  ;; Test event signature hashing
  (test-case "ERC-20 Transfer topic = 0xddf252ad..."
    (let ((topic (coalton:coalton (web3/events:erc20-transfer-topic coalton:Unit))))
      (assert (= (length topic) 32))
      ;; keccak256("Transfer(address,address,uint256)")
      ;; = 0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef
      (assert (= (aref topic 0) #xdd))
      (assert (= (aref topic 1) #xf2))
      (assert (= (aref topic 2) #x52))
      (assert (= (aref topic 3) #xad))))

  (test-case "ERC-20 Approval topic = 0x8c5be1e5..."
    (let ((topic (coalton:coalton (web3/events:erc20-approval-topic coalton:Unit))))
      (assert (= (length topic) 32))
      ;; keccak256("Approval(address,address,uint256)")
      ;; = 0x8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925
      (assert (= (aref topic 0) #x8c))
      (assert (= (aref topic 1) #x5b))
      (assert (= (aref topic 2) #xe1))
      (assert (= (aref topic 3) #xe5))))

  (test-case "ERC-1155 TransferSingle topic"
    (let ((topic (coalton:coalton (web3/events:erc1155-transfer-single-topic coalton:Unit))))
      (assert (= (length topic) 32))
      ;; keccak256("TransferSingle(address,address,address,uint256,uint256)")
      ;; = 0xc3d58168c5ae7397731d063d5bbf3d657854427343f4c083240f7aacaa2d0f62
      (assert (= (aref topic 0) #xc3))
      (assert (= (aref topic 1) #xd5))
      (assert (= (aref topic 2) #x81))
      (assert (= (aref topic 3) #x68))))

  (test-case "ERC-1155 TransferBatch topic"
    (let ((topic (coalton:coalton (web3/events:erc1155-transfer-batch-topic coalton:Unit))))
      (assert (= (length topic) 32))
      ;; keccak256("TransferBatch(address,address,address,uint256[],uint256[])")
      ;; = 0x4a39dc06d4c0dbc64b70af90fd698a233a518aa5d07e595d983b8c0526c8f7fb
      (assert (= (aref topic 0) #x4a))
      (assert (= (aref topic 1) #x39))
      (assert (= (aref topic 2) #xdc))
      (assert (= (aref topic 3) #x06))))

  (test-case "ERC-721 ApprovalForAll topic"
    (let ((topic (coalton:coalton (web3/events:erc721-approval-for-all-topic coalton:Unit))))
      (assert (= (length topic) 32))
      ;; keccak256("ApprovalForAll(address,address,bool)")
      ;; = 0x17307eab39ab6107e8899845ad3d59bd9653f200f220920489ca2b5937696c31
      (assert (= (aref topic 0) #x17))
      (assert (= (aref topic 1) #x30))
      (assert (= (aref topic 2) #x7e))
      (assert (= (aref topic 3) #xab))))

  (test-case "ERC-1155 ApprovalForAll topic"
    (let ((topic (coalton:coalton (web3/events:erc1155-approval-for-all-topic coalton:Unit))))
      (assert (= (length topic) 32))
      ;; Same as ERC-721 ApprovalForAll since signature is identical
      (assert (= (aref topic 0) #x17))
      (assert (= (aref topic 1) #x30))))

  (test-case "event-signature computes keccak256"
    (let ((sig (coalton:coalton (web3/events:event-signature "Transfer(address,address,uint256)"))))
      (assert (= (length sig) 32))
      (assert (= (aref sig 0) #xdd))))

  (test-case "event-topic is alias for event-signature"
    (let ((topic (coalton:coalton (web3/events:event-topic "Transfer(address,address,uint256)"))))
      (assert (= (length topic) 32))
      (assert (= (aref topic 0) #xdd))))

  ;;; =========================================================================
  ;;; Decode Indexed Parameter Tests
  ;;; =========================================================================

  (test-case "decode-indexed-address extracts address from 32-byte topic"
    ;; Address 0xd8da6bf26964af9d7eed9e03e53415d37aa96045 padded to 32 bytes
    (let* ((topic (result-value (coalton:coalton
                                 (web3/types:hex-decode
                                  "000000000000000000000000d8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (result (coalton:coalton
                    (web3/events:decode-indexed-address
                     (coalton:lisp web3/types:Bytes () topic)))))
      (assert (is-ok result))
      (let ((addr (result-value result)))
        ;; Check it's the expected address
        (let ((addr-hex (coalton:coalton
                         (web3/address:address-to-hex
                          (coalton:lisp web3/address:Address () addr)))))
          (assert (string-equal addr-hex "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045"))))))

  (test-case "decode-indexed-uint256 extracts value from 32-byte topic"
    ;; Value 1000 (0x3e8) padded to 32 bytes
    (let* ((topic (result-value (coalton:coalton
                                 (web3/types:hex-decode
                                  "00000000000000000000000000000000000000000000000000000000000003e8"))))
           (result (coalton:coalton
                    (web3/events:decode-indexed-uint256
                     (coalton:lisp web3/types:Bytes () topic)))))
      (assert (is-ok result))
      (let ((value (result-value result)))
        (let ((expected (coalton:coalton (web3/types:u256-from-integer 1000))))
          (assert (coalton:coalton
                   (web3/types:u256-equal?
                    (coalton:lisp web3/types:U256 () value)
                    (coalton:lisp web3/types:U256 () expected))))))))

  (test-case "decode-indexed-address fails on short topic"
    (let* ((topic (result-value (coalton:coalton
                                 (web3/types:hex-decode "d8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (result (coalton:coalton
                    (web3/events:decode-indexed-address
                     (coalton:lisp web3/types:Bytes () topic)))))
      (assert (not (is-ok result)))))

  (test-case "decode-indexed-uint256 fails on short topic"
    (let* ((topic (result-value (coalton:coalton (web3/types:hex-decode "03e8"))))
           (result (coalton:coalton
                    (web3/events:decode-indexed-uint256
                     (coalton:lisp web3/types:Bytes () topic)))))
      (assert (not (is-ok result))))))
