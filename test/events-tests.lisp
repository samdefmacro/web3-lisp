;;; Event log parsing tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Event Signature Tests
;;; =========================================================================

(defun run-events-tests ()
  (format t "~%=== Events Tests ===~%")

  ;; Test event signature hashing
  (test-case "ERC-20 Transfer topic = 0xddf252ad..."
    (let ((topic (coalton:coalton web3/events:erc20-transfer-topic)))
      (assert (= (length topic) 32))
      ;; keccak256("Transfer(address,address,uint256)")
      ;; = 0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef
      (assert (= (aref topic 0) #xdd))
      (assert (= (aref topic 1) #xf2))
      (assert (= (aref topic 2) #x52))
      (assert (= (aref topic 3) #xad))))

  (test-case "ERC-20 Approval topic = 0x8c5be1e5..."
    (let ((topic (coalton:coalton web3/events:erc20-approval-topic)))
      (assert (= (length topic) 32))
      ;; keccak256("Approval(address,address,uint256)")
      ;; = 0x8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925
      (assert (= (aref topic 0) #x8c))
      (assert (= (aref topic 1) #x5b))
      (assert (= (aref topic 2) #xe1))
      (assert (= (aref topic 3) #xe5))))

  (test-case "ERC-1155 TransferSingle topic"
    (let ((topic (coalton:coalton web3/events:erc1155-transfer-single-topic)))
      (assert (= (length topic) 32))
      ;; keccak256("TransferSingle(address,address,address,uint256,uint256)")
      ;; = 0xc3d58168c5ae7397731d063d5bbf3d657854427343f4c083240f7aacaa2d0f62
      (assert (= (aref topic 0) #xc3))
      (assert (= (aref topic 1) #xd5))
      (assert (= (aref topic 2) #x81))
      (assert (= (aref topic 3) #x68))))

  (test-case "ERC-1155 TransferBatch topic"
    (let ((topic (coalton:coalton web3/events:erc1155-transfer-batch-topic)))
      (assert (= (length topic) 32))
      ;; keccak256("TransferBatch(address,address,address,uint256[],uint256[])")
      ;; = 0x4a39dc06d4c0dbc64b70af90fd698a233a518aa5d07e595d983b8c0526c8f7fb
      (assert (= (aref topic 0) #x4a))
      (assert (= (aref topic 1) #x39))
      (assert (= (aref topic 2) #xdc))
      (assert (= (aref topic 3) #x06))))

  (test-case "ERC-721 ApprovalForAll topic"
    (let ((topic (coalton:coalton web3/events:erc721-approval-for-all-topic)))
      (assert (= (length topic) 32))
      ;; keccak256("ApprovalForAll(address,address,bool)")
      ;; = 0x17307eab39ab6107e8899845ad3d59bd9653f200f220920489ca2b5937696c31
      (assert (= (aref topic 0) #x17))
      (assert (= (aref topic 1) #x30))
      (assert (= (aref topic 2) #x7e))
      (assert (= (aref topic 3) #xab))))

  (test-case "ERC-1155 ApprovalForAll topic"
    (let ((topic (coalton:coalton web3/events:erc1155-approval-for-all-topic)))
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
      (assert (not (is-ok result)))))

  ;;; =========================================================================
  ;;; Additional Event Signature Tests
  ;;; =========================================================================

  (test-case "ERC-721 Transfer topic = 0xddf252ad..."
    ;; ERC-721 Transfer has same signature as ERC-20 for first 3 params
    ;; keccak256("Transfer(address,address,uint256)")
    (let ((topic (coalton:coalton web3/events:erc721-transfer-topic)))
      (assert (= (length topic) 32))
      (assert (= (aref topic 0) #xdd))
      (assert (= (aref topic 1) #xf2))
      (assert (= (aref topic 2) #x52))
      (assert (= (aref topic 3) #xad))))

  (test-case "ERC-721 Approval topic = 0x8c5be1e5..."
    ;; ERC-721 Approval also has same signature as ERC-20
    (let ((topic (coalton:coalton web3/events:erc721-approval-topic)))
      (assert (= (length topic) 32))
      (assert (= (aref topic 0) #x8c))
      (assert (= (aref topic 1) #x5b))))

  (test-case "event-signature for custom event"
    (let ((sig (coalton:coalton (web3/events:event-signature "Deposit(address,uint256)"))))
      (assert (= (length sig) 32))))

  (test-case "event-signature for complex event"
    ;; Event with multiple indexed parameters
    (let ((sig (coalton:coalton (web3/events:event-signature "Swap(address,address,int256,int256,uint160,uint128,int24)"))))
      (assert (= (length sig) 32))))

  (test-case "event-signature is deterministic"
    (let ((sig1 (coalton:coalton (web3/events:event-signature "MyEvent(uint256)")))
          (sig2 (coalton:coalton (web3/events:event-signature "MyEvent(uint256)"))))
      ;; Same signature should produce same topic
      (assert (= (aref sig1 0) (aref sig2 0)))
      (assert (= (aref sig1 31) (aref sig2 31)))))

  (test-case "different events have different signatures"
    (let ((sig1 (coalton:coalton (web3/events:event-signature "EventA(uint256)")))
          (sig2 (coalton:coalton (web3/events:event-signature "EventB(uint256)"))))
      ;; Different events should produce different topics
      (assert (not (and (= (aref sig1 0) (aref sig2 0))
                        (= (aref sig1 1) (aref sig2 1))
                        (= (aref sig1 2) (aref sig2 2))
                        (= (aref sig1 3) (aref sig2 3)))))))

  ;;; =========================================================================
  ;;; Additional Indexed Parameter Decoding Tests
  ;;; =========================================================================

  (test-case "decode-indexed-address with zero address"
    (let* ((topic (result-value (coalton:coalton
                                 (web3/types:hex-decode
                                  "0000000000000000000000000000000000000000000000000000000000000000"))))
           (result (coalton:coalton
                    (web3/events:decode-indexed-address
                     (coalton:lisp web3/types:Bytes () topic)))))
      (assert (is-ok result))
      (let ((addr-hex (coalton:coalton
                       (web3/address:address-to-hex
                        (coalton:lisp web3/address:Address () (result-value result))))))
        (assert (string-equal (string-downcase addr-hex)
                              "0x0000000000000000000000000000000000000000")))))

  (test-case "decode-indexed-address with max address"
    (let* ((topic (result-value (coalton:coalton
                                 (web3/types:hex-decode
                                  "000000000000000000000000ffffffffffffffffffffffffffffffffffffffff"))))
           (result (coalton:coalton
                    (web3/events:decode-indexed-address
                     (coalton:lisp web3/types:Bytes () topic)))))
      (assert (is-ok result))
      (let ((addr-hex (coalton:coalton
                       (web3/address:address-to-hex
                        (coalton:lisp web3/address:Address () (result-value result))))))
        (assert (string-equal (string-downcase addr-hex)
                              "0xffffffffffffffffffffffffffffffffffffffff")))))

  (test-case "decode-indexed-uint256 with zero"
    (let* ((topic (result-value (coalton:coalton
                                 (web3/types:hex-decode
                                  "0000000000000000000000000000000000000000000000000000000000000000"))))
           (result (coalton:coalton
                    (web3/events:decode-indexed-uint256
                     (coalton:lisp web3/types:Bytes () topic)))))
      (assert (is-ok result))
      (let ((value (result-value result)))
        (let ((expected (coalton:coalton web3/types:u256-zero)))
          (assert (coalton:coalton
                   (web3/types:u256-equal?
                    (coalton:lisp web3/types:U256 () value)
                    (coalton:lisp web3/types:U256 () expected))))))))

  (test-case "decode-indexed-uint256 with large value"
    ;; Max uint256 = 2^256 - 1 = all ff's
    (let* ((topic (result-value (coalton:coalton
                                 (web3/types:hex-decode
                                  "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"))))
           (result (coalton:coalton
                    (web3/events:decode-indexed-uint256
                     (coalton:lisp web3/types:Bytes () topic)))))
      (assert (is-ok result))
      (let ((value (result-value result)))
        (let ((expected (coalton:coalton web3/types:u256-max)))
          (assert (coalton:coalton
                   (web3/types:u256-equal?
                    (coalton:lisp web3/types:U256 () value)
                    (coalton:lisp web3/types:U256 () expected))))))))

  (test-case "decode-indexed-uint256 with 1 ETH in wei"
    ;; 1 ETH = 1e18 wei = 0xDE0B6B3A7640000
    (let* ((topic (result-value (coalton:coalton
                                 (web3/types:hex-decode
                                  "0000000000000000000000000000000000000000000000000de0b6b3a7640000"))))
           (result (coalton:coalton
                    (web3/events:decode-indexed-uint256
                     (coalton:lisp web3/types:Bytes () topic)))))
      (assert (is-ok result))
      (let ((value (result-value result)))
        (let ((expected (coalton:coalton (web3/types:u256-from-integer 1000000000000000000))))
          (assert (coalton:coalton
                   (web3/types:u256-equal?
                    (coalton:lisp web3/types:U256 () value)
                    (coalton:lisp web3/types:U256 () expected))))))))

  (test-case "decode-indexed-address rejects empty bytes"
    (let* ((topic (result-value (coalton:coalton (web3/types:hex-decode ""))))
           (result (coalton:coalton
                    (web3/events:decode-indexed-address
                     (coalton:lisp web3/types:Bytes () topic)))))
      (assert (not (is-ok result)))))

  (test-case "decode-indexed-uint256 rejects empty bytes"
    (let* ((topic (result-value (coalton:coalton (web3/types:hex-decode ""))))
           (result (coalton:coalton
                    (web3/events:decode-indexed-uint256
                     (coalton:lisp web3/types:Bytes () topic)))))
      (assert (not (is-ok result)))))

  ;;; =========================================================================
  ;;; matches-event-signature Tests
  ;;; =========================================================================

  (test-case "matches-event-signature? returns true for matching topic"
    ;; Create an EventLog with Transfer topic and test against Transfer signature
    (let* ((transfer-topic (coalton:coalton web3/events:erc20-transfer-topic))
           (zero-addr (result-value (coalton:coalton
                                     (web3/address:address-from-hex
                                      "0x0000000000000000000000000000000000000001"))))
           (empty-data (coalton:coalton web3/types:bytes-empty))
           ;; Create EventLog with Transfer topic
           (event-log (coalton:coalton
                       (web3/events:EventLog
                        (coalton:lisp web3/address:Address () zero-addr)
                        (coalton:Cons (coalton:lisp web3/types:Bytes () transfer-topic) coalton:Nil)
                        (coalton:lisp web3/types:Bytes () empty-data)
                        coalton-prelude:None
                        coalton-prelude:None
                        coalton-prelude:None)))
           ;; Test against Transfer signature - expect match
           (expected-topic (coalton:coalton (web3/events:event-signature "Transfer(address,address,uint256)")))
           (matches (coalton:coalton
                     (web3/events:matches-event-signature?
                      (coalton:lisp web3/types:Bytes () expected-topic)
                      (coalton:lisp web3/events:EventLog () event-log)))))
      (assert (eq matches coalton:True))))

  (test-case "matches-event-signature? returns false for non-matching topic"
    ;; Create EventLog with Transfer topic but test against Approval signature
    (let* ((transfer-topic (coalton:coalton web3/events:erc20-transfer-topic))
           (zero-addr (result-value (coalton:coalton
                                     (web3/address:address-from-hex
                                      "0x0000000000000000000000000000000000000001"))))
           (empty-data (coalton:coalton web3/types:bytes-empty))
           (event-log (coalton:coalton
                       (web3/events:EventLog
                        (coalton:lisp web3/address:Address () zero-addr)
                        (coalton:Cons (coalton:lisp web3/types:Bytes () transfer-topic) coalton:Nil)
                        (coalton:lisp web3/types:Bytes () empty-data)
                        coalton-prelude:None
                        coalton-prelude:None
                        coalton-prelude:None)))
           ;; Test against Approval signature - expect no match
           (expected-topic (coalton:coalton (web3/events:event-signature "Approval(address,address,uint256)")))
           (matches (coalton:coalton
                     (web3/events:matches-event-signature?
                      (coalton:lisp web3/types:Bytes () expected-topic)
                      (coalton:lisp web3/events:EventLog () event-log)))))
      (assert (eq matches coalton:False))))

  (test-case "matches-event-signature? with approval topic"
    ;; Create EventLog with Approval topic and test against Approval signature
    (let* ((approval-topic (coalton:coalton web3/events:erc20-approval-topic))
           (zero-addr (result-value (coalton:coalton
                                     (web3/address:address-from-hex
                                      "0x0000000000000000000000000000000000000001"))))
           (empty-data (coalton:coalton web3/types:bytes-empty))
           (event-log (coalton:coalton
                       (web3/events:EventLog
                        (coalton:lisp web3/address:Address () zero-addr)
                        (coalton:Cons (coalton:lisp web3/types:Bytes () approval-topic) coalton:Nil)
                        (coalton:lisp web3/types:Bytes () empty-data)
                        coalton-prelude:None
                        coalton-prelude:None
                        coalton-prelude:None)))
           ;; Test against Approval signature - expect match
           (expected-topic (coalton:coalton (web3/events:event-signature "Approval(address,address,uint256)")))
           (matches (coalton:coalton
                     (web3/events:matches-event-signature?
                      (coalton:lisp web3/types:Bytes () expected-topic)
                      (coalton:lisp web3/events:EventLog () event-log)))))
      (assert (eq matches coalton:True))))

  (test-case "matches-event-signature? returns false for empty topics"
    ;; EventLog with empty topics list should return False
    (let* ((zero-addr (result-value (coalton:coalton
                                     (web3/address:address-from-hex
                                      "0x0000000000000000000000000000000000000001"))))
           (empty-data (coalton:coalton web3/types:bytes-empty))
           ;; Create EventLog with NO topics (empty list)
           (event-log (coalton:coalton
                       (web3/events:EventLog
                        (coalton:lisp web3/address:Address () zero-addr)
                        coalton:Nil  ; Empty topics list
                        (coalton:lisp web3/types:Bytes () empty-data)
                        coalton-prelude:None
                        coalton-prelude:None
                        coalton-prelude:None)))
           (expected-topic (coalton:coalton (web3/events:event-signature "Transfer(address,address,uint256)")))
           (matches (coalton:coalton
                     (web3/events:matches-event-signature?
                      (coalton:lisp web3/types:Bytes () expected-topic)
                      (coalton:lisp web3/events:EventLog () event-log)))))
      (assert (eq matches coalton:False))))

  ;;; =========================================================================
  ;;; Additional Event Topic Tests
  ;;; =========================================================================

  (test-case "ERC-20 and ERC-721 Transfer topics are identical"
    ;; Both standards use Transfer(address,address,uint256)
    (let ((erc20-topic (coalton:coalton web3/events:erc20-transfer-topic))
          (erc721-topic (coalton:coalton web3/events:erc721-transfer-topic)))
      (assert (= (aref erc20-topic 0) (aref erc721-topic 0)))
      (assert (= (aref erc20-topic 1) (aref erc721-topic 1)))
      (assert (= (aref erc20-topic 31) (aref erc721-topic 31)))))

  (test-case "ERC-721 and ERC-1155 ApprovalForAll topics are identical"
    ;; Both use ApprovalForAll(address,address,bool)
    (let ((erc721-topic (coalton:coalton web3/events:erc721-approval-for-all-topic))
          (erc1155-topic (coalton:coalton web3/events:erc1155-approval-for-all-topic)))
      (assert (= (aref erc721-topic 0) (aref erc1155-topic 0)))
      (assert (= (aref erc721-topic 31) (aref erc1155-topic 31))))))
