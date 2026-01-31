;;; ERC-721 module tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; ERC-721 Function Selector Tests
;;; =========================================================================

(defun run-erc721-tests ()
  (format t "~%=== ERC-721 Tests ===~%")

  ;; Metadata extension selectors
  (test-case "selector name() = 0x06fdde03"
    (let ((selector (coalton:coalton (web3/erc721:selector-name coalton:Unit))))
      (assert (= (length selector) 4))
      (assert (= (aref selector 0) #x06))
      (assert (= (aref selector 1) #xfd))
      (assert (= (aref selector 2) #xde))
      (assert (= (aref selector 3) #x03))))

  (test-case "selector symbol() = 0x95d89b41"
    (let ((selector (coalton:coalton (web3/erc721:selector-symbol coalton:Unit))))
      (assert (= (aref selector 0) #x95))
      (assert (= (aref selector 1) #xd8))
      (assert (= (aref selector 2) #x9b))
      (assert (= (aref selector 3) #x41))))

  (test-case "selector tokenURI(uint256) = 0xc87b56dd"
    (let ((selector (coalton:coalton (web3/erc721:selector-token-uri coalton:Unit))))
      (assert (= (aref selector 0) #xc8))
      (assert (= (aref selector 1) #x7b))
      (assert (= (aref selector 2) #x56))
      (assert (= (aref selector 3) #xdd))))

  ;; Core ERC-721 selectors
  (test-case "selector balanceOf(address) = 0x70a08231"
    (let ((selector (coalton:coalton (web3/erc721:selector-balance-of coalton:Unit))))
      (assert (= (aref selector 0) #x70))
      (assert (= (aref selector 1) #xa0))
      (assert (= (aref selector 2) #x82))
      (assert (= (aref selector 3) #x31))))

  (test-case "selector ownerOf(uint256) = 0x6352211e"
    (let ((selector (coalton:coalton (web3/erc721:selector-owner-of coalton:Unit))))
      (assert (= (aref selector 0) #x63))
      (assert (= (aref selector 1) #x52))
      (assert (= (aref selector 2) #x21))
      (assert (= (aref selector 3) #x1e))))

  (test-case "selector getApproved(uint256) = 0x081812fc"
    (let ((selector (coalton:coalton (web3/erc721:selector-get-approved coalton:Unit))))
      (assert (= (aref selector 0) #x08))
      (assert (= (aref selector 1) #x18))
      (assert (= (aref selector 2) #x12))
      (assert (= (aref selector 3) #xfc))))

  (test-case "selector isApprovedForAll(address,address) = 0xe985e9c5"
    (let ((selector (coalton:coalton (web3/erc721:selector-is-approved-for-all coalton:Unit))))
      (assert (= (aref selector 0) #xe9))
      (assert (= (aref selector 1) #x85))
      (assert (= (aref selector 2) #xe9))
      (assert (= (aref selector 3) #xc5))))

  (test-case "selector transferFrom(address,address,uint256) = 0x23b872dd"
    (let ((selector (coalton:coalton (web3/erc721:selector-transfer-from coalton:Unit))))
      (assert (= (aref selector 0) #x23))
      (assert (= (aref selector 1) #xb8))
      (assert (= (aref selector 2) #x72))
      (assert (= (aref selector 3) #xdd))))

  (test-case "selector safeTransferFrom(address,address,uint256) = 0x42842e0e"
    (let ((selector (coalton:coalton (web3/erc721:selector-safe-transfer-from coalton:Unit))))
      (assert (= (aref selector 0) #x42))
      (assert (= (aref selector 1) #x84))
      (assert (= (aref selector 2) #x2e))
      (assert (= (aref selector 3) #x0e))))

  (test-case "selector safeTransferFrom(address,address,uint256,bytes) = 0xb88d4fde"
    (let ((selector (coalton:coalton (web3/erc721:selector-safe-transfer-from-with-data coalton:Unit))))
      (assert (= (aref selector 0) #xb8))
      (assert (= (aref selector 1) #x8d))
      (assert (= (aref selector 2) #x4f))
      (assert (= (aref selector 3) #xde))))

  (test-case "selector approve(address,uint256) = 0x095ea7b3"
    (let ((selector (coalton:coalton (web3/erc721:selector-approve coalton:Unit))))
      (assert (= (aref selector 0) #x09))
      (assert (= (aref selector 1) #x5e))
      (assert (= (aref selector 2) #xa7))
      (assert (= (aref selector 3) #xb3))))

  (test-case "selector setApprovalForAll(address,bool) = 0xa22cb465"
    (let ((selector (coalton:coalton (web3/erc721:selector-set-approval-for-all coalton:Unit))))
      (assert (= (aref selector 0) #xa2))
      (assert (= (aref selector 1) #x2c))
      (assert (= (aref selector 2) #xb4))
      (assert (= (aref selector 3) #x65))))

  ;;; =========================================================================
  ;;; Calldata Builder Tests
  ;;; =========================================================================

  (test-case "erc721-transfer-from-data builds correct calldata"
    (let* ((from-addr (result-value (coalton:coalton
                                     (web3/address:address-from-hex
                                      "0x1111111111111111111111111111111111111111"))))
           (to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0x2222222222222222222222222222222222222222"))))
           (token-id (coalton:coalton (web3/types:u256-from-integer 1)))
           (calldata (coalton:coalton
                      (web3/erc721:erc721-transfer-from-data
                       (coalton:lisp web3/address:Address () from-addr)
                       (coalton:lisp web3/address:Address () to-addr)
                       (coalton:lisp web3/types:U256 () token-id)))))
      ;; 4 bytes selector + 32 bytes from + 32 bytes to + 32 bytes tokenId = 100 bytes
      (assert (= (length calldata) 100))
      ;; Check selector (transferFrom)
      (assert (= (aref calldata 0) #x23))
      (assert (= (aref calldata 1) #xb8))
      (assert (= (aref calldata 2) #x72))
      (assert (= (aref calldata 3) #xdd))))

  (test-case "erc721-safe-transfer-from-data builds correct calldata"
    (let* ((from-addr (result-value (coalton:coalton
                                     (web3/address:address-from-hex
                                      "0x1111111111111111111111111111111111111111"))))
           (to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0x2222222222222222222222222222222222222222"))))
           (token-id (coalton:coalton (web3/types:u256-from-integer 42)))
           (calldata (coalton:coalton
                      (web3/erc721:erc721-safe-transfer-from-data
                       (coalton:lisp web3/address:Address () from-addr)
                       (coalton:lisp web3/address:Address () to-addr)
                       (coalton:lisp web3/types:U256 () token-id)))))
      ;; 4 bytes selector + 32 bytes from + 32 bytes to + 32 bytes tokenId = 100 bytes
      (assert (= (length calldata) 100))
      ;; Check selector (safeTransferFrom 3 args)
      (assert (= (aref calldata 0) #x42))
      (assert (= (aref calldata 1) #x84))
      (assert (= (aref calldata 2) #x2e))
      (assert (= (aref calldata 3) #x0e))))

  (test-case "erc721-approve-data builds correct calldata"
    (let* ((to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0x1234567890123456789012345678901234567890"))))
           (token-id (coalton:coalton (web3/types:u256-from-integer 999)))
           (calldata (coalton:coalton
                      (web3/erc721:erc721-approve-data
                       (coalton:lisp web3/address:Address () to-addr)
                       (coalton:lisp web3/types:U256 () token-id)))))
      ;; 4 bytes selector + 32 bytes address + 32 bytes tokenId = 68 bytes
      (assert (= (length calldata) 68))
      ;; Check selector (approve)
      (assert (= (aref calldata 0) #x09))
      (assert (= (aref calldata 1) #x5e))
      (assert (= (aref calldata 2) #xa7))
      (assert (= (aref calldata 3) #xb3))))

  (test-case "erc721-set-approval-for-all-data with true"
    (let* ((operator (result-value (coalton:coalton
                                    (web3/address:address-from-hex
                                     "0x1234567890123456789012345678901234567890"))))
           (calldata (coalton:coalton
                      (web3/erc721:erc721-set-approval-for-all-data
                       (coalton:lisp web3/address:Address () operator)
                       coalton:True))))
      ;; 4 bytes selector + 32 bytes address + 32 bytes bool = 68 bytes
      (assert (= (length calldata) 68))
      ;; Check selector (setApprovalForAll)
      (assert (= (aref calldata 0) #xa2))
      (assert (= (aref calldata 1) #x2c))
      (assert (= (aref calldata 2) #xb4))
      (assert (= (aref calldata 3) #x65))
      ;; Bool true is encoded as 1 in last byte of second 32-byte word
      (assert (= (aref calldata 67) 1))))

  (test-case "erc721-set-approval-for-all-data with false"
    (let* ((operator (result-value (coalton:coalton
                                    (web3/address:address-from-hex
                                     "0x1234567890123456789012345678901234567890"))))
           (calldata (coalton:coalton
                      (web3/erc721:erc721-set-approval-for-all-data
                       (coalton:lisp web3/address:Address () operator)
                       coalton:False))))
      (assert (= (length calldata) 68))
      ;; Bool false is encoded as 0 in last byte
      (assert (= (aref calldata 67) 0))))

  (test-case "erc721-approve-data with token id 0"
    (let* ((to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0x0000000000000000000000000000000000000000"))))
           (token-id (coalton:coalton (web3/types:u256-zero coalton:Unit)))
           (calldata (coalton:coalton
                      (web3/erc721:erc721-approve-data
                       (coalton:lisp web3/address:Address () to-addr)
                       (coalton:lisp web3/types:U256 () token-id)))))
      (assert (= (length calldata) 68))
      ;; Token ID 0 should be all zeros in last 32 bytes
      (assert (= (aref calldata 67) 0))
      (assert (= (aref calldata 36) 0)))))
