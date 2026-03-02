;;; ERC-1155 module tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; ERC-1155 Function Selector Tests
;;; =========================================================================

(defun run-erc1155-tests ()
  (format t "~%=== ERC-1155 Tests ===~%")

  ;; Function selector tests
  (test-case "selector uri(uint256) = 0x0e89341c"
    (let ((selector (coalton:coalton web3/erc1155:selector-uri)))
      (assert (= (length selector) 4))
      (assert (= (aref selector 0) #x0e))
      (assert (= (aref selector 1) #x89))
      (assert (= (aref selector 2) #x34))
      (assert (= (aref selector 3) #x1c))))

  (test-case "selector balanceOf(address,uint256) = 0x00fdd58e"
    (let ((selector (coalton:coalton web3/erc1155:selector-balance-of)))
      (assert (= (aref selector 0) #x00))
      (assert (= (aref selector 1) #xfd))
      (assert (= (aref selector 2) #xd5))
      (assert (= (aref selector 3) #x8e))))

  (test-case "selector balanceOfBatch(address[],uint256[]) = 0x4e1273f4"
    (let ((selector (coalton:coalton web3/erc1155:selector-balance-of-batch)))
      (assert (= (aref selector 0) #x4e))
      (assert (= (aref selector 1) #x12))
      (assert (= (aref selector 2) #x73))
      (assert (= (aref selector 3) #xf4))))

  (test-case "selector isApprovedForAll(address,address) = 0xe985e9c5"
    (let ((selector (coalton:coalton web3/erc1155:selector-is-approved-for-all)))
      (assert (= (aref selector 0) #xe9))
      (assert (= (aref selector 1) #x85))
      (assert (= (aref selector 2) #xe9))
      (assert (= (aref selector 3) #xc5))))

  (test-case "selector safeTransferFrom(address,address,uint256,uint256,bytes) = 0xf242432a"
    (let ((selector (coalton:coalton web3/erc1155:selector-safe-transfer-from)))
      (assert (= (aref selector 0) #xf2))
      (assert (= (aref selector 1) #x42))
      (assert (= (aref selector 2) #x43))
      (assert (= (aref selector 3) #x2a))))

  (test-case "selector safeBatchTransferFrom(...) = 0x2eb2c2d6"
    (let ((selector (coalton:coalton web3/erc1155:selector-safe-batch-transfer-from)))
      (assert (= (aref selector 0) #x2e))
      (assert (= (aref selector 1) #xb2))
      (assert (= (aref selector 2) #xc2))
      (assert (= (aref selector 3) #xd6))))

  (test-case "selector setApprovalForAll(address,bool) = 0xa22cb465"
    (let ((selector (coalton:coalton web3/erc1155:selector-set-approval-for-all)))
      (assert (= (aref selector 0) #xa2))
      (assert (= (aref selector 1) #x2c))
      (assert (= (aref selector 2) #xb4))
      (assert (= (aref selector 3) #x65))))

  ;;; =========================================================================
  ;;; Calldata Builder Tests
  ;;; =========================================================================

  (test-case "erc1155-safe-transfer-from-data builds correct calldata"
    (let* ((from-addr (result-value (coalton:coalton
                                     (web3/address:address-from-hex
                                      "0x1111111111111111111111111111111111111111"))))
           (to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0x2222222222222222222222222222222222222222"))))
           (token-id (coalton:coalton (web3/types:u256-from-integer 1)))
           (amount (coalton:coalton (web3/types:u256-from-integer 100)))
           (data (coalton:coalton web3/types:bytes-empty))
           (calldata (coalton:coalton
                      (web3/erc1155:erc1155-safe-transfer-from-data
                       (coalton:lisp web3/address:Address () from-addr)
                       (coalton:lisp web3/address:Address () to-addr)
                       (coalton:lisp web3/types:U256 () token-id)
                       (coalton:lisp web3/types:U256 () amount)
                       (coalton:lisp web3/types:Bytes () data)))))
      ;; Check selector
      (assert (= (aref calldata 0) #xf2))
      (assert (= (aref calldata 1) #x42))
      (assert (= (aref calldata 2) #x43))
      (assert (= (aref calldata 3) #x2a))
      ;; Should have selector + from + to + tokenId + amount + bytes offset + bytes length
      (assert (>= (length calldata) 164))))

  (test-case "erc1155-safe-batch-transfer-from-data builds correct calldata"
    (let* ((from-addr (result-value (coalton:coalton
                                     (web3/address:address-from-hex
                                      "0x1111111111111111111111111111111111111111"))))
           (to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0x2222222222222222222222222222222222222222"))))
           (token-ids (coalton:coalton
                       (coalton:Cons (web3/types:u256-from-integer 1)
                                     (coalton:Cons (web3/types:u256-from-integer 2) coalton:Nil))))
           (amounts (coalton:coalton
                     (coalton:Cons (web3/types:u256-from-integer 10)
                                   (coalton:Cons (web3/types:u256-from-integer 20) coalton:Nil))))
           (data (coalton:coalton web3/types:bytes-empty))
           (calldata (coalton:coalton
                      (web3/erc1155:erc1155-safe-batch-transfer-from-data
                       (coalton:lisp web3/address:Address () from-addr)
                       (coalton:lisp web3/address:Address () to-addr)
                       (coalton:lisp (coalton:List web3/types:U256) () token-ids)
                       (coalton:lisp (coalton:List web3/types:U256) () amounts)
                       (coalton:lisp web3/types:Bytes () data)))))
      ;; Check selector
      (assert (= (aref calldata 0) #x2e))
      (assert (= (aref calldata 1) #xb2))
      (assert (= (aref calldata 2) #xc2))
      (assert (= (aref calldata 3) #xd6))
      ;; Should be fairly long with arrays
      (assert (> (length calldata) 200))))

  (test-case "erc1155-set-approval-for-all-data with true"
    (let* ((operator (result-value (coalton:coalton
                                    (web3/address:address-from-hex
                                     "0x1234567890123456789012345678901234567890"))))
           (calldata (coalton:coalton
                      (web3/erc1155:erc1155-set-approval-for-all-data
                       (coalton:lisp web3/address:Address () operator)
                       coalton:True))))
      ;; 4 bytes selector + 32 bytes address + 32 bytes bool = 68 bytes
      (assert (= (length calldata) 68))
      ;; Check selector
      (assert (= (aref calldata 0) #xa2))
      (assert (= (aref calldata 1) #x2c))
      (assert (= (aref calldata 2) #xb4))
      (assert (= (aref calldata 3) #x65))
      ;; Bool true
      (assert (= (aref calldata 67) 1))))

  (test-case "erc1155-set-approval-for-all-data with false"
    (let* ((operator (result-value (coalton:coalton
                                    (web3/address:address-from-hex
                                     "0x1234567890123456789012345678901234567890"))))
           (calldata (coalton:coalton
                      (web3/erc1155:erc1155-set-approval-for-all-data
                       (coalton:lisp web3/address:Address () operator)
                       coalton:False))))
      (assert (= (length calldata) 68))
      ;; Bool false
      (assert (= (aref calldata 67) 0))))

  (test-case "erc1155-safe-transfer-from-data with data payload"
    (let* ((from-addr (result-value (coalton:coalton
                                     (web3/address:address-from-hex
                                      "0x1111111111111111111111111111111111111111"))))
           (to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0x2222222222222222222222222222222222222222"))))
           (token-id (coalton:coalton (web3/types:u256-from-integer 42)))
           (amount (coalton:coalton (web3/types:u256-from-integer 1)))
           (data (coalton:coalton
                  (web3/types:bytes-from-list
                   (coalton:Cons #xde (coalton:Cons #xad (coalton:Cons #xbe (coalton:Cons #xef coalton:Nil)))))))
           (calldata (coalton:coalton
                      (web3/erc1155:erc1155-safe-transfer-from-data
                       (coalton:lisp web3/address:Address () from-addr)
                       (coalton:lisp web3/address:Address () to-addr)
                       (coalton:lisp web3/types:U256 () token-id)
                       (coalton:lisp web3/types:U256 () amount)
                       (coalton:lisp web3/types:Bytes () data)))))
      ;; Should include the data bytes
      (assert (> (length calldata) 164))
      ;; Check selector
      (assert (= (aref calldata 0) #xf2))))

  (test-case "erc1155-safe-transfer-from-data with zero amount"
    (let* ((from-addr (result-value (coalton:coalton
                                     (web3/address:address-from-hex
                                      "0x1111111111111111111111111111111111111111"))))
           (to-addr (result-value (coalton:coalton
                                   (web3/address:address-from-hex
                                    "0x2222222222222222222222222222222222222222"))))
           (token-id (coalton:coalton (web3/types:u256-from-integer 1)))
           (amount (coalton:coalton web3/types:u256-zero))
           (data (coalton:coalton web3/types:bytes-empty))
           (calldata (coalton:coalton
                      (web3/erc1155:erc1155-safe-transfer-from-data
                       (coalton:lisp web3/address:Address () from-addr)
                       (coalton:lisp web3/address:Address () to-addr)
                       (coalton:lisp web3/types:U256 () token-id)
                       (coalton:lisp web3/types:U256 () amount)
                       (coalton:lisp web3/types:Bytes () data)))))
      ;; Check selector present
      (assert (= (aref calldata 0) #xf2))
      (assert (>= (length calldata) 164)))))
