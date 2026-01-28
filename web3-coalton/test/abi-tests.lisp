;;; ABI module tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; ABI Tests
;;; =========================================================================

(defun run-abi-tests ()
  (format t "~%=== ABI Tests ===~%")

  ;; Function selector tests
  (test-case "function selector - transfer(address,uint256) = 0xa9059cbb"
    (assert (eq (web3-tests:test-function-selector-transfer coalton:Unit) coalton:True)))

  (test-case "function selector - balanceOf(address)"
    ;; balanceOf(address) = 0x70a08231
    (let ((selector (coalton:coalton
                     (web3/abi:function-selector "balanceOf(address)"))))
      (assert (= (aref selector 0) #x70))
      (assert (= (aref selector 1) #xa0))
      (assert (= (aref selector 2) #x82))
      (assert (= (aref selector 3) #x31))))

  (test-case "function selector - approve(address,uint256)"
    ;; approve(address,uint256) = 0x095ea7b3
    (let ((selector (coalton:coalton
                     (web3/abi:function-selector "approve(address,uint256)"))))
      (assert (= (aref selector 0) #x09))
      (assert (= (aref selector 1) #x5e))
      (assert (= (aref selector 2) #xa7))
      (assert (= (aref selector 3) #xb3))))

  ;; Event topic tests
  (test-case "event topic - Transfer(address,address,uint256)"
    ;; Transfer(address,address,uint256) = 0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef
    (let ((topic (coalton:coalton
                  (web3/abi:event-topic "Transfer(address,address,uint256)"))))
      (assert (= (length topic) 32))
      (assert (= (aref topic 0) #xdd))
      (assert (= (aref topic 1) #xf2))
      (assert (= (aref topic 2) #x52))
      (assert (= (aref topic 3) #xad))))

  ;; Encoding tests
  (test-case "ABI encode uint256"
    (assert (eq (web3-tests:test-abi-encode-uint256 coalton:Unit) coalton:True)))

  (test-case "ABI encode bool"
    (assert (eq (web3-tests:test-abi-encode-bool coalton:Unit) coalton:True)))

  (test-case "ABI encode address"
    (assert (eq (web3-tests:test-abi-encode-address coalton:Unit) coalton:True)))

  (test-case "ABI encode bool(false)"
    (let ((encoded (coalton:coalton
                    (web3/abi:abi-encode
                     (coalton:Cons (web3/abi:AbiBoolVal coalton:False) coalton:Nil)))))
      (assert (= (length encoded) 32))
      (assert (= (aref encoded 31) 0))))

  (test-case "ABI encode multiple values (uint256, bool)"
    (let ((encoded (coalton:coalton
                    (web3/abi:abi-encode
                     (coalton:Cons (web3/abi:AbiUintVal (web3/types:u256-from-integer 42))
                                   (coalton:Cons (web3/abi:AbiBoolVal coalton:True) coalton:Nil))))))
      (assert (= (length encoded) 64))
      ;; First word: uint256(42)
      (assert (= (aref encoded 31) 42))
      ;; Second word: bool(true)
      (assert (= (aref encoded 63) 1))))

  (test-case "ABI encode with selector"
    ;; transfer(address, uint256) with selector prepended
    (let* ((selector (coalton:coalton
                      (web3/abi:function-selector "transfer(address,uint256)")))
           (addr-bytes (make-array 20 :fill-pointer 20 :adjustable t :initial-element 0))
           (encoded (coalton:coalton
                     (web3/abi:abi-encode-with-selector
                      (coalton:lisp web3/types:Bytes () selector)
                      (coalton:Cons
                       (web3/abi:AbiAddressVal (coalton:lisp web3/types:Bytes () addr-bytes))
                       (coalton:Cons
                        (web3/abi:AbiUintVal (web3/types:u256-from-integer 1000))
                        coalton:Nil))))))
      ;; 4 bytes selector + 32 bytes address + 32 bytes uint256
      (assert (= (length encoded) 68))
      ;; Check selector bytes
      (assert (= (aref encoded 0) #xa9))
      (assert (= (aref encoded 1) #x05))
      (assert (= (aref encoded 2) #x9c))
      (assert (= (aref encoded 3) #xbb)))))
