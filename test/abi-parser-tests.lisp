;;; ABI Parser tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; ABI Parser Tests
;;; =========================================================================

(defun run-abi-parser-tests ()
  (format t "~%=== ABI Parser Tests ===~%")

  ;;; =========================================================================
  ;;; Type String Parsing Tests
  ;;; =========================================================================

  (test-case "parse-abi-type: uint256"
    (let ((result (coalton:coalton
                   (web3/abi-parser:parse-abi-type "uint256"))))
      (assert (result-ok-p result))))

  (test-case "parse-abi-type: uint defaults to uint256"
    (let ((result (coalton:coalton
                   (web3/abi-parser:parse-abi-type "uint"))))
      (assert (result-ok-p result))))

  (test-case "parse-abi-type: uint8"
    (let ((result (coalton:coalton
                   (web3/abi-parser:parse-abi-type "uint8"))))
      (assert (result-ok-p result))))

  (test-case "parse-abi-type: int256"
    (let ((result (coalton:coalton
                   (web3/abi-parser:parse-abi-type "int256"))))
      (assert (result-ok-p result))))

  (test-case "parse-abi-type: address"
    (let ((result (coalton:coalton
                   (web3/abi-parser:parse-abi-type "address"))))
      (assert (result-ok-p result))))

  (test-case "parse-abi-type: bool"
    (let ((result (coalton:coalton
                   (web3/abi-parser:parse-abi-type "bool"))))
      (assert (result-ok-p result))))

  (test-case "parse-abi-type: string"
    (let ((result (coalton:coalton
                   (web3/abi-parser:parse-abi-type "string"))))
      (assert (result-ok-p result))))

  (test-case "parse-abi-type: bytes"
    (let ((result (coalton:coalton
                   (web3/abi-parser:parse-abi-type "bytes"))))
      (assert (result-ok-p result))))

  (test-case "parse-abi-type: bytes32"
    (let ((result (coalton:coalton
                   (web3/abi-parser:parse-abi-type "bytes32"))))
      (assert (result-ok-p result))))

  (test-case "parse-abi-type: bytes4"
    (let ((result (coalton:coalton
                   (web3/abi-parser:parse-abi-type "bytes4"))))
      (assert (result-ok-p result))))

  (test-case "parse-abi-type: address[] dynamic array"
    (let ((result (coalton:coalton
                   (web3/abi-parser:parse-abi-type "address[]"))))
      (assert (result-ok-p result))))

  (test-case "parse-abi-type: uint256[3] fixed array"
    (let ((result (coalton:coalton
                   (web3/abi-parser:parse-abi-type "uint256[3]"))))
      (assert (result-ok-p result))))

  (test-case "parse-abi-type: tuple (uint256,address)"
    (let ((result (coalton:coalton
                   (web3/abi-parser:parse-abi-type "(uint256,address)"))))
      (assert (result-ok-p result))))

  (test-case "parse-abi-type: nested array uint256[][]"
    (let ((result (coalton:coalton
                   (web3/abi-parser:parse-abi-type "uint256[][]"))))
      (assert (result-ok-p result))))

  (test-case "parse-abi-type: rejects invalid type"
    (let ((result (coalton:coalton
                   (web3/abi-parser:parse-abi-type "invalidtype"))))
      (assert (result-err-p result))))

  (test-case "parse-abi-type: rejects invalid uint size"
    (let ((result (coalton:coalton
                   (web3/abi-parser:parse-abi-type "uint257"))))
      (assert (result-err-p result))))

  ;;; =========================================================================
  ;;; JSON Parsing Tests
  ;;; =========================================================================

  (test-case "parse-abi-json: simple function"
    (let* ((json "[{\"type\":\"function\",\"name\":\"balanceOf\",\"inputs\":[{\"name\":\"account\",\"type\":\"address\"}],\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"stateMutability\":\"view\"}]")
           (result (coalton:coalton
                    (web3/abi-parser:parse-abi-json
                     (coalton:lisp coalton:String () json)))))
      (assert (result-ok-p result))
      (let ((items (result-value result)))
        (assert (= (coalton:coalton
                    (coalton-library/list:length
                     (coalton:lisp (coalton:List web3/abi-parser:AbiItem) () items)))
                   1)))))

  (test-case "parse-abi-json: event with indexed param"
    (let* ((json "[{\"type\":\"event\",\"name\":\"Transfer\",\"inputs\":[{\"name\":\"from\",\"type\":\"address\",\"indexed\":true},{\"name\":\"to\",\"type\":\"address\",\"indexed\":true},{\"name\":\"value\",\"type\":\"uint256\",\"indexed\":false}],\"anonymous\":false}]")
           (result (coalton:coalton
                    (web3/abi-parser:parse-abi-json
                     (coalton:lisp coalton:String () json)))))
      (assert (result-ok-p result))))

  (test-case "parse-function-json: transfer function"
    (let* ((json "{\"type\":\"function\",\"name\":\"transfer\",\"inputs\":[{\"name\":\"to\",\"type\":\"address\"},{\"name\":\"amount\",\"type\":\"uint256\"}],\"outputs\":[{\"name\":\"\",\"type\":\"bool\"}],\"stateMutability\":\"nonpayable\"}")
           (result (coalton:coalton
                    (web3/abi-parser:parse-function-json
                     (coalton:lisp coalton:String () json)))))
      (assert (result-ok-p result))
      ;; Check selector is correct (0xa9059cbb for transfer)
      (let* ((fn (result-value result))
             (selector (coalton:coalton
                        (web3/abi-parser:.fn-selector
                         (coalton:lisp web3/abi-parser:ParsedFunction () fn)))))
        (assert (= (aref selector 0) #xa9))
        (assert (= (aref selector 1) #x05))
        (assert (= (aref selector 2) #x9c))
        (assert (= (aref selector 3) #xbb)))))

  (test-case "parse-function-json: function name"
    (let* ((json "{\"type\":\"function\",\"name\":\"approve\",\"inputs\":[{\"name\":\"spender\",\"type\":\"address\"},{\"name\":\"amount\",\"type\":\"uint256\"}],\"outputs\":[{\"name\":\"\",\"type\":\"bool\"}]}")
           (result (coalton:coalton
                    (web3/abi-parser:parse-function-json
                     (coalton:lisp coalton:String () json)))))
      (assert (result-ok-p result))
      (let ((fn (result-value result)))
        (assert (string= (coalton:coalton
                          (web3/abi-parser:.fn-name
                           (coalton:lisp web3/abi-parser:ParsedFunction () fn)))
                         "approve")))))

  (test-case "parse-event-json: Transfer event topic"
    (let* ((json "{\"type\":\"event\",\"name\":\"Transfer\",\"inputs\":[{\"name\":\"from\",\"type\":\"address\",\"indexed\":true},{\"name\":\"to\",\"type\":\"address\",\"indexed\":true},{\"name\":\"value\",\"type\":\"uint256\",\"indexed\":false}]}")
           (result (coalton:coalton
                    (web3/abi-parser:parse-event-json
                     (coalton:lisp coalton:String () json)))))
      (assert (result-ok-p result))
      ;; Check topic is keccak256("Transfer(address,address,uint256)")
      ;; = 0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef
      (let* ((ev (result-value result))
             (topic (coalton:coalton
                     (web3/abi-parser:.event-topic
                      (coalton:lisp web3/abi-parser:ParsedEvent () ev)))))
        (assert (= (aref topic 0) #xdd))
        (assert (= (aref topic 1) #xf2))
        (assert (= (aref topic 2) #x52))
        (assert (= (aref topic 3) #xad)))))

  (test-case "parse-abi-json: multiple items"
    (let* ((json "[{\"type\":\"function\",\"name\":\"transfer\",\"inputs\":[{\"name\":\"to\",\"type\":\"address\"},{\"name\":\"amount\",\"type\":\"uint256\"}],\"outputs\":[{\"name\":\"\",\"type\":\"bool\"}]},{\"type\":\"function\",\"name\":\"approve\",\"inputs\":[{\"name\":\"spender\",\"type\":\"address\"},{\"name\":\"amount\",\"type\":\"uint256\"}],\"outputs\":[{\"name\":\"\",\"type\":\"bool\"}]},{\"type\":\"event\",\"name\":\"Transfer\",\"inputs\":[{\"name\":\"from\",\"type\":\"address\",\"indexed\":true},{\"name\":\"to\",\"type\":\"address\",\"indexed\":true},{\"name\":\"value\",\"type\":\"uint256\"}]}]")
           (result (coalton:coalton
                    (web3/abi-parser:parse-abi-json
                     (coalton:lisp coalton:String () json)))))
      (assert (result-ok-p result))
      (let ((items (result-value result)))
        (assert (= (coalton:coalton
                    (coalton-library/list:length
                     (coalton:lisp (coalton:List web3/abi-parser:AbiItem) () items)))
                   3)))))

  (test-case "parse-abi-json: constructor"
    (let* ((json "[{\"type\":\"constructor\",\"inputs\":[{\"name\":\"name\",\"type\":\"string\"},{\"name\":\"symbol\",\"type\":\"string\"}]}]")
           (result (coalton:coalton
                    (web3/abi-parser:parse-abi-json
                     (coalton:lisp coalton:String () json)))))
      (assert (result-ok-p result))))

  (test-case "parse-abi-json: receive and fallback"
    (let* ((json "[{\"type\":\"receive\"},{\"type\":\"fallback\"}]")
           (result (coalton:coalton
                    (web3/abi-parser:parse-abi-json
                     (coalton:lisp coalton:String () json)))))
      (assert (result-ok-p result))))

  (test-case "parse-abi-json: custom error"
    (let* ((json "[{\"type\":\"error\",\"name\":\"InsufficientBalance\",\"inputs\":[{\"name\":\"required\",\"type\":\"uint256\"},{\"name\":\"available\",\"type\":\"uint256\"}]}]")
           (result (coalton:coalton
                    (web3/abi-parser:parse-abi-json
                     (coalton:lisp coalton:String () json)))))
      (assert (result-ok-p result))))

  ;;; =========================================================================
  ;;; Signature Building Tests
  ;;; =========================================================================

  (test-case "build-function-signature: transfer(address,uint256)"
    ;; Use parsed JSON function instead of manually building params
    (let* ((json "{\"type\":\"function\",\"name\":\"transfer\",\"inputs\":[{\"name\":\"to\",\"type\":\"address\"},{\"name\":\"amount\",\"type\":\"uint256\"}],\"outputs\":[]}")
           (result (coalton:coalton
                    (web3/abi-parser:parse-function-json
                     (coalton:lisp coalton:String () json))))
           (fn (result-value result))
           (sig (coalton:coalton
                 (web3/abi-parser:build-function-signature
                  (web3/abi-parser:.fn-name
                   (coalton:lisp web3/abi-parser:ParsedFunction () fn))
                  (web3/abi-parser:.fn-inputs
                   (coalton:lisp web3/abi-parser:ParsedFunction () fn))))))
      (assert (string= sig "transfer(address,uint256)"))))

  (test-case "build-event-signature with array type"
    ;; Use parsed JSON event instead of manually building params
    (let* ((json "{\"type\":\"event\",\"name\":\"BatchMint\",\"inputs\":[{\"name\":\"ids\",\"type\":\"uint256[]\"}]}")
           (result (coalton:coalton
                    (web3/abi-parser:parse-event-json
                     (coalton:lisp coalton:String () json))))
           (ev (result-value result))
           (sig (coalton:coalton
                 (web3/abi-parser:build-event-signature
                  (web3/abi-parser:.event-name
                   (coalton:lisp web3/abi-parser:ParsedEvent () ev))
                  (web3/abi-parser:.event-inputs
                   (coalton:lisp web3/abi-parser:ParsedEvent () ev))))))
      (assert (string= sig "BatchMint(uint256[])"))))

  ;;; =========================================================================
  ;;; Calldata Encoding Tests
  ;;; =========================================================================

  (test-case "encode-function-call: builds correct calldata"
    (let* ((json "{\"type\":\"function\",\"name\":\"transfer\",\"inputs\":[{\"name\":\"to\",\"type\":\"address\"},{\"name\":\"amount\",\"type\":\"uint256\"}],\"outputs\":[{\"name\":\"\",\"type\":\"bool\"}]}")
           (fn-result (coalton:coalton
                       (web3/abi-parser:parse-function-json
                        (coalton:lisp coalton:String () json)))))
      (assert (result-ok-p fn-result))
      (let* ((fn (result-value fn-result))
             ;; Create test values - 20 bytes address all 0x11
             (to-addr (make-array 20 :fill-pointer 20 :adjustable t
                                     :initial-element #x11))
             ;; Build ABI values using Coalton
             (addr-val (coalton:coalton
                        (web3/abi:AbiAddressVal
                         (coalton:lisp web3/types:Bytes () to-addr))))
             (amount-val (coalton:coalton
                          (web3/abi:AbiUintVal
                           (web3/types:u256-from-integer 1000)))))
        ;; Build args list and encode
        (let* ((args (cl:list addr-val amount-val))
               (calldata (coalton:coalton
                          (web3/abi-parser:encode-function-call
                           (coalton:lisp web3/abi-parser:ParsedFunction () fn)
                           (coalton:lisp (coalton:List web3/abi:AbiValue) () args)))))
          ;; Should be 4 (selector) + 32 (address) + 32 (amount) = 68 bytes
          (assert (= (length calldata) 68))
          ;; First 4 bytes should be transfer selector
          (assert (= (aref calldata 0) #xa9))
          (assert (= (aref calldata 1) #x05))
          (assert (= (aref calldata 2) #x9c))
          (assert (= (aref calldata 3) #xbb))))))

  (test-case "decode-function-output: decodes uint256"
    (let* ((json "{\"type\":\"function\",\"name\":\"balanceOf\",\"inputs\":[{\"name\":\"account\",\"type\":\"address\"}],\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}]}")
           (fn-result (coalton:coalton
                       (web3/abi-parser:parse-function-json
                        (coalton:lisp coalton:String () json))))
           (fn (result-value fn-result))
           ;; Create output data: 1000 as uint256
           (output-data (make-array 32 :fill-pointer 32 :adjustable t :initial-element 0)))
      ;; Set value to 1000 (0x3e8) in big-endian
      (setf (aref output-data 30) #x03)
      (setf (aref output-data 31) #xe8)
      (let ((result (coalton:coalton
                     (web3/abi-parser:decode-function-output
                      (coalton:lisp web3/abi-parser:ParsedFunction () fn)
                      (coalton:lisp web3/types:Bytes () output-data)))))
        (assert (result-ok-p result))
        (let ((values (result-value result)))
          (assert (= (coalton:coalton
                      (coalton-library/list:length
                       (coalton:lisp (coalton:List web3/abi:AbiValue) () values)))
                     1)))))))
