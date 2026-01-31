;;; Contract Abstraction tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Contract Tests
;;; =========================================================================

;; Sample ERC-20 ABI for testing
(defvar *erc20-abi*
  "[{\"type\":\"function\",\"name\":\"name\",\"inputs\":[],\"outputs\":[{\"name\":\"\",\"type\":\"string\"}],\"stateMutability\":\"view\"},
    {\"type\":\"function\",\"name\":\"symbol\",\"inputs\":[],\"outputs\":[{\"name\":\"\",\"type\":\"string\"}],\"stateMutability\":\"view\"},
    {\"type\":\"function\",\"name\":\"decimals\",\"inputs\":[],\"outputs\":[{\"name\":\"\",\"type\":\"uint8\"}],\"stateMutability\":\"view\"},
    {\"type\":\"function\",\"name\":\"totalSupply\",\"inputs\":[],\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"stateMutability\":\"view\"},
    {\"type\":\"function\",\"name\":\"balanceOf\",\"inputs\":[{\"name\":\"account\",\"type\":\"address\"}],\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"stateMutability\":\"view\"},
    {\"type\":\"function\",\"name\":\"transfer\",\"inputs\":[{\"name\":\"to\",\"type\":\"address\"},{\"name\":\"amount\",\"type\":\"uint256\"}],\"outputs\":[{\"name\":\"\",\"type\":\"bool\"}],\"stateMutability\":\"nonpayable\"},
    {\"type\":\"function\",\"name\":\"approve\",\"inputs\":[{\"name\":\"spender\",\"type\":\"address\"},{\"name\":\"amount\",\"type\":\"uint256\"}],\"outputs\":[{\"name\":\"\",\"type\":\"bool\"}],\"stateMutability\":\"nonpayable\"},
    {\"type\":\"function\",\"name\":\"allowance\",\"inputs\":[{\"name\":\"owner\",\"type\":\"address\"},{\"name\":\"spender\",\"type\":\"address\"}],\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"stateMutability\":\"view\"},
    {\"type\":\"function\",\"name\":\"transferFrom\",\"inputs\":[{\"name\":\"from\",\"type\":\"address\"},{\"name\":\"to\",\"type\":\"address\"},{\"name\":\"amount\",\"type\":\"uint256\"}],\"outputs\":[{\"name\":\"\",\"type\":\"bool\"}],\"stateMutability\":\"nonpayable\"},
    {\"type\":\"event\",\"name\":\"Transfer\",\"inputs\":[{\"name\":\"from\",\"type\":\"address\",\"indexed\":true},{\"name\":\"to\",\"type\":\"address\",\"indexed\":true},{\"name\":\"value\",\"type\":\"uint256\",\"indexed\":false}],\"anonymous\":false},
    {\"type\":\"event\",\"name\":\"Approval\",\"inputs\":[{\"name\":\"owner\",\"type\":\"address\",\"indexed\":true},{\"name\":\"spender\",\"type\":\"address\",\"indexed\":true},{\"name\":\"value\",\"type\":\"uint256\",\"indexed\":false}],\"anonymous\":false}]")

(defun run-contract-tests ()
  (format t "~%=== Contract Tests ===~%")

  ;;; =========================================================================
  ;;; Contract Creation Tests
  ;;; =========================================================================

  (test-case "contract-from-abi-json creates contract"
    (let* ((addr-result (coalton:coalton
                         (web3/address:address-from-hex
                          "0xdAC17F958D2ee523a2206206994597C13D831ec7")))
           (addr (result-value addr-result))
           (contract-result (coalton:coalton
                             (web3/contract:contract-from-abi-json
                              (coalton:lisp web3/address:Address () addr)
                              (coalton:lisp coalton:String () *erc20-abi*)))))
      (assert (result-ok-p contract-result))))

  (test-case "contract-from-abi-json rejects invalid JSON"
    (let* ((addr-result (coalton:coalton
                         (web3/address:address-from-hex
                          "0xdAC17F958D2ee523a2206206994597C13D831ec7")))
           (addr (result-value addr-result))
           (contract-result (coalton:coalton
                             (web3/contract:contract-from-abi-json
                              (coalton:lisp web3/address:Address () addr)
                              "not valid json"))))
      (assert (result-err-p contract-result))))

  ;;; =========================================================================
  ;;; Function Lookup Tests
  ;;; =========================================================================

  (test-case "get-function finds transfer"
    (let* ((addr-result (coalton:coalton
                         (web3/address:address-from-hex
                          "0xdAC17F958D2ee523a2206206994597C13D831ec7")))
           (addr (result-value addr-result))
           (contract (result-value
                      (coalton:coalton
                       (web3/contract:contract-from-abi-json
                        (coalton:lisp web3/address:Address () addr)
                        (coalton:lisp coalton:String () *erc20-abi*)))))
           (fn-opt (coalton:coalton
                    (web3/contract:get-function
                     (coalton:lisp web3/contract:Contract () contract)
                     "transfer"))))
      ;; Check it's Some, not None
      (assert (not (eq fn-opt 'coalton-prelude:None)))))

  (test-case "get-function returns None for unknown function"
    (let* ((addr-result (coalton:coalton
                         (web3/address:address-from-hex
                          "0xdAC17F958D2ee523a2206206994597C13D831ec7")))
           (addr (result-value addr-result))
           (contract (result-value
                      (coalton:coalton
                       (web3/contract:contract-from-abi-json
                        (coalton:lisp web3/address:Address () addr)
                        (coalton:lisp coalton:String () *erc20-abi*)))))
           (fn-opt (coalton:coalton
                    (web3/contract:get-function
                     (coalton:lisp web3/contract:Contract () contract)
                     "nonexistent"))))
      ;; Coalton None is a singleton - use equal for comparison
      (assert (equal fn-opt coalton-library/classes:None))))

  (test-case "get-event finds Transfer"
    (let* ((addr-result (coalton:coalton
                         (web3/address:address-from-hex
                          "0xdAC17F958D2ee523a2206206994597C13D831ec7")))
           (addr (result-value addr-result))
           (contract (result-value
                      (coalton:coalton
                       (web3/contract:contract-from-abi-json
                        (coalton:lisp web3/address:Address () addr)
                        (coalton:lisp coalton:String () *erc20-abi*)))))
           (ev-opt (coalton:coalton
                    (web3/contract:get-event
                     (coalton:lisp web3/contract:Contract () contract)
                     "Transfer"))))
      (assert (not (eq ev-opt 'coalton-prelude:None)))))

  ;;; =========================================================================
  ;;; List Functions/Events Tests
  ;;; =========================================================================

  (test-case "list-functions returns all function names"
    (let* ((addr-result (coalton:coalton
                         (web3/address:address-from-hex
                          "0xdAC17F958D2ee523a2206206994597C13D831ec7")))
           (addr (result-value addr-result))
           (contract (result-value
                      (coalton:coalton
                       (web3/contract:contract-from-abi-json
                        (coalton:lisp web3/address:Address () addr)
                        (coalton:lisp coalton:String () *erc20-abi*)))))
           (fn-names (coalton:coalton
                      (web3/contract:list-functions
                       (coalton:lisp web3/contract:Contract () contract))))
           (len (coalton:coalton
                 (coalton-library/list:length
                  (coalton:lisp (coalton:List coalton:String) () fn-names)))))
      ;; Should have 9 functions
      (assert (= len 9))))

  (test-case "list-events returns all event names"
    (let* ((addr-result (coalton:coalton
                         (web3/address:address-from-hex
                          "0xdAC17F958D2ee523a2206206994597C13D831ec7")))
           (addr (result-value addr-result))
           (contract (result-value
                      (coalton:coalton
                       (web3/contract:contract-from-abi-json
                        (coalton:lisp web3/address:Address () addr)
                        (coalton:lisp coalton:String () *erc20-abi*)))))
           (ev-names (coalton:coalton
                      (web3/contract:list-events
                       (coalton:lisp web3/contract:Contract () contract))))
           (len (coalton:coalton
                 (coalton-library/list:length
                  (coalton:lisp (coalton:List coalton:String) () ev-names)))))
      ;; Should have 2 events
      (assert (= len 2))))

  ;;; =========================================================================
  ;;; Encoding Tests
  ;;; =========================================================================

  (test-case "encode-function-call-by-name encodes transfer"
    (let* ((addr-result (coalton:coalton
                         (web3/address:address-from-hex
                          "0xdAC17F958D2ee523a2206206994597C13D831ec7")))
           (addr (result-value addr-result))
           (contract (result-value
                      (coalton:coalton
                       (web3/contract:contract-from-abi-json
                        (coalton:lisp web3/address:Address () addr)
                        (coalton:lisp coalton:String () *erc20-abi*)))))
           ;; Create args: to address and amount
           (to-addr-result (coalton:coalton
                            (web3/address:address-from-hex
                             "0x1111111111111111111111111111111111111111")))
           (to-addr (result-value to-addr-result))
           (to-bytes (coalton:coalton
                      (web3/address:address-bytes
                       (coalton:lisp web3/address:Address () to-addr))))
           (addr-val (coalton:coalton
                      (web3/abi:AbiAddressVal
                       (coalton:lisp web3/types:Bytes () to-bytes))))
           (amount-val (coalton:coalton
                        (web3/abi:AbiUintVal
                         (web3/types:u256-from-integer 1000000))))
           (args (list addr-val amount-val))
           (result (coalton:coalton
                    (web3/contract:encode-function-call-by-name
                     (coalton:lisp web3/contract:Contract () contract)
                     "transfer"
                     (coalton:lisp (coalton:List web3/abi:AbiValue) () args)))))
      (assert (result-ok-p result))
      (let ((calldata (result-value result)))
        ;; Should be 4 + 32 + 32 = 68 bytes
        (assert (= (length calldata) 68))
        ;; Check transfer selector
        (assert (= (aref calldata 0) #xa9))
        (assert (= (aref calldata 1) #x05))
        (assert (= (aref calldata 2) #x9c))
        (assert (= (aref calldata 3) #xbb)))))

  (test-case "encode-function-call-by-name fails for unknown function"
    (let* ((addr-result (coalton:coalton
                         (web3/address:address-from-hex
                          "0xdAC17F958D2ee523a2206206994597C13D831ec7")))
           (addr (result-value addr-result))
           (contract (result-value
                      (coalton:coalton
                       (web3/contract:contract-from-abi-json
                        (coalton:lisp web3/address:Address () addr)
                        (coalton:lisp coalton:String () *erc20-abi*)))))
           (result (coalton:coalton
                    (web3/contract:encode-function-call-by-name
                     (coalton:lisp web3/contract:Contract () contract)
                     "nonexistent"
                     coalton:Nil))))
      (assert (result-err-p result))))

  ;;; =========================================================================
  ;;; Call Builder Tests
  ;;; =========================================================================

  (test-case "call-builder creates builder for function"
    (let* ((addr-result (coalton:coalton
                         (web3/address:address-from-hex
                          "0xdAC17F958D2ee523a2206206994597C13D831ec7")))
           (addr (result-value addr-result))
           (contract (result-value
                      (coalton:coalton
                       (web3/contract:contract-from-abi-json
                        (coalton:lisp web3/address:Address () addr)
                        (coalton:lisp coalton:String () *erc20-abi*)))))
           (builder-result (coalton:coalton
                            (web3/contract:call-builder
                             (coalton:lisp web3/contract:Contract () contract)
                             "balanceOf"))))
      (assert (result-ok-p builder-result))))

  (test-case "call-builder with-arg and build-calldata"
    (let* ((addr-result (coalton:coalton
                         (web3/address:address-from-hex
                          "0xdAC17F958D2ee523a2206206994597C13D831ec7")))
           (addr (result-value addr-result))
           (contract (result-value
                      (coalton:coalton
                       (web3/contract:contract-from-abi-json
                        (coalton:lisp web3/address:Address () addr)
                        (coalton:lisp coalton:String () *erc20-abi*)))))
           (builder (result-value
                     (coalton:coalton
                      (web3/contract:call-builder
                       (coalton:lisp web3/contract:Contract () contract)
                       "balanceOf"))))
           ;; Create address argument
           (account-result (coalton:coalton
                            (web3/address:address-from-hex
                             "0x1111111111111111111111111111111111111111")))
           (account (result-value account-result))
           (account-bytes (coalton:coalton
                           (web3/address:address-bytes
                            (coalton:lisp web3/address:Address () account))))
           (addr-val (coalton:coalton
                      (web3/abi:AbiAddressVal
                       (coalton:lisp web3/types:Bytes () account-bytes))))
           ;; Add argument and build
           (builder-with-arg (coalton:coalton
                              (web3/contract:with-arg
                               (coalton:lisp web3/contract:CallBuilder () builder)
                               (coalton:lisp web3/abi:AbiValue () addr-val))))
           (calldata (coalton:coalton
                      (web3/contract:build-calldata
                       (coalton:lisp web3/contract:CallBuilder () builder-with-arg)))))
      ;; Should be 4 + 32 = 36 bytes
      (assert (= (length calldata) 36))
      ;; Check balanceOf selector (0x70a08231)
      (assert (= (aref calldata 0) #x70))
      (assert (= (aref calldata 1) #xa0))
      (assert (= (aref calldata 2) #x82))
      (assert (= (aref calldata 3) #x31))))

  (test-case "build-call-request creates request with address"
    (let* ((contract-addr-result (coalton:coalton
                                  (web3/address:address-from-hex
                                   "0xdAC17F958D2ee523a2206206994597C13D831ec7")))
           (contract-addr (result-value contract-addr-result))
           (contract (result-value
                      (coalton:coalton
                       (web3/contract:contract-from-abi-json
                        (coalton:lisp web3/address:Address () contract-addr)
                        (coalton:lisp coalton:String () *erc20-abi*)))))
           (builder (result-value
                     (coalton:coalton
                      (web3/contract:call-builder
                       (coalton:lisp web3/contract:Contract () contract)
                       "totalSupply"))))
           (request (coalton:coalton
                     (web3/contract:build-call-request
                      (coalton:lisp web3/contract:CallBuilder () builder))))
           (request-data (coalton:coalton
                          (web3/contract:.call-data
                           (coalton:lisp web3/contract:CallRequest () request)))))
      ;; totalSupply has no args, so just 4 bytes selector
      (assert (= (length request-data) 4))
      ;; Check totalSupply selector (0x18160ddd)
      (assert (= (aref request-data 0) #x18))
      (assert (= (aref request-data 1) #x16))
      (assert (= (aref request-data 2) #x0d))
      (assert (= (aref request-data 3) #xdd))))

  ;;; =========================================================================
  ;;; Decoding Tests
  ;;; =========================================================================

  (test-case "decode-function-output-by-name decodes balanceOf"
    (let* ((addr-result (coalton:coalton
                         (web3/address:address-from-hex
                          "0xdAC17F958D2ee523a2206206994597C13D831ec7")))
           (addr (result-value addr-result))
           (contract (result-value
                      (coalton:coalton
                       (web3/contract:contract-from-abi-json
                        (coalton:lisp web3/address:Address () addr)
                        (coalton:lisp coalton:String () *erc20-abi*)))))
           ;; Create output data: 1000000 as uint256
           (output-data (make-array 32 :fill-pointer 32 :adjustable t :initial-element 0)))
      ;; Set value to 1000000 (0xF4240) in big-endian
      (setf (aref output-data 29) #x0f)
      (setf (aref output-data 30) #x42)
      (setf (aref output-data 31) #x40)
      (let ((result (coalton:coalton
                     (web3/contract:decode-function-output-by-name
                      (coalton:lisp web3/contract:Contract () contract)
                      "balanceOf"
                      (coalton:lisp web3/types:Bytes () output-data)))))
        (assert (result-ok-p result))
        (let ((values (result-value result)))
          (assert (= (coalton:coalton
                      (coalton-library/list:length
                       (coalton:lisp (coalton:List web3/abi:AbiValue) () values)))
                     1)))))))
