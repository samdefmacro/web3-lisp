;;; Integration Tests Against Local Anvil Node
;;;
;;; Requires: WEB3_INTEGRATION=1 environment variable
;;; Requires: Anvil running on http://127.0.0.1:8545
;;;
;;; Start Anvil:  anvil
;;; Run tests:    WEB3_INTEGRATION=1 sbcl --non-interactive \
;;;                 --eval '(asdf:load-system "web3/tests")' \
;;;                 --eval '(web3-tests/runner:run-all-tests)'

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Constants
;;; =========================================================================

(defvar *anvil-url* "http://127.0.0.1:8545")
(defvar *anvil-chain-id* 31337)

;; Anvil default Account 0
(defvar *account0-pk-hex*
  "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80")
(defvar *account0-addr-hex*
  "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")

;; Anvil default Account 1
(defvar *account1-pk-hex*
  "0x59c6995e998f97a5a0044966f0945389dc9e86dae88c7a8412f4603b6b78690d")
(defvar *account1-addr-hex*
  "0x70997970C51812dc3A010C7d01b50e0d17dc79C8")

;; Pre-compiled minimal ERC20 (TestToken: name="TestToken", symbol="TT",
;; decimals=18, totalSupply=1000000*10^18 minted to deployer)
(defvar *test-erc20-bytecode-hex*
  "0x60806040526040518060400160405280600981526020017f54657374546f6b656e00000000000000000000000000000000000000000000008152505f908161004791906103c3565b506040518060400160405280600281526020017f54540000000000000000000000000000000000000000000000000000000000008152506001908161008c91906103c3565b50601260025f6101000a81548160ff021916908360ff1602179055503480156100b3575f5ffd5b5069d3c21bcecceda100000060038190555060035460045f3373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020015f20819055503373ffffffffffffffffffffffffffffffffffffffff165f73ffffffffffffffffffffffffffffffffffffffff167fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef60035460405161016891906104a1565b60405180910390a36104ba565b5f81519050919050565b7f4e487b71000000000000000000000000000000000000000000000000000000005f52604160045260245ffd5b7f4e487b71000000000000000000000000000000000000000000000000000000005f52602260045260245ffd5b5f60028204905060018216806101f057607f821691505b602082108103610203576102026101ac565b5b50919050565b5f819050815f5260205f209050919050565b5f6020601f8301049050919050565b5f82821b905092915050565b5f600883026102657fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8261022a565b61026f868361022a565b95508019841693508086168417925050509392505050565b5f819050919050565b5f819050919050565b5f6102b36102ae6102a984610287565b610290565b610287565b9050919050565b5f819050919050565b6102cc83610299565b6102e06102d8826102ba565b848454610236565b825550505050565b5f5f905090565b6102f76102e8565b6103028184846102c3565b505050565b5f5b828110156103285761031d5f8284016102ef565b600181019050610309565b505050565b601f82111561037b578282111561037a5761034781610209565b6103508361021b565b6103598561021b565b6020861015610366575f90505b80830161037582840382610307565b505050505b5b505050565b5f82821c905092915050565b5f61039b5f1984600802610380565b1980831691505092915050565b5f6103b3838361038c565b9150826002028217905092915050565b6103cc82610175565b67ffffffffffffffff8111156103e5576103e461017f565b5b6103ef82546101d9565b6103fa82828561032d565b5f60209050601f83116001811461042b575f8415610419578287015190505b61042385826103a8565b86555061048a565b601f19841661043986610209565b5f5b828110156104605784890151825560018201915060208501945060208101905061043b565b8683101561047d5784890151610479601f89168261038c565b8355505b6001600288020188555050505b505050505050565b61049b81610287565b82525050565b5f6020820190506104b45f830184610492565b92915050565b6104ef806104c75f395ff3fe608060405234801561000f575f5ffd5b5060043610610060575f3560e01c806306fdde031461006457806318160ddd14610082578063313ce567146100a057806370a08231146100be57806395d89b41146100ee578063dd62ed3e1461010c575b5f5ffd5b61006c61013c565b6040516100799190610310565b60405180910390f35b61008a6101c7565b6040516100979190610348565b60405180910390f35b6100a86101cd565b6040516100b5919061037c565b60405180910390f35b6100d860048036038101906100d391906103f3565b6101df565b6040516100e59190610348565b60405180910390f35b6100f66101f4565b6040516101039190610310565b60405180910390f35b6101266004803603810190610121919061041e565b610280565b6040516101339190610348565b60405180910390f35b5f805461014890610489565b80601f016020809104026020016040519081016040528092919081815260200182805461017490610489565b80156101bf5780601f10610196576101008083540402835291602001916101bf565b820191905f5260205f20905b8154815290600101906020018083116101a257829003601f168201915b505050505081565b60035481565b60025f9054906101000a900460ff1681565b6004602052805f5260405f205f915090505481565b6001805461020190610489565b80601f016020809104026020016040519081016040528092919081815260200182805461022d90610489565b80156102785780601f1061024f57610100808354040283529160200191610278565b820191905f5260205f20905b81548152906001019060200180831161025b57829003601f168201915b505050505081565b6005602052815f5260405f20602052805f5260405f205f91509150505481565b5f81519050919050565b5f82825260208201905092915050565b8281835e5f83830152505050565b5f601f19601f8301169050919050565b5f6102e2826102a0565b6102ec81856102aa565b93506102fc8185602086016102ba565b610305816102c8565b840191505092915050565b5f6020820190508181035f83015261032881846102d8565b905092915050565b5f819050919050565b61034281610330565b82525050565b5f60208201905061035b5f830184610339565b92915050565b5f60ff82169050919050565b61037681610361565b82525050565b5f60208201905061038f5f83018461036d565b92915050565b5f5ffd5b5f73ffffffffffffffffffffffffffffffffffffffff82169050919050565b5f6103c282610399565b9050919050565b6103d2816103b8565b81146103dc575f5ffd5b50565b5f813590506103ed816103c9565b92915050565b5f6020828403121561040857610407610395565b5b5f610415848285016103df565b91505092915050565b5f5f6040838503121561043457610433610395565b5b5f610441858286016103df565b9250506020610452858286016103df565b9150509250929050565b7f4e487b71000000000000000000000000000000000000000000000000000000005f52602260045260245ffd5b5f60028204905060018216806104a057607f821691505b6020821081036104b3576104b261045c565b5b5091905056fea26469706673582212201e752684ce5f489792323cab58244e3b80c849987a349be9859847a057c85a5864736f6c63430008210033")

;;; =========================================================================
;;; Shared State (populated during test run)
;;; =========================================================================

(defvar *integ-provider* nil)
(defvar *integ-pk0* nil)
(defvar *integ-pk1* nil)
(defvar *integ-wallet0* nil)
(defvar *integ-wallet1* nil)
(defvar *integ-addr0* nil)
(defvar *integ-addr1* nil)
(defvar *integ-transfer-hash* nil)
(defvar *integ-erc20-deploy-hash* nil)
(defvar *integ-erc20-addr* nil)

;;; =========================================================================
;;; Helper Functions
;;; =========================================================================

(defun %integ-setup ()
  "Initialize shared state for integration tests. Returns T on success."
  (handler-case
      (progn
        (setf *integ-provider*
              (coalton:coalton
               (web3/provider:make-http-provider "http://127.0.0.1:8545")))
        (setf *integ-pk0*
              (result-value
               (coalton:coalton
                (web3/types:hex-decode
                 (coalton:lisp coalton:String () *account0-pk-hex*)))))
        (setf *integ-pk1*
              (result-value
               (coalton:coalton
                (web3/types:hex-decode
                 (coalton:lisp coalton:String () *account1-pk-hex*)))))
        (setf *integ-addr0*
              (result-value
               (coalton:coalton
                (web3/address:address-from-hex
                 (coalton:lisp coalton:String () *account0-addr-hex*)))))
        (setf *integ-addr1*
              (result-value
               (coalton:coalton
                (web3/address:address-from-hex
                 (coalton:lisp coalton:String () *account1-addr-hex*)))))
        (setf *integ-wallet0*
              (coalton:coalton
               (web3/wallet:wallet-with-provider
                (coalton:lisp web3/types:Bytes () *integ-pk0*)
                (coalton:lisp web3/provider:HttpProvider () *integ-provider*))))
        (setf *integ-wallet1*
              (coalton:coalton
               (web3/wallet:wallet-with-provider
                (coalton:lisp web3/types:Bytes () *integ-pk1*)
                (coalton:lisp web3/provider:HttpProvider () *integ-provider*))))
        (setf *integ-transfer-hash* nil)
        (setf *integ-erc20-deploy-hash* nil)
        (setf *integ-erc20-addr* nil)
        t)
    (error (e)
      (format t "  Integration setup FAILED: ~A~%" e)
      nil)))

(defun %integ-get-nonce (addr)
  "Get current nonce for an address from Anvil."
  (result-value
   (coalton:coalton
    (web3/provider:eth-get-transaction-count
     (coalton:lisp web3/provider:HttpProvider () *integ-provider*)
     (coalton:lisp web3/address:Address () addr)))))

(defun %integ-make-transfer-tx (nonce to-addr value-wei)
  "Build an EIP-1559 ETH transfer transaction."
  (let ((priority-fee (coalton:coalton (web3/types:u256-from-integer 1000000000)))
        (max-fee (coalton:coalton (web3/types:u256-from-integer 20000000000)))
        (value (coalton:coalton
                (web3/types:u256-from-integer
                 (coalton:lisp coalton:Integer () value-wei))))
        (empty-data (make-array 0 :fill-pointer 0 :adjustable t)))
    (coalton:coalton
     (web3/transaction:make-transaction
      web3/transaction:EIP1559Tx
      31337
      (coalton:lisp coalton:U64 () nonce)
      (coalton:lisp web3/types:U256 () priority-fee)
      (coalton:lisp web3/types:U256 () max-fee)
      21000
      (coalton-prelude:Some (coalton:lisp web3/address:Address () to-addr))
      (coalton:lisp web3/types:U256 () value)
      (coalton:lisp web3/types:Bytes () empty-data)
      coalton:Nil))))

(defun %integ-u256-to-int (u256-val)
  "Convert a Coalton U256 to CL integer."
  (coalton:coalton
   (web3/types:u256-to-integer
    (coalton:lisp web3/types:U256 () u256-val))))

;;; =========================================================================
;;; Category A: Provider Basics (7 tests)
;;; =========================================================================

(defun %run-integ-provider-tests ()
  (format t "~%--- A: Provider Basics ---~%")

  (test-case "integ: eth_chainId returns 31337"
    (let ((result (coalton:coalton
                   (web3/provider:eth-chain-id
                    (coalton:lisp web3/provider:HttpProvider () *integ-provider*)))))
      (assert (result-ok-p result))
      (assert (= (result-value result) 31337))))

  (test-case "integ: eth_blockNumber returns >= 0"
    (let ((result (coalton:coalton
                   (web3/provider:eth-block-number
                    (coalton:lisp web3/provider:HttpProvider () *integ-provider*)))))
      (assert (result-ok-p result))
      (assert (>= (result-value result) 0))))

  (test-case "integ: eth_gasPrice returns > 0"
    (let ((result (coalton:coalton
                   (web3/provider:eth-gas-price
                    (coalton:lisp web3/provider:HttpProvider () *integ-provider*)))))
      (assert (result-ok-p result))
      (assert (> (%integ-u256-to-int (result-value result)) 0))))

  (test-case "integ: eth_getBalance for Account 0 returns large balance"
    (let ((result (coalton:coalton
                   (web3/provider:eth-get-balance
                    (coalton:lisp web3/provider:HttpProvider () *integ-provider*)
                    (coalton:lisp web3/address:Address () *integ-addr0*)))))
      (assert (result-ok-p result))
      ;; Anvil accounts start with 10000 ETH = 10^22 wei
      (assert (> (%integ-u256-to-int (result-value result))
                 1000000000000000000))))  ; > 1 ETH

  (test-case "integ: eth_getBalance for random address returns 0"
    (let* ((random-addr (result-value
                         (coalton:coalton
                          (web3/address:address-from-hex
                           "0x0000000000000000000000000000000000000042"))))
           (result (coalton:coalton
                    (web3/provider:eth-get-balance
                     (coalton:lisp web3/provider:HttpProvider () *integ-provider*)
                     (coalton:lisp web3/address:Address () random-addr)))))
      (assert (result-ok-p result))
      (assert (= (%integ-u256-to-int (result-value result)) 0))))

  (test-case "integ: eth_getTransactionCount for Account 0 >= 0"
    (let ((result (coalton:coalton
                   (web3/provider:eth-get-transaction-count
                    (coalton:lisp web3/provider:HttpProvider () *integ-provider*)
                    (coalton:lisp web3/address:Address () *integ-addr0*)))))
      (assert (result-ok-p result))
      (assert (>= (result-value result) 0))))

  (test-case "integ: multiple sequential RPC calls succeed"
    (let ((r1 (coalton:coalton
               (web3/provider:eth-chain-id
                (coalton:lisp web3/provider:HttpProvider () *integ-provider*))))
          (r2 (coalton:coalton
               (web3/provider:eth-block-number
                (coalton:lisp web3/provider:HttpProvider () *integ-provider*))))
          (r3 (coalton:coalton
               (web3/provider:eth-gas-price
                (coalton:lisp web3/provider:HttpProvider () *integ-provider*)))))
      (assert (result-ok-p r1))
      (assert (result-ok-p r2))
      (assert (result-ok-p r3)))))

;;; =========================================================================
;;; Category B: Wallet (5 tests)
;;; =========================================================================

(defun %run-integ-wallet-tests ()
  (format t "~%--- B: Wallet ---~%")

  (test-case "integ: wallet creation with provider succeeds"
    (assert *integ-wallet0*)
    (assert *integ-wallet1*))

  (test-case "integ: wallet-address matches Account 0"
    (let* ((result (coalton:coalton
                    (web3/wallet:wallet-address
                     (coalton:lisp web3/wallet:Wallet () *integ-wallet0*))))
           (addr (result-value result))
           (hex (coalton:coalton
                 (web3/address:address-to-hex
                  (coalton:lisp web3/address:Address () addr)))))
      (assert (result-ok-p result))
      (assert (string-equal hex *account0-addr-hex*))))

  (test-case "integ: wallet-address matches Account 1"
    (let* ((result (coalton:coalton
                    (web3/wallet:wallet-address
                     (coalton:lisp web3/wallet:Wallet () *integ-wallet1*))))
           (addr (result-value result))
           (hex (coalton:coalton
                 (web3/address:address-to-hex
                  (coalton:lisp web3/address:Address () addr)))))
      (assert (result-ok-p result))
      (assert (string-equal hex *account1-addr-hex*))))

  (test-case "integ: wallet-get-balance returns large balance"
    (let ((result (coalton:coalton
                   (web3/wallet:wallet-get-balance
                    (coalton:lisp web3/wallet:Wallet () *integ-wallet0*)))))
      (assert (result-ok-p result))
      (assert (> (%integ-u256-to-int (result-value result))
                 1000000000000000000))))

  (test-case "integ: wallet-get-nonce returns >= 0"
    (let ((result (coalton:coalton
                   (web3/wallet:wallet-get-nonce
                    (coalton:lisp web3/wallet:Wallet () *integ-wallet0*)))))
      (assert (result-ok-p result))
      (assert (>= (result-value result) 0)))))

;;; =========================================================================
;;; Category C: ETH Transfer (5 tests)
;;; =========================================================================

(defun %run-integ-transfer-tests ()
  (format t "~%--- C: ETH Transfer ---~%")

  (test-case "integ: sign and send EIP-1559 transfer"
    (let* ((nonce (%integ-get-nonce *integ-addr0*))
           (tx (%integ-make-transfer-tx nonce *integ-addr1* 10000000000000000))
           (signed-result
             (coalton:coalton
              (web3/transaction:tx-sign
               (coalton:lisp web3/transaction:Transaction () tx)
               (coalton:lisp web3/types:Bytes () *integ-pk0*))))
           (signed-tx (result-value signed-result))
           (send-result
             (coalton:coalton
              (web3/provider:eth-send-raw-transaction
               (coalton:lisp web3/provider:HttpProvider () *integ-provider*)
               (coalton:lisp web3/types:Bytes () signed-tx)))))
      (assert (result-ok-p signed-result))
      (assert (result-ok-p send-result))
      (let ((tx-hash (result-value send-result)))
        (assert (> (length tx-hash) 0))
        (setf *integ-transfer-hash* tx-hash))))

  (test-case "integ: verify transfer receipt"
    (assert *integ-transfer-hash*)
    ;; Ensure block is committed by making a round-trip RPC call
    (coalton:coalton
     (web3/provider:eth-block-number
      (coalton:lisp web3/provider:HttpProvider () *integ-provider*)))
    (let ((receipt-json-result
            (coalton:coalton
             (web3/provider:eth-get-transaction-receipt
              (coalton:lisp web3/provider:HttpProvider () *integ-provider*)
              (coalton:lisp coalton:String () *integ-transfer-hash*)))))
      (assert (result-ok-p receipt-json-result))
      (let* ((receipt-json (result-value receipt-json-result))
             (receipt-result
               (coalton:coalton
                (web3/receipt:parse-receipt
                 (coalton:lisp coalton:String () receipt-json)))))
        (assert (result-ok-p receipt-result))
        (let* ((receipt (result-value receipt-result))
               (success (coalton:coalton
                         (web3/receipt:receipt-success?
                          (coalton:lisp web3/receipt:Receipt () receipt)))))
          (assert (eq success coalton:True))))))

  (test-case "integ: wallet-send-transaction works"
    (let* ((nonce (%integ-get-nonce *integ-addr0*))
           (tx (%integ-make-transfer-tx nonce *integ-addr1* 10000000000000000))
           (result (coalton:coalton
                    (web3/wallet:wallet-send-transaction
                     (coalton:lisp web3/wallet:Wallet () *integ-wallet0*)
                     (coalton:lisp web3/transaction:Transaction () tx)))))
      (assert (result-ok-p result))
      (assert (> (length (result-value result)) 0))))

  (test-case "integ: nonce increments after send"
    (let* ((nonce-before (%integ-get-nonce *integ-addr0*))
           (tx (%integ-make-transfer-tx nonce-before *integ-addr1* 1000000000000000))
           (result (coalton:coalton
                    (web3/wallet:wallet-send-transaction
                     (coalton:lisp web3/wallet:Wallet () *integ-wallet0*)
                     (coalton:lisp web3/transaction:Transaction () tx))))
           (nonce-after (%integ-get-nonce *integ-addr0*)))
      (assert (result-ok-p result))
      (assert (= nonce-after (1+ nonce-before)))))

  (test-case "integ: recipient balance increases after transfer"
    (let* ((balance-before
             (let ((r (coalton:coalton
                       (web3/provider:eth-get-balance
                        (coalton:lisp web3/provider:HttpProvider () *integ-provider*)
                        (coalton:lisp web3/address:Address () *integ-addr1*)))))
               (%integ-u256-to-int (result-value r))))
           (nonce (%integ-get-nonce *integ-addr0*))
           (tx (%integ-make-transfer-tx nonce *integ-addr1* 50000000000000000))
           (_send (coalton:coalton
                   (web3/wallet:wallet-send-transaction
                    (coalton:lisp web3/wallet:Wallet () *integ-wallet0*)
                    (coalton:lisp web3/transaction:Transaction () tx))))
           (balance-after
             (let ((r (coalton:coalton
                       (web3/provider:eth-get-balance
                        (coalton:lisp web3/provider:HttpProvider () *integ-provider*)
                        (coalton:lisp web3/address:Address () *integ-addr1*)))))
               (%integ-u256-to-int (result-value r)))))
      (declare (ignore _send))
      (assert (> balance-after balance-before)))))

;;; =========================================================================
;;; Category D: Gas Estimation (4 tests)
;;; =========================================================================

(defun %run-integ-gas-tests ()
  (format t "~%--- D: Gas Estimation ---~%")

  (test-case "integ: eth-estimate-gas for simple transfer returns 21000"
    (let* ((value-u256 (coalton:coalton (web3/types:u256-from-integer 1000000000000000)))
           (empty-data (make-array 0 :fill-pointer 0 :adjustable t))
           (result (coalton:coalton
                    (web3/provider:eth-estimate-gas
                     (coalton:lisp web3/provider:HttpProvider () *integ-provider*)
                     (coalton:lisp web3/address:Address () *integ-addr0*)
                     (coalton-prelude:Some
                      (coalton:lisp web3/address:Address () *integ-addr1*))
                     (coalton:lisp web3/types:U256 () value-u256)
                     (coalton:lisp web3/types:Bytes () empty-data)))))
      (assert (result-ok-p result))
      (assert (= (result-value result) 21000))))

  (test-case "integ: simulate estimate-gas works"
    (let* ((empty-data (make-array 0 :fill-pointer 0 :adjustable t))
           (value-u256 (coalton:coalton (web3/types:u256-from-integer 1000000000000000)))
           (opts (coalton:coalton
                  web3/simulate:default-call-options))
           (result (coalton:coalton
                    (web3/simulate:estimate-gas
                     (coalton:lisp web3/provider:HttpProvider () *integ-provider*)
                     (coalton:lisp web3/address:Address () *integ-addr1*)
                     (coalton:lisp web3/types:Bytes () empty-data)
                     (coalton:lisp web3/types:U256 () value-u256)
                     (coalton:lisp web3/simulate:CallOptions () opts)))))
      (assert (result-ok-p result))
      (assert (>= (result-value result) 21000))))

  (test-case "integ: estimate-transaction-gas on transfer tx"
    (let* ((nonce (%integ-get-nonce *integ-addr0*))
           (tx (%integ-make-transfer-tx nonce *integ-addr1* 1000000000000000))
           (result (coalton:coalton
                    (web3/simulate:estimate-transaction-gas
                     (coalton:lisp web3/provider:HttpProvider () *integ-provider*)
                     (coalton:lisp web3/transaction:Transaction () tx)))))
      (assert (result-ok-p result))
      (assert (>= (result-value result) 21000))))

  (test-case "integ: estimate-transaction-cost returns valid GasEstimate"
    (let* ((nonce (%integ-get-nonce *integ-addr0*))
           (tx (%integ-make-transfer-tx nonce *integ-addr1* 1000000000000000))
           (result (coalton:coalton
                    (web3/simulate:estimate-transaction-cost
                     (coalton:lisp web3/provider:HttpProvider () *integ-provider*)
                     (coalton:lisp web3/transaction:Transaction () tx)))))
      (assert (result-ok-p result))
      (let* ((estimate (result-value result))
             (gas-limit (coalton:coalton
                         (web3/simulate:.gas-estimate-gas-limit
                          (coalton:lisp web3/simulate:GasEstimate () estimate))))
             (total-cost (coalton:coalton
                          (web3/simulate:.gas-estimate-total-cost
                           (coalton:lisp web3/simulate:GasEstimate () estimate)))))
        (assert (> gas-limit 0))
        (assert (> (%integ-u256-to-int total-cost) 0))))))

;;; =========================================================================
;;; Category E: Nonce Management (5 tests)
;;; =========================================================================

(defun %run-integ-nonce-tests ()
  (format t "~%--- E: Nonce Management ---~%")

  (test-case "integ: nonce-get fetches from Anvil"
    (let* ((nm (coalton:coalton
                (web3/nonce-manager:make-nonce-manager
                 (coalton:lisp web3/provider:HttpProvider () *integ-provider*))))
           (result (coalton:coalton
                    (web3/nonce-manager:nonce-get
                     (coalton:lisp web3/address:Address () *integ-addr0*)
                     31337
                     (coalton:lisp web3/nonce-manager:NonceManager () nm)))))
      (assert (result-ok-p result))
      (assert (>= (result-value result) 0))))

  (test-case "integ: nonce-consume increments locally"
    (let* ((nm (coalton:coalton
                (web3/nonce-manager:make-nonce-manager
                 (coalton:lisp web3/provider:HttpProvider () *integ-provider*))))
           (first-result
             (coalton:coalton
              (web3/nonce-manager:nonce-consume
               (coalton:lisp web3/address:Address () *integ-addr0*)
               31337
               (coalton:lisp web3/nonce-manager:NonceManager () nm))))
           (first-nonce (result-value first-result))
           (second-result
             (coalton:coalton
              (web3/nonce-manager:nonce-consume
               (coalton:lisp web3/address:Address () *integ-addr0*)
               31337
               (coalton:lisp web3/nonce-manager:NonceManager () nm)))))
      (assert (result-ok-p first-result))
      (assert (result-ok-p second-result))
      (assert (= (result-value second-result) (1+ first-nonce)))))

  (test-case "integ: nonce-peek returns Some after nonce-get"
    (let* ((nm (coalton:coalton
                (web3/nonce-manager:make-nonce-manager
                 (coalton:lisp web3/provider:HttpProvider () *integ-provider*)))))
      ;; First get to populate cache
      (coalton:coalton
       (web3/nonce-manager:nonce-get
        (coalton:lisp web3/address:Address () *integ-addr0*)
        31337
        (coalton:lisp web3/nonce-manager:NonceManager () nm)))
      ;; Now peek should return Some
      (let ((peek-result
              (coalton:coalton
               (web3/nonce-manager:nonce-peek
                (coalton:lisp web3/address:Address () *integ-addr0*)
                31337
                (coalton:lisp web3/nonce-manager:NonceManager () nm)))))
        (assert (optional-some-p peek-result)))))

  (test-case "integ: nonce-sync resets delta to network value"
    (let* ((nm (coalton:coalton
                (web3/nonce-manager:make-nonce-manager
                 (coalton:lisp web3/provider:HttpProvider () *integ-provider*)))))
      ;; Consume a couple nonces to build delta
      (coalton:coalton
       (web3/nonce-manager:nonce-consume
        (coalton:lisp web3/address:Address () *integ-addr0*)
        31337
        (coalton:lisp web3/nonce-manager:NonceManager () nm)))
      (coalton:coalton
       (web3/nonce-manager:nonce-consume
        (coalton:lisp web3/address:Address () *integ-addr0*)
        31337
        (coalton:lisp web3/nonce-manager:NonceManager () nm)))
      ;; Sync should reset delta
      (let ((sync-result
              (coalton:coalton
               (web3/nonce-manager:nonce-sync
                (coalton:lisp web3/address:Address () *integ-addr0*)
                31337
                (coalton:lisp web3/nonce-manager:NonceManager () nm)))))
        (assert (result-ok-p sync-result))
        ;; After sync, nonce-get should return the network value
        (let* ((network-nonce (%integ-get-nonce *integ-addr0*))
               (cached-result
                 (coalton:coalton
                  (web3/nonce-manager:nonce-get
                   (coalton:lisp web3/address:Address () *integ-addr0*)
                   31337
                   (coalton:lisp web3/nonce-manager:NonceManager () nm)))))
          (assert (result-ok-p cached-result))
          (assert (= (result-value cached-result) network-nonce))))))

  (test-case "integ: nonce-reset clears cache, peek returns None"
    (let* ((nm (coalton:coalton
                (web3/nonce-manager:make-nonce-manager
                 (coalton:lisp web3/provider:HttpProvider () *integ-provider*)))))
      ;; Populate cache
      (coalton:coalton
       (web3/nonce-manager:nonce-get
        (coalton:lisp web3/address:Address () *integ-addr0*)
        31337
        (coalton:lisp web3/nonce-manager:NonceManager () nm)))
      ;; Reset
      (coalton:coalton
       (web3/nonce-manager:nonce-reset
        (coalton:lisp web3/address:Address () *integ-addr0*)
        31337
        (coalton:lisp web3/nonce-manager:NonceManager () nm)))
      ;; Peek should now return None
      (let ((peek-result
              (coalton:coalton
               (web3/nonce-manager:nonce-peek
                (coalton:lisp web3/address:Address () *integ-addr0*)
                31337
                (coalton:lisp web3/nonce-manager:NonceManager () nm)))))
        (assert (optional-none-p peek-result))))))

;;; =========================================================================
;;; Category F: Simulate (4 tests)
;;; =========================================================================

(defun %run-integ-simulate-tests ()
  (format t "~%--- F: Simulate ---~%")

  (test-case "integ: simulate-call on EOA returns empty bytes"
    (let* ((empty-data (make-array 0 :fill-pointer 0 :adjustable t))
           (opts (coalton:coalton
                  web3/simulate:default-call-options))
           (result (coalton:coalton
                    (web3/simulate:simulate-call
                     (coalton:lisp web3/provider:HttpProvider () *integ-provider*)
                     (coalton:lisp web3/address:Address () *integ-addr1*)
                     (coalton:lisp web3/types:Bytes () empty-data)
                     (coalton:lisp web3/simulate:CallOptions () opts)))))
      (assert (result-ok-p result))
      (let* ((return-bytes (result-value result))
             (len (length return-bytes)))
        (assert (= len 0)))))

  (test-case "integ: simulate estimate-transaction-gas for transfer >= 21000"
    (let* ((nonce (%integ-get-nonce *integ-addr0*))
           (tx (%integ-make-transfer-tx nonce *integ-addr1* 1000000000000000))
           (result (coalton:coalton
                    (web3/simulate:estimate-transaction-gas
                     (coalton:lisp web3/provider:HttpProvider () *integ-provider*)
                     (coalton:lisp web3/transaction:Transaction () tx)))))
      (assert (result-ok-p result))
      (assert (>= (result-value result) 21000))))

  (test-case "integ: simulate estimate-transaction-cost returns valid estimate"
    (let* ((nonce (%integ-get-nonce *integ-addr0*))
           (tx (%integ-make-transfer-tx nonce *integ-addr1* 1000000000000000))
           (result (coalton:coalton
                    (web3/simulate:estimate-transaction-cost
                     (coalton:lisp web3/provider:HttpProvider () *integ-provider*)
                     (coalton:lisp web3/transaction:Transaction () tx)))))
      (assert (result-ok-p result))
      (let* ((estimate (result-value result))
             (gas-limit (coalton:coalton
                         (web3/simulate:.gas-estimate-gas-limit
                          (coalton:lisp web3/simulate:GasEstimate () estimate)))))
        (assert (> gas-limit 0)))))

  (test-case "integ: populate-transaction fills chain-id, gas-limit, nonce"
    (let* ((empty-data (make-array 0 :fill-pointer 0 :adjustable t))
           (value-u256 (coalton:coalton (web3/types:u256-from-integer 1000000000000000)))
           (result (coalton:coalton
                    (web3/simulate:populate-transaction
                     (coalton:lisp web3/provider:HttpProvider () *integ-provider*)
                     (coalton:lisp web3/address:Address () *integ-addr0*)
                     web3/transaction:EIP1559Tx
                     (coalton-prelude:Some
                      (coalton:lisp web3/address:Address () *integ-addr1*))
                     (coalton:lisp web3/types:U256 () value-u256)
                     (coalton:lisp web3/types:Bytes () empty-data)))))
      (assert (result-ok-p result))
      (let* ((tx (result-value result))
             (chain-id (coalton:coalton
                        (web3/transaction:.tx-chain-id
                         (coalton:lisp web3/transaction:Transaction () tx))))
             (gas-limit (coalton:coalton
                         (web3/transaction:.tx-gas-limit
                          (coalton:lisp web3/transaction:Transaction () tx))))
             (nonce (coalton:coalton
                     (web3/transaction:.tx-nonce
                      (coalton:lisp web3/transaction:Transaction () tx)))))
        (assert (= chain-id 31337))
        (assert (> gas-limit 0))
        (assert (>= nonce 0))))))

;;; =========================================================================
;;; Category G: ERC20 Deploy + Read (8 tests)
;;; =========================================================================

(defun %run-integ-erc20-tests ()
  (format t "~%--- G: ERC20 Deploy + Read ---~%")

  (test-case "integ: deploy ERC20 contract"
    (let* ((nonce (%integ-get-nonce *integ-addr0*))
           (bytecode (result-value
                      (coalton:coalton
                       (web3/types:hex-decode
                        (coalton:lisp coalton:String () *test-erc20-bytecode-hex*)))))
           (priority-fee (coalton:coalton (web3/types:u256-from-integer 1000000000)))
           (max-fee (coalton:coalton (web3/types:u256-from-integer 20000000000)))
           (zero-value (coalton:coalton (web3/types:u256-from-integer 0)))
           ;; Build deployment tx (to = None)
           (tx (coalton:coalton
                (web3/transaction:make-transaction
                 web3/transaction:EIP1559Tx
                 31337
                 (coalton:lisp coalton:U64 () nonce)
                 (coalton:lisp web3/types:U256 () priority-fee)
                 (coalton:lisp web3/types:U256 () max-fee)
                 2000000
                 coalton-prelude:None
                 (coalton:lisp web3/types:U256 () zero-value)
                 (coalton:lisp web3/types:Bytes () bytecode)
                 coalton:Nil)))
           (send-result
             (coalton:coalton
              (web3/wallet:wallet-send-transaction
               (coalton:lisp web3/wallet:Wallet () *integ-wallet0*)
               (coalton:lisp web3/transaction:Transaction () tx)))))
      (assert (result-ok-p send-result))
      (setf *integ-erc20-deploy-hash* (result-value send-result))
      ;; Compute contract address deterministically
      (let ((contract-addr
              (coalton:coalton
               (web3/deploy:compute-create-address
                (coalton:lisp web3/address:Address () *integ-addr0*)
                (coalton:lisp coalton:Integer () nonce)))))
        (assert contract-addr)
        (setf *integ-erc20-addr* contract-addr))))

  (test-case "integ: ERC20 deployment receipt is successful"
    (assert *integ-erc20-deploy-hash*)
    ;; Ensure block is committed
    (coalton:coalton
     (web3/provider:eth-block-number
      (coalton:lisp web3/provider:HttpProvider () *integ-provider*)))
    (let ((receipt-json-result
            (coalton:coalton
             (web3/provider:eth-get-transaction-receipt
              (coalton:lisp web3/provider:HttpProvider () *integ-provider*)
              (coalton:lisp coalton:String () *integ-erc20-deploy-hash*)))))
      (assert (result-ok-p receipt-json-result))
      (let* ((receipt-json (result-value receipt-json-result))
             (receipt-result
               (coalton:coalton
                (web3/receipt:parse-receipt
                 (coalton:lisp coalton:String () receipt-json)))))
        (assert (result-ok-p receipt-result))
        (let* ((receipt (result-value receipt-result))
               (success (coalton:coalton
                         (web3/receipt:receipt-success?
                          (coalton:lisp web3/receipt:Receipt () receipt)))))
          (assert (eq success coalton:True))))))

  (test-case "integ: erc20-name returns TestToken"
    (assert *integ-erc20-addr*)
    (let ((result (coalton:coalton
                   (web3/erc20:erc20-name
                    (coalton:lisp web3/provider:HttpProvider () *integ-provider*)
                    (coalton:lisp web3/address:Address () *integ-erc20-addr*)))))
      (assert (result-ok-p result))
      (assert (string= (result-value result) "TestToken"))))

  (test-case "integ: erc20-symbol returns TT"
    (assert *integ-erc20-addr*)
    (let ((result (coalton:coalton
                   (web3/erc20:erc20-symbol
                    (coalton:lisp web3/provider:HttpProvider () *integ-provider*)
                    (coalton:lisp web3/address:Address () *integ-erc20-addr*)))))
      (assert (result-ok-p result))
      (assert (string= (result-value result) "TT"))))

  (test-case "integ: erc20-decimals returns 18"
    (assert *integ-erc20-addr*)
    (let ((result (coalton:coalton
                   (web3/erc20:erc20-decimals
                    (coalton:lisp web3/provider:HttpProvider () *integ-provider*)
                    (coalton:lisp web3/address:Address () *integ-erc20-addr*)))))
      (assert (result-ok-p result))
      (assert (= (result-value result) 18))))

  (test-case "integ: erc20-total-supply returns 1000000 * 10^18"
    (assert *integ-erc20-addr*)
    (let ((result (coalton:coalton
                   (web3/erc20:erc20-total-supply
                    (coalton:lisp web3/provider:HttpProvider () *integ-provider*)
                    (coalton:lisp web3/address:Address () *integ-erc20-addr*)))))
      (assert (result-ok-p result))
      ;; 1000000 * 10^18 = 10^24
      (assert (= (%integ-u256-to-int (result-value result))
                 1000000000000000000000000))))

  (test-case "integ: erc20-balance-of deployer returns total supply"
    (assert *integ-erc20-addr*)
    (let ((result (coalton:coalton
                   (web3/erc20:erc20-balance-of
                    (coalton:lisp web3/provider:HttpProvider () *integ-provider*)
                    (coalton:lisp web3/address:Address () *integ-erc20-addr*)
                    (coalton:lisp web3/address:Address () *integ-addr0*)))))
      (assert (result-ok-p result))
      (assert (= (%integ-u256-to-int (result-value result))
                 1000000000000000000000000))))

  (test-case "integ: erc20-balance-of non-holder returns 0"
    (assert *integ-erc20-addr*)
    (let* ((random-addr (result-value
                         (coalton:coalton
                          (web3/address:address-from-hex
                           "0x0000000000000000000000000000000000000042"))))
           (result (coalton:coalton
                    (web3/erc20:erc20-balance-of
                     (coalton:lisp web3/provider:HttpProvider () *integ-provider*)
                     (coalton:lisp web3/address:Address () *integ-erc20-addr*)
                     (coalton:lisp web3/address:Address () random-addr)))))
      (assert (result-ok-p result))
      (assert (= (%integ-u256-to-int (result-value result)) 0)))))

;;; =========================================================================
;;; Main Runner
;;; =========================================================================

(defun run-integration-tests ()
  "Run integration tests against local Anvil node.
   Requires WEB3_INTEGRATION=1 and Anvil running on http://127.0.0.1:8545."
  (if (not (uiop:getenv "WEB3_INTEGRATION"))
      (format t "~%=== Integration Tests SKIPPED (set WEB3_INTEGRATION=1) ===~%")
      (progn
        (format t "~%=== Integration Tests (Anvil @ ~A) ===~%" *anvil-url*)
        (if (not (%integ-setup))
            (format t "  Cannot connect to Anvil - skipping integration tests~%")
            (progn
              (%run-integ-provider-tests)
              (%run-integ-wallet-tests)
              (%run-integ-transfer-tests)
              (%run-integ-gas-tests)
              (%run-integ-nonce-tests)
              (%run-integ-simulate-tests)
              (%run-integ-erc20-tests))))))
