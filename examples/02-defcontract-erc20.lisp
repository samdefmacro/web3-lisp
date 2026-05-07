;;;; Bind an ERC-20 contract with `defcontract` and exercise the typed
;;;; wrappers it generates.
;;;;
;;;;   sbcl --load examples/02-defcontract-erc20.lisp
;;;;
;;;; Demonstrates:
;;;;   - defcontract reading the ABI from an inline string
;;;;   - generated read wrappers (returning CL strings/integers)
;;;;   - generated calldata builders for non-view functions
;;;;   - the "no network" path: builders run offline; reads need an RPC
;;;;
;;;; Set WEB3_INTEGRATION=1 to actually issue eth_call against *url*.

(asdf:load-system "web3/easy")

(defpackage #:web3-example/erc20
  (:use #:cl))
(in-package #:web3-example/erc20)

(defparameter *url*
  (or (uiop:getenv "WEB3_RPC_URL") "https://eth.llamarpc.com"))

;; Standard minimal ERC-20 ABI (just what we need to read + transfer).
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *erc20-abi*
    "[
     {\"type\":\"function\",\"name\":\"name\",\"inputs\":[],\"outputs\":[{\"name\":\"\",\"type\":\"string\"}],\"stateMutability\":\"view\"},
     {\"type\":\"function\",\"name\":\"symbol\",\"inputs\":[],\"outputs\":[{\"name\":\"\",\"type\":\"string\"}],\"stateMutability\":\"view\"},
     {\"type\":\"function\",\"name\":\"decimals\",\"inputs\":[],\"outputs\":[{\"name\":\"\",\"type\":\"uint8\"}],\"stateMutability\":\"view\"},
     {\"type\":\"function\",\"name\":\"balanceOf\",\"inputs\":[{\"name\":\"owner\",\"type\":\"address\"}],\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"stateMutability\":\"view\"},
     {\"type\":\"function\",\"name\":\"transfer\",\"inputs\":[{\"name\":\"to\",\"type\":\"address\"},{\"name\":\"amount\",\"type\":\"uint256\"}],\"outputs\":[{\"name\":\"\",\"type\":\"bool\"}],\"stateMutability\":\"nonpayable\"},
     {\"type\":\"function\",\"name\":\"approve\",\"inputs\":[{\"name\":\"spender\",\"type\":\"address\"},{\"name\":\"amount\",\"type\":\"uint256\"}],\"outputs\":[{\"name\":\"\",\"type\":\"bool\"}],\"stateMutability\":\"nonpayable\"}
     ]"))

(web3:defcontract usdc
  :address "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"
  :abi *erc20-abi*)

;;; --- Offline: calldata builders work without a network ----------------------

(format t "~&== offline: calldata builders ==~%")
(format t "  transfer(0xd8da..., 1.5 USDC):~%    ~A~%"
        (usdc-transfer-data
         "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045"
         (web3:parse-units "1.5" 6)))
(format t "  approve(0x111..., 1000):~%    ~A~%"
        (usdc-approve-data "0x1111111111111111111111111111111111111111" 1000))

;;; --- Online: reads against a real RPC --------------------------------------

(when (uiop:getenv "WEB3_INTEGRATION")
  (format t "~&== online: live reads ==~%")
  (handler-case
      (progn
        (format t "  USDC name:     ~A~%" (usdc-name *url*))
        (format t "  USDC symbol:   ~A~%" (usdc-symbol *url*))
        (format t "  USDC decimals: ~A~%" (usdc-decimals *url*))
        (let* ((vitalik "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045")
               (raw     (usdc-balance-of *url* vitalik))
               (decs    (usdc-decimals *url*)))
          (format t "  vitalik USDC balance: ~A USDC~%"
                  (web3:format-units raw decs))))
    (web3:web3-error (c)
      (format *error-output* "~&web3 error: ~A~%" (web3:web3-error-message c)))))

(unless (uiop:getenv "WEB3_INTEGRATION")
  (format t "~&(set WEB3_INTEGRATION=1 to also exercise live reads)~%"))
