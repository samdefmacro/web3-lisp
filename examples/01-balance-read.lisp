;;;; Read ETH and USDC balances for an address.
;;;;
;;;;   sbcl --load examples/01-balance-read.lisp
;;;;
;;;; Override the URL or address with environment variables:
;;;;   WEB3_RPC_URL=https://my-node ADDRESS=0x... sbcl --load examples/01-balance-read.lisp

(asdf:load-system "web3/easy")

(defpackage #:web3-example/balance-read
  (:use #:cl))
(in-package #:web3-example/balance-read)

(defparameter *url*
  (or (uiop:getenv "WEB3_RPC_URL") "https://eth.llamarpc.com"))

(defparameter *address*
  (or (uiop:getenv "ADDRESS")
      "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045"))  ; vitalik.eth

(defparameter *usdc*
  "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48")

(defun report-eth-balance ()
  (let ((wei (web3:get-balance *url* *address*)))
    (format t "~&ETH balance of ~A:~%  ~A wei~%  = ~A ETH~%"
            (web3:checksum-address *address*)
            wei
            (web3:format-ether wei))))

(defun report-usdc-balance ()
  ;; Two ways to do it: bare web3:erc20-balance, or via defcontract.
  (let ((bare    (web3:erc20-balance *url* *usdc* *address*))
        (decimals (web3:erc20-decimals *url* *usdc*))
        (symbol  (web3:erc20-symbol *url* *usdc*)))
    (format t "~&~A balance: ~A (raw, decimals = ~A)~%  = ~A ~A~%"
            symbol bare decimals
            (web3:format-units bare decimals) symbol)))

(handler-case
    (progn
      (report-eth-balance)
      (report-usdc-balance))
  (web3:web3-error (c)
    (format *error-output* "~&web3 error: ~A~%" (web3:web3-error-message c))
    (uiop:quit 1)))
