;;;; Sign and send an ETH transfer against a local Anvil node.
;;;;
;;;;   anvil &                    # in another terminal
;;;;   sbcl --load examples/03-send-eth.lisp
;;;;
;;;; The default Anvil dev key (the first listed one) sends 0.01 ETH to the
;;;; second account. Override the URL with WEB3_RPC_URL or change the
;;;; constants below.
;;;;
;;;; This demonstrates the full lifecycle:
;;;;   - load a wallet from a private-key hex
;;;;   - sign + broadcast in one call
;;;;   - poll for the receipt

(asdf:load-system "web3/easy")

(defpackage #:web3-example/send-eth
  (:use #:cl))
(in-package #:web3-example/send-eth)

(defparameter *url*
  (or (uiop:getenv "WEB3_RPC_URL") "http://127.0.0.1:8545"))

(defparameter *private-key*
  ;; Anvil dev account #0 — DO NOT USE ON MAINNET.
  "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80")

(defparameter *to*
  "0x70997970C51812dc3A010C7d01b50e0d17dc79C8")  ; Anvil dev account #1

(defparameter *amount-eth* "0.01")

(handler-case
    (let ((wallet (web3:make-wallet-from-hex *private-key* *url*)))
      (format t "~&from:    ~A~%" (web3:wallet-address wallet))
      (format t "to:      ~A~%" (web3:checksum-address *to*))
      (format t "balance: ~A ETH (before)~%"
              (web3:format-ether (web3:wallet-balance wallet)))
      (format t "chain:   ~A   nonce: ~A~%"
              (web3:chain-id *url*) (web3:wallet-nonce wallet))

      ;; chain-id, nonce, gas, fees are all auto-populated from *url*.
      (let* ((tx-hash (web3:wallet-send-eth
                       wallet *to* (web3:parse-ether *amount-eth*))))
        (format t "~&sent: ~A~%waiting for receipt...~%" tx-hash)
        (let ((receipt (web3:wait-for-receipt *url* tx-hash
                                              :max-attempts 30
                                              :poll-interval-ms 1000)))
          (declare (ignore receipt))
          (format t "confirmed.~%"))
        (format t "~&balance: ~A ETH (after)~%"
                (web3:format-ether (web3:wallet-balance wallet)))))
  (web3:web3-error (c)
    (format *error-output* "~&web3 error: ~A~%" (web3:web3-error-message c))
    (uiop:quit 1)))
