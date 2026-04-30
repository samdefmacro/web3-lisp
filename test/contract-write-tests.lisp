;;; Contract write tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Contract Write Tests
;;;
;;; The send paths require a live JSON-RPC provider; these unit tests cover
;;; the deterministic error paths (no-provider wallet, unknown function).
;;; Live-RPC roundtrip is exercised by the integration suite when WEB3_TEST_RPC_URL
;;; is set.
;;; =========================================================================

(defun run-contract-write-tests ()
  (format t "~%=== Contract Write Tests ===~%")

  (test-case "send-raw-call returns WalletError when wallet has no provider"
    (let* ((priv-key (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (wallet (coalton:coalton
                     (web3/wallet:make-wallet
                      (coalton:lisp web3/types:Bytes () priv-key))))
           (to-result (coalton:coalton
                        (web3/address:address-from-hex
                         "0xdAC17F958D2ee523a2206206994597C13D831ec7")))
           (to-addr (result-value to-result))
           (data (make-array 4 :fill-pointer 4 :adjustable t
                                :initial-contents '(#x00 #x00 #x00 #x00)))
           (result (coalton:coalton
                     (web3/contract-write:send-raw-call
                      (coalton:lisp web3/wallet:Wallet () wallet)
                      (coalton:lisp web3/address:Address () to-addr)
                      web3/types:u256-zero
                      (coalton:lisp web3/types:Bytes () data)))))
      (assert (result-err-p result))))

  (test-case "send-function-call returns WalletError when wallet has no provider"
    (let* ((priv-key (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (wallet (coalton:coalton
                     (web3/wallet:make-wallet
                      (coalton:lisp web3/types:Bytes () priv-key))))
           (addr-result (coalton:coalton
                          (web3/address:address-from-hex
                           "0xdAC17F958D2ee523a2206206994597C13D831ec7")))
           (addr (result-value addr-result))
           (contract (result-value
                       (coalton:coalton
                         (web3/contract:contract-from-abi-json
                          (coalton:lisp web3/address:Address () addr)
                          (coalton:lisp coalton:String () *erc20-abi*)))))
           ;; Build args for transfer(to, amount)
           (to-result (coalton:coalton
                        (web3/address:address-from-hex
                         "0x1111111111111111111111111111111111111111")))
           (to-addr (result-value to-result))
           (to-bytes (coalton:coalton
                       (web3/address:address-bytes
                        (coalton:lisp web3/address:Address () to-addr))))
           (addr-val (coalton:coalton
                       (web3/abi:AbiAddressVal
                        (coalton:lisp web3/types:Bytes () to-bytes))))
           (amount-val (coalton:coalton
                         (web3/abi:AbiUintVal
                          (web3/types:u256-from-integer 1000))))
           (args (list addr-val amount-val))
           (result (coalton:coalton
                     (web3/contract-write:send-function-call
                      (coalton:lisp web3/wallet:Wallet () wallet)
                      (coalton:lisp web3/contract:Contract () contract)
                      "transfer"
                      (coalton:lisp (coalton:List web3/abi:AbiValue) () args)
                      web3/types:u256-zero))))
      (assert (result-err-p result))))

  (test-case "send-function-call returns AbiError for unknown function"
    (let* ((priv-key (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (wallet (coalton:coalton
                     (web3/wallet:make-wallet
                      (coalton:lisp web3/types:Bytes () priv-key))))
           (addr-result (coalton:coalton
                          (web3/address:address-from-hex
                           "0xdAC17F958D2ee523a2206206994597C13D831ec7")))
           (addr (result-value addr-result))
           (contract (result-value
                       (coalton:coalton
                         (web3/contract:contract-from-abi-json
                          (coalton:lisp web3/address:Address () addr)
                          (coalton:lisp coalton:String () *erc20-abi*)))))
           (result (coalton:coalton
                     (web3/contract-write:send-function-call
                      (coalton:lisp web3/wallet:Wallet () wallet)
                      (coalton:lisp web3/contract:Contract () contract)
                      "nonexistent"
                      coalton:Nil
                      web3/types:u256-zero))))
      ;; Encoding runs before the provider check, so the Err must be an
      ;; AbiError (not the no-provider WalletError that other tests trigger).
      (assert (result-err-p result))
      (assert (%type-contains (result-value result) "ABI"))))

  (test-case "send-function-call-with-builder routes through send-raw-call"
    ;; Verify the builder path also fails cleanly with no provider configured.
    (let* ((priv-key (make-array 32 :fill-pointer 32 :adjustable t :initial-element 1))
           (wallet (coalton:coalton
                     (web3/wallet:make-wallet
                      (coalton:lisp web3/types:Bytes () priv-key))))
           (addr-result (coalton:coalton
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
                         "totalSupply"))))
           (result (coalton:coalton
                     (web3/contract-write:send-function-call-with-builder
                      (coalton:lisp web3/wallet:Wallet () wallet)
                      (coalton:lisp web3/contract:CallBuilder () builder)
                      web3/types:u256-zero))))
      (assert (result-err-p result))))

  (format t "~%  Note: Set WEB3_TEST_RPC_URL for contract-write integration tests~%"))
