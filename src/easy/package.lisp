;;;; web3/easy - Plain Common Lisp surface over web3-lisp.
;;;;
;;;; The Coalton modules give you typed correctness; this module gives you
;;;; the one-line ergonomics. Functions take CL strings and integers, return
;;;; CL strings and integers, and signal `web3-error` on failure instead of
;;;; threading Result types through callers. Use this from the REPL or from
;;;; non-Coalton consumer code; reach for the typed modules when you need
;;;; the type safety.
;;;;
;;;; Internal Coalton helpers live in `web3/easy-bridge`.

;;; Internal Coalton helper package — anything the CL surface needs to build
;;; or unwrap that's awkward to do directly from CL goes here.
(cl:defpackage #:web3/easy-bridge
  (:documentation "Internal Coalton bridge helpers used by the web3/easy CL surface.")
  (:use #:coalton
        #:coalton-prelude)
  (:export
   ;; misc helpers
   #:eth-call
   #:make-eip1559-eth-transfer
   ;; AbiValue list construction
   #:empty-args
   #:push-uint
   #:push-int
   #:push-address
   #:push-bool
   #:push-string
   #:push-bytes
   #:push-bytes-fixed
   ;; AbiValue array wrappers (input encoding for T[] / T[N])
   #:wrap-uint-array
   #:wrap-uint-fixed-array
   #:wrap-int-array
   #:wrap-int-fixed-array
   #:wrap-bool-array
   #:wrap-bool-fixed-array
   #:wrap-string-array
   #:wrap-string-fixed-array
   #:wrap-address-array
   #:wrap-address-fixed-array
   #:wrap-bytes-array
   #:wrap-bytes-fixed-element-array
   ;; encode + call + decode pipeline
   #:contract-encode-and-call
   #:contract-encode-calldata
   ;; output extractors (single value)
   #:extract-uint
   #:extract-int
   #:extract-bool
   #:extract-string
   #:extract-address
   #:extract-bytes
   ;; output extractors (arrays)
   #:extract-uint-array
   #:extract-int-array
   #:extract-bool-array
   #:extract-string-array
   #:extract-address-array
   #:extract-bytes-array
   ;; event helpers
   #:event-topic-bytes
   #:decode-event-by-name
   #:event-arg-uint
   #:event-arg-int
   #:event-arg-bool
   #:event-arg-string
   #:event-arg-address
   #:event-arg-bytes
   ;; multicall3 helpers
   #:multicall3-canonical-address
   #:make-call3
   #:encode-aggregate3
   #:decode-aggregate3-response
   ;; block helpers
   #:fetch-block-json
   #:parse-block-from-json
   #:block-number-of
   #:block-hash-hex
   #:block-parent-hash-hex
   #:block-timestamp-of
   #:block-miner-hex
   #:block-gas-limit-of
   #:block-gas-used-of
   #:block-base-fee-of
   #:block-tx-count-of
   #:block-size-of))

;;; Public CL surface.
(cl:defpackage #:web3/easy
  (:documentation
   "Plain CL convenience surface: connect, balance/nonce reads, ERC-20 helpers,
    unit conversion, address checksums, transaction send + receipt wait.")
  (:use #:cl)
  (:nicknames #:web3)
  (:export
   ;; Conditions
   #:web3-error
   #:web3-error-message
   ;; Retry / fallback
   #:with-fallback
   ;; Multicall3 batched reads
   #:multicall
   #:*multicall3-address*
   ;; Parsed block reads
   #:get-block
   ;; Provider reads (URL-based)
   #:get-block-number
   #:chain-id
   #:get-balance
   #:get-transaction-count
   #:gas-price
   #:max-priority-fee
   #:get-code
   #:get-storage-at
   #:eth-call
   #:get-receipt
   #:wait-for-receipt
   ;; Units
   #:parse-ether
   #:format-ether
   #:parse-units
   #:format-units
   #:parse-gwei
   #:format-gwei
   ;; Address
   #:checksum-address
   #:address-zero
   ;; Hashing
   #:keccak256
   ;; ERC-20 reads
   #:erc20-name
   #:erc20-symbol
   #:erc20-decimals
   #:erc20-balance
   #:erc20-total-supply
   #:erc20-allowance
   ;; Wallet (lightweight CL wrapper)
   #:wallet
   #:make-wallet-from-hex
   #:wallet-address
   #:wallet-balance
   #:wallet-nonce
   #:wallet-send-eth
   ;; Contract bindings
   #:defcontract))
