;;; Web3 Test Package Definitions

;;; Package for Coalton test helpers
(defpackage #:web3-tests
  (:documentation "Coalton test helpers for web3-coalton library")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:vec #:coalton-library/vector)
   (#:iter #:coalton-library/iterator)
   (#:list #:coalton-library/list)
   (#:types #:web3/types)
   (#:rlp #:web3/rlp)
   (#:crypto #:web3/crypto)
   (#:addr #:web3/address)
   (#:abi #:web3/abi)
   (#:tx #:web3/transaction)
   (#:provider #:web3/provider)
   (#:wallet #:web3/wallet))
  (:export
   ;; Types tests
   #:test-hex-encode-empty
   #:test-hex-encode-bytes
   #:test-hex-decode-roundtrip
   #:test-hex-decode-prefixed
   #:test-u256-zero
   #:test-u256-from-integer
   #:test-u256-add
   #:test-u256-sub
   #:test-u256-mul
   #:test-u256-to-bytes-roundtrip
   #:test-u256-comparison
   #:test-bytes-pad-left
   #:test-bytes-pad-right
   #:test-bytes-equal
   #:test-ether-wei-conversion

   ;; RLP tests
   #:test-rlp-single-byte
   #:test-rlp-short-string
   #:test-rlp-empty-string
   #:test-rlp-empty-list
   #:test-rlp-integer-zero
   #:test-rlp-integer-small
   #:test-rlp-nested-list
   #:test-rlp-decode-roundtrip

   ;; Crypto tests
   #:test-keccak256-empty
   #:test-keccak256-hello

   ;; Address tests
   #:test-address-from-hex
   #:test-address-checksum

   ;; ABI tests
   #:test-function-selector-transfer
   #:test-abi-encode-uint256
   #:test-abi-encode-bool
   #:test-abi-encode-address))

;;; Package for pure CL test runners
(defpackage #:web3-tests/runner
  (:documentation "Pure CL test runners for web3-coalton library")
  (:use #:cl)
  (:export
   #:run-all-tests
   ;; Helper predicates for testing Coalton values
   #:optional-some-p
   #:optional-none-p))
