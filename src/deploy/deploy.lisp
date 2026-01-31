;;;; Contract deployment module
;;;;
;;;; Provides utilities for:
;;;; - Creating deployment transaction data (bytecode + constructor args)
;;;; - Computing CREATE addresses (from deployer + nonce)
;;;; - Computing CREATE2 addresses (from deployer + salt + init code hash)

(in-package #:web3/deploy)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Internal Helpers
  ;;; =========================================================================

  (declare bytes-concat (types:Bytes -> types:Bytes -> types:Bytes))
  (define (bytes-concat a b)
    "Concatenate two byte arrays."
    (types:bytes-concat-many (Cons a (Cons b Nil))))

  (declare address-from-bytes-unchecked (types:Bytes -> addr:Address))
  (define (address-from-bytes-unchecked bytes)
    "Create an address from exactly 20 bytes without validation.
     Internal use only - caller must ensure bytes is 20 bytes."
    (match (addr:address-from-bytes bytes)
      ((Ok addr) addr)
      ;; This should never happen if caller provides 20 bytes
      ((Err _) (error "Invalid address bytes"))))

  ;;; =========================================================================
  ;;; Deployment Data Construction
  ;;; =========================================================================

  (declare deployment-data (types:Bytes -> types:Bytes))
  (define (deployment-data bytecode)
    "Create deployment data from contract bytecode (no constructor args).
     Returns the bytecode as-is, ready for a deployment transaction."
    bytecode)

  (declare deployment-data-with-constructor
           (types:Bytes -> (List abi:AbiValue) -> types:Bytes))
  (define (deployment-data-with-constructor bytecode constructor-args)
    "Create deployment data from bytecode and constructor arguments.
     The constructor args are ABI-encoded and appended to the bytecode.

     Example:
       (deployment-data-with-constructor
         my-bytecode
         (Cons (AbiAddressVal owner-addr)
               (Cons (AbiUintVal initial-supply) Nil)))"
    (match constructor-args
      ((Nil) bytecode)
      (_ (let ((encoded-args (abi:abi-encode constructor-args)))
           (bytes-concat bytecode encoded-args)))))

  ;;; =========================================================================
  ;;; Constructor Encoding Helpers
  ;;; =========================================================================

  (declare encode-constructor ((List abi:AbiValue) -> types:Bytes))
  (define (encode-constructor args)
    "ABI-encode constructor arguments.
     This is the same as abi-encode but named for clarity in deployment context."
    (abi:abi-encode args))

  ;;; =========================================================================
  ;;; CREATE Address Computation
  ;;; =========================================================================
  ;;;
  ;;; CREATE address = keccak256(rlp([sender, nonce]))[12:]
  ;;;
  ;;; The address is derived from:
  ;;; - The deployer's address
  ;;; - The deployer's nonce at deployment time

  (declare compute-create-address (addr:Address -> Integer -> addr:Address))
  (define (compute-create-address deployer nonce)
    "Compute the address where a contract will be deployed using CREATE.

     Parameters:
       deployer - The address deploying the contract
       nonce    - The deployer's transaction nonce

     Returns the address where the contract will be deployed.

     Formula: address = keccak256(rlp([deployer, nonce]))[12:32]"
    (let ((deployer-bytes (addr:address-bytes deployer)))
      ;; RLP encode [deployer_address, nonce]
      (let ((rlp-encoded (rlp:rlp-encode
                          (rlp:RlpList
                           (Cons (rlp:RlpBytes deployer-bytes)
                                 (Cons (rlp-encode-integer-item nonce)
                                       Nil))))))
        ;; Hash and take last 20 bytes
        (let ((hash (crypto:keccak256 rlp-encoded)))
          (address-from-bytes-unchecked
           (types:bytes-slice 12 32 hash))))))

  (declare compute-create-address-from-hex
           (String -> Integer -> (types:Web3Result addr:Address)))
  (define (compute-create-address-from-hex deployer-hex nonce)
    "Compute CREATE address from a hex address string.

     Example:
       (compute-create-address-from-hex
         \"0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045\"
         0)"
    (match (addr:address-from-hex deployer-hex)
      ((Err e) (Err e))
      ((Ok deployer) (Ok (compute-create-address deployer nonce)))))

  ;;; =========================================================================
  ;;; CREATE2 Address Computation
  ;;; =========================================================================
  ;;;
  ;;; CREATE2 address = keccak256(0xff ++ sender ++ salt ++ keccak256(init_code))[12:]
  ;;;
  ;;; The address is derived from:
  ;;; - A fixed prefix (0xff)
  ;;; - The deployer's address
  ;;; - A 32-byte salt chosen by the deployer
  ;;; - The keccak256 hash of the initialization code

  (declare compute-create2-address
           (addr:Address -> types:Bytes -> types:Bytes -> addr:Address))
  (define (compute-create2-address deployer salt init-code)
    "Compute the address where a contract will be deployed using CREATE2.

     Parameters:
       deployer  - The address (or factory contract) deploying
       salt      - A 32-byte salt value
       init-code - The contract initialization code (bytecode + constructor args)

     Returns the address where the contract will be deployed.

     Formula: address = keccak256(0xff ++ deployer ++ salt ++ keccak256(init_code))[12:32]

     Note: CREATE2 allows deterministic deployment - the same inputs always
     produce the same address, regardless of nonce."
    (let ((deployer-bytes (addr:address-bytes deployer))
          (init-code-hash (crypto:keccak256 init-code))
          ;; Ensure salt is exactly 32 bytes (pad if needed)
          (salt-32 (types:bytes-pad-left 32 salt)))
      ;; Construct: 0xff ++ deployer (20 bytes) ++ salt (32 bytes) ++ init_code_hash (32 bytes)
      ;; Total: 1 + 20 + 32 + 32 = 85 bytes
      (let ((prefix (types:bytes-from-list (Cons #xff Nil))))
        (let ((data (bytes-concat
                     prefix
                     (bytes-concat
                      deployer-bytes
                      (bytes-concat salt-32 init-code-hash)))))
          ;; Hash and take last 20 bytes
          (let ((hash (crypto:keccak256 data)))
            (address-from-bytes-unchecked
             (types:bytes-slice 12 32 hash)))))))

  (declare compute-create2-address-from-hex
           (String -> types:Bytes -> types:Bytes -> (types:Web3Result addr:Address)))
  (define (compute-create2-address-from-hex deployer-hex salt init-code)
    "Compute CREATE2 address from a hex address string.

     Example:
       (compute-create2-address-from-hex
         \"0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045\"
         (u256-to-bytes (u256-from-integer 12345))  ; salt
         my-init-code)"
    (match (addr:address-from-hex deployer-hex)
      ((Err e) (Err e))
      ((Ok deployer) (Ok (compute-create2-address deployer salt init-code)))))

  ;;; =========================================================================
  ;;; Utility: RLP encode integer as RlpItem
  ;;; =========================================================================

  (declare rlp-encode-integer-item (Integer -> rlp:RlpItem))
  (define (rlp-encode-integer-item n)
    "Convert an integer to an RlpItem for RLP encoding."
    (if (== n 0)
        (rlp:RlpBytes (types:bytes-empty))
        (rlp:RlpBytes (integer-to-bytes-minimal n))))

  (declare integer-to-bytes-minimal (Integer -> types:Bytes))
  (define (integer-to-bytes-minimal n)
    "Convert integer to minimal byte representation (no leading zeros)."
    (if (== n 0)
        (types:bytes-empty)
        (lisp types:Bytes (n)
          (cl:let* ((byte-count (cl:ceiling (cl:integer-length n) 8))
                    (result (cl:make-array byte-count :fill-pointer byte-count :adjustable cl:t)))
            (cl:dotimes (i byte-count result)
              (cl:setf (cl:aref result (cl:- byte-count 1 i))
                       (cl:logand #xff (cl:ash n (cl:* -8 i))))))))))
