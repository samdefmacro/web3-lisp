;;;; Coalton-side bridge for web3/easy
;;;;
;;;; CL callers in `web3/easy` use these helpers to avoid having to construct
;;;; Coalton values (Some/None, Lists, structs, AbiValue variants) from raw
;;;; CL. Everything here is plain Coalton with no I/O of its own; the
;;;; networking lives in web3/provider.

(in-package #:web3/easy-bridge)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; -------------------------------------------------------------------------
  ;;; Convenience wrappers used by the easy.lisp CL surface
  ;;; -------------------------------------------------------------------------

  (declare eth-call
           (web3/provider:HttpProvider
            -> (Optional web3/address:Address)
            -> web3/address:Address
            -> web3/types:Bytes
            -> (web3/types:Web3Result web3/types:Bytes)))
  (define (eth-call provider from to data)
    "eth_call thin wrapper so the CL surface doesn't need to build (Optional Address)."
    (web3/provider:eth-call provider from to data))

  (declare make-eip1559-eth-transfer
           (U64     ; chain-id
            -> U64    ; nonce
            -> web3/types:U256 ; max-priority-fee
            -> web3/types:U256 ; max-fee
            -> U64    ; gas-limit
            -> web3/address:Address ; to
            -> web3/types:U256 ; value
            -> web3/transaction:Transaction))
  (define (make-eip1559-eth-transfer chain-id nonce
                                     max-priority-fee max-fee
                                     gas-limit to value)
    "Build an EIP-1559 ETH-transfer transaction (no calldata, no access list)."
    (web3/transaction:make-transaction
     web3/transaction:EIP1559Tx
     chain-id nonce max-priority-fee max-fee gas-limit
     (Some to)
     value
     web3/types:bytes-empty
     Nil))

  ;;; -------------------------------------------------------------------------
  ;;; Building AbiValue lists from CL inputs
  ;;; -------------------------------------------------------------------------
  ;;;
  ;;; defcontract knows each argument's Solidity type at macroexpand time and
  ;;; emits one of these helpers per arg, then funnels the resulting
  ;;; (List AbiValue) into encode-call-and-decode. CL never has to touch
  ;;; AbiValue constructors directly.

  (declare empty-args (List web3/abi:AbiValue))
  (define empty-args Nil)

  (declare push-uint
           (web3/types:U256 -> (List web3/abi:AbiValue) -> (List web3/abi:AbiValue)))
  (define (push-uint v acc)
    "Prepend a uint256 argument to ACC."
    (Cons (web3/abi:AbiUintVal v) acc))

  (declare push-int
           (Integer -> (List web3/abi:AbiValue) -> (List web3/abi:AbiValue)))
  (define (push-int v acc)
    "Prepend an int256 argument to ACC."
    (Cons (web3/abi:AbiIntVal v) acc))

  (declare push-address
           (web3/address:Address -> (List web3/abi:AbiValue) -> (List web3/abi:AbiValue)))
  (define (push-address a acc)
    "Prepend an address argument to ACC."
    (Cons (web3/abi:AbiAddressVal (web3/address:address-bytes a)) acc))

  (declare push-bool
           (Boolean -> (List web3/abi:AbiValue) -> (List web3/abi:AbiValue)))
  (define (push-bool b acc)
    "Prepend a bool argument to ACC."
    (Cons (web3/abi:AbiBoolVal b) acc))

  (declare push-string
           (String -> (List web3/abi:AbiValue) -> (List web3/abi:AbiValue)))
  (define (push-string s acc)
    "Prepend a string argument to ACC."
    (Cons (web3/abi:AbiStringVal s) acc))

  (declare push-bytes
           (web3/types:Bytes -> (List web3/abi:AbiValue) -> (List web3/abi:AbiValue)))
  (define (push-bytes b acc)
    "Prepend a dynamic bytes argument to ACC."
    (Cons (web3/abi:AbiBytesVal b) acc))

  (declare push-bytes-fixed
           (web3/types:Bytes -> (List web3/abi:AbiValue) -> (List web3/abi:AbiValue)))
  (define (push-bytes-fixed b acc)
    "Prepend a fixed-bytesN argument (e.g. bytes32) to ACC."
    (Cons (web3/abi:AbiBytesFixedVal b) acc))

  ;;; -------------------------------------------------------------------------
  ;;; Array wrapping (for T[] and T[N])
  ;;; -------------------------------------------------------------------------
  ;;;
  ;;; The CL surface converts a CL list into a typed Coalton list once
  ;;; (Integer -> U256, hex -> Address, etc.) and hands it here. These helpers
  ;;; just wrap the homogenous Coalton list into the matching AbiValue and
  ;;; push it onto the arg accumulator.
  ;;;
  ;;; AbiArrayVal vs AbiFixedArrayVal differ only in encoding (the latter
  ;;; omits the length prefix), so the macro picks the right wrapper based on
  ;;; the declared Solidity type.

  (declare wrap-uint-array
           ((List web3/types:U256) -> web3/abi:AbiValue))
  (define (wrap-uint-array xs) (web3/abi:AbiArrayVal (map web3/abi:AbiUintVal xs)))

  (declare wrap-uint-fixed-array
           ((List web3/types:U256) -> web3/abi:AbiValue))
  (define (wrap-uint-fixed-array xs)
    (web3/abi:AbiFixedArrayVal (map web3/abi:AbiUintVal xs)))

  (declare wrap-int-array
           ((List Integer) -> web3/abi:AbiValue))
  (define (wrap-int-array xs) (web3/abi:AbiArrayVal (map web3/abi:AbiIntVal xs)))

  (declare wrap-int-fixed-array
           ((List Integer) -> web3/abi:AbiValue))
  (define (wrap-int-fixed-array xs)
    (web3/abi:AbiFixedArrayVal (map web3/abi:AbiIntVal xs)))

  (declare wrap-bool-array
           ((List Boolean) -> web3/abi:AbiValue))
  (define (wrap-bool-array xs) (web3/abi:AbiArrayVal (map web3/abi:AbiBoolVal xs)))

  (declare wrap-bool-fixed-array
           ((List Boolean) -> web3/abi:AbiValue))
  (define (wrap-bool-fixed-array xs)
    (web3/abi:AbiFixedArrayVal (map web3/abi:AbiBoolVal xs)))

  (declare wrap-string-array
           ((List String) -> web3/abi:AbiValue))
  (define (wrap-string-array xs) (web3/abi:AbiArrayVal (map web3/abi:AbiStringVal xs)))

  (declare wrap-string-fixed-array
           ((List String) -> web3/abi:AbiValue))
  (define (wrap-string-fixed-array xs)
    (web3/abi:AbiFixedArrayVal (map web3/abi:AbiStringVal xs)))

  (declare wrap-address-array
           ((List web3/address:Address) -> web3/abi:AbiValue))
  (define (wrap-address-array xs)
    (web3/abi:AbiArrayVal
     (map (fn (a) (web3/abi:AbiAddressVal (web3/address:address-bytes a))) xs)))

  (declare wrap-address-fixed-array
           ((List web3/address:Address) -> web3/abi:AbiValue))
  (define (wrap-address-fixed-array xs)
    (web3/abi:AbiFixedArrayVal
     (map (fn (a) (web3/abi:AbiAddressVal (web3/address:address-bytes a))) xs)))

  (declare wrap-bytes-array
           ((List web3/types:Bytes) -> web3/abi:AbiValue))
  (define (wrap-bytes-array xs)
    (web3/abi:AbiArrayVal (map web3/abi:AbiBytesVal xs)))

  (declare wrap-bytes-fixed-element-array
           ((List web3/types:Bytes) -> web3/abi:AbiValue))
  (define (wrap-bytes-fixed-element-array xs)
    "An array of fixed-size bytesN elements (dynamic outer, fixed-size element)."
    (web3/abi:AbiArrayVal (map web3/abi:AbiBytesFixedVal xs)))

  ;;; -------------------------------------------------------------------------
  ;;; Encode + call + decode in one step
  ;;; -------------------------------------------------------------------------

  (declare contract-encode-and-call
           (web3/provider:HttpProvider
            -> web3/contract:Contract
            -> String                              ; function name
            -> (List web3/abi:AbiValue)            ; args (already in declared order)
            -> (web3/types:Web3Result (List web3/abi:AbiValue))))
  (define (contract-encode-and-call provider contract fn-name args)
    "Encode a function call by name, eth_call, and decode the outputs."
    (do (calldata <- (web3/contract:encode-function-call-by-name contract fn-name args))
        (raw      <- (web3/provider:eth-call
                      provider None (web3/contract:.contract-address contract)
                      calldata))
        (web3/contract:decode-function-output-by-name contract fn-name raw)))

  (declare contract-encode-calldata
           (web3/contract:Contract
            -> String
            -> (List web3/abi:AbiValue)
            -> (web3/types:Web3Result web3/types:Bytes)))
  (define (contract-encode-calldata contract fn-name args)
    "Encode a function call by name into raw calldata bytes (no eth_call)."
    (web3/contract:encode-function-call-by-name contract fn-name args))

  ;;; -------------------------------------------------------------------------
  ;;; Extract single output value to a CL-friendly representation
  ;;; -------------------------------------------------------------------------
  ;;;
  ;;; All extractors take the decoded `(List AbiValue)` and return a value
  ;;; ready to hand back to CL. They Err if the head shape doesn't match the
  ;;; declared output type — that catches contract/ABI drift.

  (declare extract-uint
           ((List web3/abi:AbiValue) -> (web3/types:Web3Result web3/types:U256)))
  (define (extract-uint xs)
    (match xs
      ((Cons (web3/abi:AbiUintVal u) _) (Ok u))
      (_ (Err (web3/types:AbiError
               "easy-bridge:extract-uint: expected uint head")))))

  (declare extract-int
           ((List web3/abi:AbiValue) -> (web3/types:Web3Result Integer)))
  (define (extract-int xs)
    (match xs
      ((Cons (web3/abi:AbiIntVal i) _) (Ok i))
      (_ (Err (web3/types:AbiError
               "easy-bridge:extract-int: expected int head")))))

  (declare extract-bool
           ((List web3/abi:AbiValue) -> (web3/types:Web3Result Boolean)))
  (define (extract-bool xs)
    (match xs
      ((Cons (web3/abi:AbiBoolVal b) _) (Ok b))
      (_ (Err (web3/types:AbiError
               "easy-bridge:extract-bool: expected bool head")))))

  (declare extract-string
           ((List web3/abi:AbiValue) -> (web3/types:Web3Result String)))
  (define (extract-string xs)
    (match xs
      ((Cons (web3/abi:AbiStringVal s) _) (Ok s))
      (_ (Err (web3/types:AbiError
               "easy-bridge:extract-string: expected string head")))))

  (declare extract-address
           ((List web3/abi:AbiValue) -> (web3/types:Web3Result web3/address:Address)))
  (define (extract-address xs)
    (match xs
      ((Cons (web3/abi:AbiAddressVal b) _) (web3/address:address-from-bytes b))
      (_ (Err (web3/types:AbiError
               "easy-bridge:extract-address: expected address head")))))

  (declare extract-bytes
           ((List web3/abi:AbiValue) -> (web3/types:Web3Result web3/types:Bytes)))
  (define (extract-bytes xs)
    (match xs
      ((Cons (web3/abi:AbiBytesVal b) _) (Ok b))
      ((Cons (web3/abi:AbiBytesFixedVal b) _) (Ok b))
      (_ (Err (web3/types:AbiError
               "easy-bridge:extract-bytes: expected bytes head")))))

  ;;; -------------------------------------------------------------------------
  ;;; Array extractors — return a typed (List X) ready for CL
  ;;; -------------------------------------------------------------------------
  ;;;
  ;;; Each extractor accepts the decoded list, expects the first element to
  ;;; be an AbiArrayVal/AbiFixedArrayVal, and walks its items into a typed
  ;;; Coalton list. Mismatched element shapes degrade to the type's zero
  ;;; rather than erroring, so a partially-malformed return still surfaces
  ;;; *something*; production code that cares should re-validate.

  (declare %array-items
           ((List web3/abi:AbiValue) -> (web3/types:Web3Result (List web3/abi:AbiValue))))
  (define (%array-items xs)
    "Extract the inner list from an AbiArrayVal/AbiFixedArrayVal head, or Err."
    (match xs
      ((Cons (web3/abi:AbiArrayVal items) _) (Ok items))
      ((Cons (web3/abi:AbiFixedArrayVal items) _) (Ok items))
      (_ (Err (web3/types:AbiError
               "easy-bridge: expected array head")))))

  (declare extract-uint-array
           ((List web3/abi:AbiValue) -> (web3/types:Web3Result (List web3/types:U256))))
  (define (extract-uint-array xs)
    (do (items <- (%array-items xs))
        (Ok (map (fn (v)
                   (match v
                     ((web3/abi:AbiUintVal u) u)
                     (_ web3/types:u256-zero)))
                 items))))

  (declare extract-int-array
           ((List web3/abi:AbiValue) -> (web3/types:Web3Result (List Integer))))
  (define (extract-int-array xs)
    (do (items <- (%array-items xs))
        (Ok (map (fn (v)
                   (match v
                     ((web3/abi:AbiIntVal i) i)
                     (_ 0)))
                 items))))

  (declare extract-bool-array
           ((List web3/abi:AbiValue) -> (web3/types:Web3Result (List Boolean))))
  (define (extract-bool-array xs)
    (do (items <- (%array-items xs))
        (Ok (map (fn (v)
                   (match v
                     ((web3/abi:AbiBoolVal b) b)
                     (_ False)))
                 items))))

  (declare extract-string-array
           ((List web3/abi:AbiValue) -> (web3/types:Web3Result (List String))))
  (define (extract-string-array xs)
    (do (items <- (%array-items xs))
        (Ok (map (fn (v)
                   (match v
                     ((web3/abi:AbiStringVal s) s)
                     (_ "")))
                 items))))

  (declare extract-address-array
           ((List web3/abi:AbiValue) -> (web3/types:Web3Result (List web3/address:Address))))
  (define (extract-address-array xs)
    (do (items <- (%array-items xs))
        (let ((parsed
                (map (fn (v)
                       (match v
                         ((web3/abi:AbiAddressVal b)
                          (match (web3/address:address-from-bytes b)
                            ((Ok a) a)
                            ((Err _) web3/address:address-zero)))
                         (_ web3/address:address-zero)))
                     items)))
          (Ok parsed))))

  (declare extract-bytes-array
           ((List web3/abi:AbiValue) -> (web3/types:Web3Result (List web3/types:Bytes))))
  (define (extract-bytes-array xs)
    (do (items <- (%array-items xs))
        (Ok (map (fn (v)
                   (match v
                     ((web3/abi:AbiBytesVal b) b)
                     ((web3/abi:AbiBytesFixedVal b) b)
                     (_ web3/types:bytes-empty)))
                 items))))

  ;;; -------------------------------------------------------------------------
  ;;; Event helpers — for defcontract-generated event decoders
  ;;; -------------------------------------------------------------------------

  (declare event-topic-bytes
           (web3/contract:Contract -> String -> (web3/types:Web3Result web3/types:Bytes)))
  (define (event-topic-bytes contract event-name)
    "Look up an event's topic0 (32-byte keccak hash of its signature)."
    (match (web3/contract:get-event contract event-name)
      ((None)
       (Err (web3/types:AbiError
             (lisp String (event-name)
               (cl:format cl:nil "easy-bridge: event not found: ~A" event-name)))))
      ((Some ev) (Ok (web3/abi-parser:.event-topic ev)))))

  (declare decode-event-by-name
           (web3/contract:Contract
            -> String
            -> (List web3/types:Bytes)   ; topics (incl. topic0)
            -> web3/types:Bytes          ; data
            -> (web3/types:Web3Result (List web3/abi:AbiValue))))
  (define (decode-event-by-name contract event-name topics data)
    "Look up an event by name and decode topics + data into AbiValues
     (in the order the ABI declared them, indexed and non-indexed mixed)."
    (match (web3/contract:get-event contract event-name)
      ((None)
       (Err (web3/types:AbiError
             (lisp String (event-name)
               (cl:format cl:nil "easy-bridge: event not found: ~A" event-name)))))
      ((Some ev)
       (web3/contract:decode-event ev topics data))))

  ;; Per-position extractors. `idx` is 0-based and matches the ABI's input
  ;; declaration order (indexed + non-indexed are interleaved by the
  ;; underlying decode).

  (declare %nth-or-err
           (UFix -> (List web3/abi:AbiValue) -> (web3/types:Web3Result web3/abi:AbiValue)))
  (define (%nth-or-err idx xs)
    "Bounds-checked nth: coalton-library/list:nth panics on out-of-range,
     so we use list:index (which returns Optional) and convert to Result."
    (match (coalton-library/list:index idx xs)
      ((Some v) (Ok v))
      ((None) (Err (web3/types:AbiError
                    (lisp String (idx)
                      (cl:format cl:nil "easy-bridge: event arg index out of range: ~A" idx)))))))

  (declare event-arg-uint
           ((List web3/abi:AbiValue) -> UFix -> (web3/types:Web3Result web3/types:U256)))
  (define (event-arg-uint xs idx)
    (do (v <- (%nth-or-err idx xs))
        (match v
          ((web3/abi:AbiUintVal u) (Ok u))
          (_ (Err (web3/types:AbiError "event-arg-uint: not a uint"))))))

  (declare event-arg-int
           ((List web3/abi:AbiValue) -> UFix -> (web3/types:Web3Result Integer)))
  (define (event-arg-int xs idx)
    (do (v <- (%nth-or-err idx xs))
        (match v
          ((web3/abi:AbiIntVal i) (Ok i))
          (_ (Err (web3/types:AbiError "event-arg-int: not an int"))))))

  (declare event-arg-bool
           ((List web3/abi:AbiValue) -> UFix -> (web3/types:Web3Result Boolean)))
  (define (event-arg-bool xs idx)
    (do (v <- (%nth-or-err idx xs))
        (match v
          ((web3/abi:AbiBoolVal b) (Ok b))
          (_ (Err (web3/types:AbiError "event-arg-bool: not a bool"))))))

  (declare event-arg-string
           ((List web3/abi:AbiValue) -> UFix -> (web3/types:Web3Result String)))
  (define (event-arg-string xs idx)
    (do (v <- (%nth-or-err idx xs))
        (match v
          ((web3/abi:AbiStringVal s) (Ok s))
          (_ (Err (web3/types:AbiError "event-arg-string: not a string"))))))

  (declare event-arg-address
           ((List web3/abi:AbiValue) -> UFix -> (web3/types:Web3Result web3/address:Address)))
  (define (event-arg-address xs idx)
    (do (v <- (%nth-or-err idx xs))
        (match v
          ((web3/abi:AbiAddressVal b) (web3/address:address-from-bytes b))
          (_ (Err (web3/types:AbiError "event-arg-address: not an address"))))))

  (declare event-arg-bytes
           ((List web3/abi:AbiValue) -> UFix -> (web3/types:Web3Result web3/types:Bytes)))
  (define (event-arg-bytes xs idx)
    (do (v <- (%nth-or-err idx xs))
        (match v
          ((web3/abi:AbiBytesVal b) (Ok b))
          ((web3/abi:AbiBytesFixedVal b) (Ok b))
          (_ (Err (web3/types:AbiError "event-arg-bytes: not bytes"))))))

  ;;; -------------------------------------------------------------------------
  ;;; Multicall3 — CL-driven aggregate3
  ;;; -------------------------------------------------------------------------

  (declare make-call3
           (web3/address:Address -> Boolean -> web3/types:Bytes
            -> web3/multicall:Call3))
  (define (make-call3 target allow-failure data)
    "Build a Call3 from CL-friendly inputs (address struct + bool + bytes)."
    (web3/multicall:Call3 (web3/address:address-bytes target) allow-failure data))

  (declare encode-aggregate3
           ((List web3/multicall:Call3) -> web3/types:Bytes))
  (define (encode-aggregate3 calls)
    "Build aggregate3 calldata for a list of Call3."
    (web3/multicall:aggregate3-calldata calls))

  (declare %extract-call-result-tuple
           (web3/abi:AbiValue -> (Tuple Boolean web3/types:Bytes)))
  (define (%extract-call-result-tuple v)
    (match v
      ((web3/abi:AbiTupleVal items)
       (match items
         ((Cons (web3/abi:AbiBoolVal s)
                (Cons (web3/abi:AbiBytesVal d) _))
          (Tuple s d))
         (_ (Tuple False web3/types:bytes-empty))))
      (_ (Tuple False web3/types:bytes-empty))))

  (declare decode-aggregate3-response
           (web3/types:Bytes
            -> (web3/types:Web3Result (List (Tuple Boolean web3/types:Bytes)))))
  (define (decode-aggregate3-response raw)
    "Decode the eth_call response of aggregate3 into a list of (success, data)."
    (let ((tuple-type (web3/abi:AbiTuple
                       (Cons web3/abi:AbiBool
                             (Cons web3/abi:AbiBytes Nil)))))
      (do (decoded <- (web3/abi:abi-decode
                       (Cons (web3/abi:AbiArray tuple-type) Nil)
                       raw))
          (match decoded
            ((Cons (web3/abi:AbiArrayVal items) _)
             (Ok (map %extract-call-result-tuple items)))
            (_ (Err (web3/types:AbiError
                     "easy-bridge:decode-aggregate3-response: expected array head")))))))

  ;;; -------------------------------------------------------------------------
  ;;; Block fetching + lightweight CL-friendly summary accessors
  ;;; -------------------------------------------------------------------------

  (declare fetch-block-json
           (web3/provider:HttpProvider -> String -> Boolean
            -> (web3/types:Web3Result (Optional String))))
  (define (fetch-block-json provider tag full-txs)
    "eth_getBlockByNumber, returning the raw JSON string or None."
    (web3/provider:eth-get-block-by-number provider tag full-txs))

  (declare parse-block-from-json
           (String -> Boolean -> (web3/types:Web3Result (Optional web3/block:Block))))
  (define (parse-block-from-json json-str full-txs)
    (web3/block:parse-get-block-response json-str full-txs))

  ;; Per-field Block extractors. These exist so the CL get-block helper can
  ;; pull each field through `(coalton:coalton (bridge-fn block))` without
  ;; chained struct accessor calls in CL — the CL side never needs to know
  ;; how `Block` nests `BlockHeader`.

  (declare block-number-of (web3/block:Block -> UFix))
  (define (block-number-of b)
    (web3/block:.header-number (web3/block:.block-header b)))

  (declare block-hash-hex (web3/block:Block -> String))
  (define (block-hash-hex b)
    (web3/types:hex-encode-prefixed
     (web3/block:.header-hash (web3/block:.block-header b))))

  (declare block-parent-hash-hex (web3/block:Block -> String))
  (define (block-parent-hash-hex b)
    (web3/types:hex-encode-prefixed
     (web3/block:.header-parent-hash (web3/block:.block-header b))))

  (declare block-timestamp-of (web3/block:Block -> UFix))
  (define (block-timestamp-of b)
    (web3/block:.header-timestamp (web3/block:.block-header b)))

  (declare block-miner-hex (web3/block:Block -> String))
  (define (block-miner-hex b)
    (web3/address:address-to-checksum-hex
     (web3/block:.header-miner (web3/block:.block-header b))))

  (declare block-gas-limit-of (web3/block:Block -> UFix))
  (define (block-gas-limit-of b)
    (web3/block:.header-gas-limit (web3/block:.block-header b)))

  (declare block-gas-used-of (web3/block:Block -> UFix))
  (define (block-gas-used-of b)
    (web3/block:.header-gas-used (web3/block:.block-header b)))

  (declare block-base-fee-of (web3/block:Block -> (Optional UFix)))
  (define (block-base-fee-of b)
    (web3/block:.header-base-fee (web3/block:.block-header b)))

  (declare block-tx-count-of (web3/block:Block -> UFix))
  (define (block-tx-count-of b)
    (coalton-library/list:length (web3/block:.block-transactions b)))

  (declare block-size-of (web3/block:Block -> UFix))
  (define (block-size-of b)
    (web3/block:.header-size (web3/block:.block-header b))))
