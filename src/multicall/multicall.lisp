;;;; Multicall module
;;;;
;;;; Provides utilities for batching multiple contract calls into a single
;;;; RPC request using the Multicall3 contract.
;;;;
;;;; Multicall3 is deployed at the same address on most EVM chains:
;;;; 0xcA11bde05977b3631167028862bE2a173976CA11

(in-package #:web3/multicall)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Internal Helpers
  ;;; =========================================================================

  (declare bytes-concat (types:Bytes -> types:Bytes -> types:Bytes))
  (define (bytes-concat a b)
    "Concatenate two byte arrays."
    (types:bytes-concat-many (Cons a (Cons b Nil))))

  ;;; =========================================================================
  ;;; Multicall3 Contract Address
  ;;; =========================================================================

  (declare multicall3-address (Unit -> String))
  (define (multicall3-address _)
    "Multicall3 contract address (same on most EVM chains).
     Deployed via CREATE2 for deterministic address."
    "0xcA11bde05977b3631167028862bE2a173976CA11")

  ;;; =========================================================================
  ;;; Call Types
  ;;; =========================================================================

  (define-struct Call
    "A simple call target + calldata pair.
     Used with aggregate() and tryAggregate()."
    (target types:Bytes)    ; 20-byte address
    (calldata types:Bytes)) ; encoded function call

  (define-struct Call3
    "A call with failure handling.
     Used with aggregate3()."
    (target types:Bytes)        ; 20-byte address
    (allow-failure Boolean)     ; if true, call can fail without reverting batch
    (calldata types:Bytes))     ; encoded function call

  (define-struct CallResult
    "Result of a single call in a batch."
    (success Boolean)           ; whether the call succeeded
    (return-data types:Bytes))  ; the return data (or revert reason)

  ;;; =========================================================================
  ;;; Function Selectors
  ;;; =========================================================================

  (declare aggregate-selector (Unit -> types:Bytes))
  (define (aggregate-selector _)
    "Function selector for aggregate((address,bytes)[]).
     0x252dba42"
    (abi:function-selector "aggregate((address,bytes)[])"))

  (declare try-aggregate-selector (Unit -> types:Bytes))
  (define (try-aggregate-selector _)
    "Function selector for tryAggregate(bool,(address,bytes)[]).
     0xbce38bd7"
    (abi:function-selector "tryAggregate(bool,(address,bytes)[])"))

  (declare aggregate3-selector (Unit -> types:Bytes))
  (define (aggregate3-selector _)
    "Function selector for aggregate3((address,bool,bytes)[]).
     0x82ad56cb"
    (abi:function-selector "aggregate3((address,bool,bytes)[])"))

  ;;; =========================================================================
  ;;; Encoding Helpers
  ;;; =========================================================================

  (declare call-to-abi-tuple (Call -> abi:AbiValue))
  (define (call-to-abi-tuple call)
    "Convert a Call to an ABI tuple value (address, bytes)."
    (abi:AbiTupleVal
     (Cons (abi:AbiAddressVal (.target call))
           (Cons (abi:AbiBytesVal (.calldata call))
                 Nil))))

  (declare call3-to-abi-tuple (Call3 -> abi:AbiValue))
  (define (call3-to-abi-tuple call)
    "Convert a Call3 to an ABI tuple value (address, bool, bytes)."
    (abi:AbiTupleVal
     (Cons (abi:AbiAddressVal (.target call))
           (Cons (abi:AbiBoolVal (.allow-failure call))
                 (Cons (abi:AbiBytesVal (.calldata call))
                       Nil)))))

  ;;; =========================================================================
  ;;; Calldata Builders
  ;;; =========================================================================

  (declare aggregate-calldata ((List Call) -> types:Bytes))
  (define (aggregate-calldata calls)
    "Build calldata for aggregate((address,bytes)[] calls).

     This is the simplest batching method - if any call fails,
     the entire batch reverts.

     Returns: (blockNumber, bytes[] returnData)"
    (let ((encoded-args (abi:abi-encode
                         (Cons (abi:AbiArrayVal (map call-to-abi-tuple calls)) Nil))))
      (bytes-concat (aggregate-selector Unit) encoded-args)))

  (declare try-aggregate-calldata (Boolean -> (List Call) -> types:Bytes))
  (define (try-aggregate-calldata require-success calls)
    "Build calldata for tryAggregate(bool requireSuccess, (address,bytes)[] calls).

     If requireSuccess is false, failed calls won't revert the batch.

     Returns: (bool success, bytes returnData)[]"
    (let ((encoded-args (abi:abi-encode
                         (Cons (abi:AbiBoolVal require-success)
                               (Cons (abi:AbiArrayVal (map call-to-abi-tuple calls))
                                     Nil)))))
      (bytes-concat (try-aggregate-selector Unit) encoded-args)))

  (declare aggregate3-calldata ((List Call3) -> types:Bytes))
  (define (aggregate3-calldata calls)
    "Build calldata for aggregate3((address,bool,bytes)[] calls).

     Each call specifies whether it can fail independently.

     Returns: (bool success, bytes returnData)[]"
    (let ((encoded-args (abi:abi-encode
                         (Cons (abi:AbiArrayVal (map call3-to-abi-tuple calls))
                               Nil))))
      (bytes-concat (aggregate3-selector Unit) encoded-args)))

  ;;; =========================================================================
  ;;; High-Level Helpers
  ;;; =========================================================================

  (declare tuple-to-call ((Tuple types:Bytes types:Bytes) -> Call))
  (define (tuple-to-call pair)
    "Convert a tuple to a Call struct."
    (match pair
      ((Tuple target calldata) (Call target calldata))))

  (declare tuple-to-call3 ((Tuple types:Bytes types:Bytes) -> Call3))
  (define (tuple-to-call3 pair)
    "Convert a tuple to a Call3 struct with allow-failure=True."
    (match pair
      ((Tuple target calldata) (Call3 target True calldata))))

  (declare batch-calls ((List (Tuple types:Bytes types:Bytes)) -> types:Bytes))
  (define (batch-calls target-calldata-pairs)
    "Create aggregate() calldata from a list of (target, calldata) pairs.
     All calls must succeed or the entire batch reverts."
    (aggregate-calldata (map tuple-to-call target-calldata-pairs)))

  (declare batch-calls-allow-failure ((List (Tuple types:Bytes types:Bytes)) -> types:Bytes))
  (define (batch-calls-allow-failure target-calldata-pairs)
    "Create aggregate3() calldata where all calls are allowed to fail.
     Failed calls return (false, revertData) instead of reverting."
    (aggregate3-calldata (map tuple-to-call3 target-calldata-pairs))))
