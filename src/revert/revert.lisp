;;;; Revert Reason Decoding
;;;;
;;;; Decodes Solidity revert reasons from failed eth_call / eth_estimateGas.
;;;; Supports:
;;;;   - Error(string)  — standard revert with message (selector 0x08c379a2)
;;;;   - Panic(uint256) — assertion failures / overflow (selector 0x4e487b71)
;;;;   - Custom errors  — unrecognized 4-byte selectors with raw data
;;;;   - Empty reverts  — no data returned

(in-package #:web3/revert)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Known Error Selectors
  ;;; =========================================================================

  (declare error-selector types:Bytes)
  (define error-selector
    "Selector for Error(string) — 0x08c379a2"
    (lisp types:Bytes ()
      (cl:make-array 4 :initial-contents '(#x08 #xc3 #x79 #xa2)
                     :fill-pointer 4 :adjustable cl:t)))

  (declare panic-selector types:Bytes)
  (define panic-selector
    "Selector for Panic(uint256) — 0x4e487b71"
    (lisp types:Bytes ()
      (cl:make-array 4 :initial-contents '(#x4e #x48 #x7b #x71)
                     :fill-pointer 4 :adjustable cl:t)))

  ;;; =========================================================================
  ;;; Revert Reason Type
  ;;; =========================================================================

  (define-type RevertReason
    "Decoded revert reason from a failed contract call."
    (RevertString String)       ; Error(string) with the message
    (RevertPanic types:U256)    ; Panic(uint256) with the panic code
    (RevertCustom types:Bytes types:Bytes)  ; Unknown selector + remaining data
    RevertEmpty)                ; Empty revert (no data)

  ;;; =========================================================================
  ;;; Panic Code Descriptions
  ;;; =========================================================================

  (declare panic-code-description (types:U256 -> String))
  (define (panic-code-description code)
    "Human-readable description for Solidity panic codes."
    (let ((n (types:u256-to-integer code)))
      (lisp String (n)
        (cl:case n
          (0 "generic compiler panic")
          (1 "assertion failure")
          (17 "arithmetic overflow/underflow")
          (18 "division or modulo by zero")
          (33 "invalid enum conversion")
          (34 "invalid storage byte array encoding")
          (49 "pop on empty array")
          (50 "array index out of bounds")
          (65 "out of memory")
          (81 "called uninitialized function")
          (cl:otherwise (cl:format cl:nil "unknown panic code ~A" n))))))

  ;;; =========================================================================
  ;;; Core Decoding
  ;;; =========================================================================

  (declare decode-revert-reason (types:Bytes -> RevertReason))
  (define (decode-revert-reason data)
    "Decode a revert reason from raw return data.
     Handles Error(string), Panic(uint256), and custom errors."
    (if (< (types:bytes-length data) 4)
        RevertEmpty
        (let ((selector (types:bytes-take 4 data))
              (payload (types:bytes-drop 4 data)))
          (cond
            ((types:bytes-equal? error-selector selector)
             ;; Error(string) — decode the string argument
             (match (abi:abi-decode (Cons abi:AbiString Nil) payload)
               ((Ok (Cons (abi:AbiStringVal msg) (Nil)))
                (RevertString msg))
               (_ (RevertCustom selector payload))))
            ((types:bytes-equal? panic-selector selector)
             ;; Panic(uint256) — decode the panic code
             (match (abi:abi-decode (Cons (abi:AbiUint 256) Nil) payload)
               ((Ok (Cons (abi:AbiUintVal code) (Nil)))
                (RevertPanic code))
               (_ (RevertCustom selector payload))))
            (True
             (RevertCustom selector payload))))))

  ;;; =========================================================================
  ;;; Hex String Decoding
  ;;; =========================================================================

  (declare decode-revert-hex (String -> RevertReason))
  (define (decode-revert-hex hex-str)
    "Decode a revert reason from a hex string (with or without 0x prefix)."
    (match (types:hex-decode hex-str)
      ((Ok data) (decode-revert-reason data))
      ((Err _) RevertEmpty)))

  ;;; =========================================================================
  ;;; Human-Readable Message
  ;;; =========================================================================

  (declare revert-reason-message (RevertReason -> String))
  (define (revert-reason-message reason)
    "Convert a RevertReason to a human-readable string."
    (match reason
      ((RevertString msg) msg)
      ((RevertPanic code)
       (lisp String (code)
         (cl:let ((desc (web3/revert::panic-code-description code)))
           (cl:format cl:nil "Panic: ~A" desc))))
      ((RevertCustom selector _data)
       (lisp String (selector)
         (cl:let ((hex (web3/types:hex-encode selector)))
           (cl:format cl:nil "custom error 0x~A" hex))))
      ((RevertEmpty) "execution reverted"))))
