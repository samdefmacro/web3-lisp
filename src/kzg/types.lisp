;;;; KZG types
;;;; Type definitions for KZG commitments

(in-package #:web3/kzg)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; KZG Constants
  ;;; =========================================================================

  (declare +commitment-size+ UFix)
  (define +commitment-size+ 48)  ;; G1 point compressed

  (declare +proof-size+ UFix)
  (define +proof-size+ 48)  ;; G1 point compressed

  ;;; =========================================================================
  ;;; KZG Types
  ;;; =========================================================================

  ;; KZG commitment (48-byte compressed G1 point)
  (define-type-alias KZGCommitment types:Bytes)

  ;; KZG proof (48-byte compressed G1 point)
  (define-type-alias KZGProof types:Bytes)

  ;; KZG context holding the trusted setup
  ;; Wraps a pointer to the c-kzg KZGSettings struct
  (repr :native cl:t)
  (define-type KZGContext
    "KZG context containing trusted setup parameters"))
