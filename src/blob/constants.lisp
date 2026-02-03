;;;; Blob constants
;;;; EIP-4844 blob constants

(in-package #:web3/blob)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; EIP-4844 Constants
  ;;; =========================================================================

  (declare +bytes-per-field-element+ UFix)
  (define +bytes-per-field-element+ 32)

  (declare +field-elements-per-blob+ UFix)
  (define +field-elements-per-blob+ 4096)

  (declare +bytes-per-blob+ UFix)
  (define +bytes-per-blob+ 131072)  ;; 32 * 4096 = 128KB

  (declare +max-blobs-per-block+ UFix)
  (define +max-blobs-per-block+ 6)

  ;; Each 32-byte field element starts with 0x00 (BLS12-381 constraint)
  ;; So only 31 bytes of actual data per field element
  (declare +usable-bytes-per-field-element+ UFix)
  (define +usable-bytes-per-field-element+ 31)

  ;; Maximum usable data bytes per blob
  (declare +usable-bytes-per-blob+ UFix)
  (define +usable-bytes-per-blob+ 126976))  ;; 31 * 4096 = 126976 bytes
