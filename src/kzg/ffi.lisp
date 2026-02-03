;;;; KZG FFI bindings
;;;; CFFI bindings to c-kzg-4844 library

(cl:in-package #:web3/kzg)

;;; =========================================================================
;;; FFI Type Definitions
;;; =========================================================================

(cffi:define-foreign-library c-kzg
  (:darwin (:or "libc_kzg_4844.dylib" "libc-kzg-4844.dylib"))
  (:unix (:or "libc_kzg_4844.so" "libc-kzg-4844.so"))
  (:windows "c_kzg_4844.dll")
  (cl:t (:default "libc_kzg_4844")))

;; Track if library is loaded
(cl:defvar *kzg-library-loaded* cl:nil)

(cl:defun ensure-kzg-library ()
  "Load the c-kzg library if not already loaded"
  (cl:unless *kzg-library-loaded*
    (cl:handler-case
        (cl:progn
          (cffi:load-foreign-library 'c-kzg)
          (cl:setf *kzg-library-loaded* cl:t))
      (cffi:load-foreign-library-error ()
        ;; Library not found - KZG functions will return errors
        cl:nil))))

;;; =========================================================================
;;; C Types and Structs
;;; =========================================================================

;; c-kzg return type
(cffi:defcenum c-kzg-ret
  (:ok 0)
  (:badargs 1)
  (:error 2)
  (:malloc 3))

;; Blob (131072 bytes)
(cffi:defctype blob (:array :uint8 131072))

;; Commitment (48 bytes)
(cffi:defctype kzg-commitment-bytes (:array :uint8 48))

;; Proof (48 bytes)
(cffi:defctype kzg-proof-bytes (:array :uint8 48))

;; Bytes32 (32 bytes)
(cffi:defctype bytes32 (:array :uint8 32))

;;; =========================================================================
;;; FFI Function Declarations
;;; =========================================================================

;; Load trusted setup from file
(cffi:defcfun ("load_trusted_setup_file" %load-trusted-setup-file) c-kzg-ret
  (out :pointer)  ;; KZGSettings*
  (fp :pointer))  ;; FILE*

;; Load trusted setup from precomputed data
(cffi:defcfun ("load_trusted_setup" %load-trusted-setup) c-kzg-ret
  (out :pointer)           ;; KZGSettings*
  (g1-monomial :pointer)   ;; g1_bytes (48 * 4096)
  (g1-monomial-count :uint64)
  (g2-monomial :pointer)   ;; g2_bytes (96 * 65)
  (g2-monomial-count :uint64)
  (g1-lagrange :pointer)   ;; g1_bytes (48 * 4096)
  (g1-lagrange-count :uint64))

;; Free trusted setup
(cffi:defcfun ("free_trusted_setup" %free-trusted-setup) :void
  (s :pointer))  ;; KZGSettings*

;; Compute blob commitment
(cffi:defcfun ("blob_to_kzg_commitment" %blob-to-kzg-commitment) c-kzg-ret
  (out :pointer)     ;; KZGCommitment*
  (blob :pointer)    ;; Blob*
  (s :pointer))      ;; KZGSettings*

;; Compute blob KZG proof
(cffi:defcfun ("compute_blob_kzg_proof" %compute-blob-kzg-proof) c-kzg-ret
  (out-proof :pointer)        ;; KZGProof*
  (out-y :pointer)            ;; Bytes32*
  (blob :pointer)             ;; Blob*
  (commitment :pointer)       ;; KZGCommitment*
  (s :pointer))               ;; KZGSettings*

;; Verify blob KZG proof
(cffi:defcfun ("verify_blob_kzg_proof" %verify-blob-kzg-proof) c-kzg-ret
  (out :pointer)        ;; bool*
  (blob :pointer)       ;; Blob*
  (commitment :pointer) ;; KZGCommitment*
  (proof :pointer)      ;; KZGProof*
  (s :pointer))         ;; KZGSettings*

;; Compute KZG proof at evaluation point z
(cffi:defcfun ("compute_kzg_proof" %compute-kzg-proof) c-kzg-ret
  (out-proof :pointer)  ;; KZGProof*
  (out-y :pointer)      ;; Bytes32*
  (blob :pointer)       ;; Blob*
  (z :pointer)          ;; Bytes32*
  (s :pointer))         ;; KZGSettings*

;; Verify KZG proof
(cffi:defcfun ("verify_kzg_proof" %verify-kzg-proof) c-kzg-ret
  (out :pointer)        ;; bool*
  (commitment :pointer) ;; KZGCommitment*
  (z :pointer)          ;; Bytes32*
  (y :pointer)          ;; Bytes32*
  (proof :pointer)      ;; KZGProof*
  (s :pointer))         ;; KZGSettings*

;; Batch verification
(cffi:defcfun ("verify_blob_kzg_proof_batch" %verify-blob-kzg-proof-batch) c-kzg-ret
  (out :pointer)          ;; bool*
  (blobs :pointer)        ;; Blob*
  (commitments :pointer)  ;; KZGCommitment*
  (proofs :pointer)       ;; KZGProof*
  (count :uint64)         ;; size_t
  (s :pointer))           ;; KZGSettings*

;;; =========================================================================
;;; KZGSettings size (approximate, depends on build)
;;; =========================================================================

;; The KZGSettings struct is opaque - we allocate enough space
;; Actual size depends on c-kzg build, this is conservative
(cl:defconstant +kzg-settings-size+ 65536)
