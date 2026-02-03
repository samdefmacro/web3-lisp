;;; KZG module tests - Pure Common Lisp
;;; Note: KZG tests require the c-kzg library and trusted setup file
;;; Tests are designed to gracefully skip if library is not available

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; KZG Tests
;;; =========================================================================

(defun run-kzg-tests ()
  (format t "~%=== KZG Tests ===~%")

  ;;; =========================================================================
  ;;; Context Tests
  ;;; =========================================================================

  (test-case "make-kzg-context fails with missing file"
    (let ((result (coalton:coalton
                   (web3/kzg:make-kzg-context "/nonexistent/path/trusted_setup.txt"))))
      ;; Should return error since file doesn't exist
      (assert (result-err-p result))))

  (test-case "kzg-context-loaded? returns false for null pointer"
    ;; This tests the basic function without needing actual context
    (assert (eq (coalton:coalton
                 (web3/kzg:kzg-context-loaded?
                  (coalton:lisp web3/kzg:KZGContext ()
                    (cffi:null-pointer))))
                coalton:False)))

  ;;; =========================================================================
  ;;; Commitment Tests (Library-Dependent)
  ;;; These tests document expected behavior but may skip without c-kzg
  ;;; =========================================================================

  (test-case "blob-to-commitment rejects wrong blob size"
    ;; This should fail validation regardless of library availability
    (let* ((bad-blob (make-array 100 :element-type 't :fill-pointer 100 :adjustable t :initial-element 0))
           (null-ctx (cffi:null-pointer))
           (result (coalton:coalton
                    (web3/kzg:blob-to-commitment
                     (coalton:lisp web3/types:Bytes () bad-blob)
                     (coalton:lisp web3/kzg:KZGContext () null-ctx)))))
      (assert (result-err-p result))))

  (test-case "compute-blob-proof rejects wrong blob size"
    (let* ((bad-blob (make-array 100 :element-type 't :fill-pointer 100 :adjustable t :initial-element 0))
           (commitment (make-array 48 :element-type 't :fill-pointer 48 :adjustable t :initial-element 0))
           (null-ctx (cffi:null-pointer))
           (result (coalton:coalton
                    (web3/kzg:compute-blob-proof
                     (coalton:lisp web3/types:Bytes () bad-blob)
                     (coalton:lisp web3/kzg:KZGCommitment () commitment)
                     (coalton:lisp web3/kzg:KZGContext () null-ctx)))))
      (assert (result-err-p result))))

  (test-case "compute-blob-proof rejects wrong commitment size"
    (let* ((blob (coalton:coalton (web3/blob:empty-blob coalton:Unit)))
           (bad-commitment (make-array 32 :element-type 't :fill-pointer 32 :adjustable t :initial-element 0))
           (null-ctx (cffi:null-pointer))
           (result (coalton:coalton
                    (web3/kzg:compute-blob-proof
                     (coalton:lisp web3/types:Bytes () blob)
                     (coalton:lisp web3/kzg:KZGCommitment () bad-commitment)
                     (coalton:lisp web3/kzg:KZGContext () null-ctx)))))
      (assert (result-err-p result))))

  ;;; =========================================================================
  ;;; Verification Tests (Library-Dependent)
  ;;; =========================================================================

  (test-case "verify-blob-proof returns false for wrong sizes"
    (let* ((bad-blob (make-array 100 :element-type 't :fill-pointer 100 :adjustable t :initial-element 0))
           (commitment (make-array 48 :element-type 't :fill-pointer 48 :adjustable t :initial-element 0))
           (proof (make-array 48 :element-type 't :fill-pointer 48 :adjustable t :initial-element 0))
           (null-ctx (cffi:null-pointer)))
      (assert (eq (coalton:coalton
                   (web3/kzg:verify-blob-proof
                    (coalton:lisp web3/types:Bytes () bad-blob)
                    (coalton:lisp web3/kzg:KZGCommitment () commitment)
                    (coalton:lisp web3/kzg:KZGProof () proof)
                    (coalton:lisp web3/kzg:KZGContext () null-ctx)))
                  coalton:False))))

  (test-case "verify-blob-proofs returns true for empty list"
    (let ((null-ctx (cffi:null-pointer)))
      (assert (eq (coalton:coalton
                   (web3/kzg:verify-blob-proofs
                    coalton:Nil
                    coalton:Nil
                    coalton:Nil
                    (coalton:lisp web3/kzg:KZGContext () null-ctx)))
                  coalton:True))))

  (test-case "verify-blob-proofs returns false for mismatched list lengths"
    (let* ((blob (coalton:coalton (web3/blob:empty-blob coalton:Unit)))
           (commitment (make-array 48 :element-type 't :fill-pointer 48 :adjustable t :initial-element 0))
           (null-ctx (cffi:null-pointer)))
      ;; One blob, one commitment, but no proofs
      (assert (eq (coalton:coalton
                   (web3/kzg:verify-blob-proofs
                    (coalton:Cons (coalton:lisp web3/types:Bytes () blob) coalton:Nil)
                    (coalton:Cons (coalton:lisp web3/kzg:KZGCommitment () commitment) coalton:Nil)
                    coalton:Nil
                    (coalton:lisp web3/kzg:KZGContext () null-ctx)))
                  coalton:False))))

  ;;; =========================================================================
  ;;; compute-kzg-proof Tests
  ;;; =========================================================================

  (test-case "compute-kzg-proof rejects wrong z size"
    (let* ((blob (coalton:coalton (web3/blob:empty-blob coalton:Unit)))
           (bad-z (make-array 16 :element-type 't :fill-pointer 16 :adjustable t :initial-element 0))
           (null-ctx (cffi:null-pointer))
           (result (coalton:coalton
                    (web3/kzg:compute-kzg-proof
                     (coalton:lisp web3/types:Bytes () blob)
                     (coalton:lisp web3/types:Bytes () bad-z)
                     (coalton:lisp web3/kzg:KZGContext () null-ctx)))))
      (assert (result-err-p result))))

  ;;; =========================================================================
  ;;; Batch Operation Tests
  ;;; =========================================================================

  (test-case "blobs-to-commitments handles empty list"
    (let* ((null-ctx (cffi:null-pointer))
           (result (coalton:coalton
                    (web3/kzg:blobs-to-commitments
                     coalton:Nil
                     (coalton:lisp web3/kzg:KZGContext () null-ctx)))))
      ;; Empty input should produce empty output (Ok Nil)
      (assert (result-ok-p result))))

  ;;; =========================================================================
  ;;; Integration Tests (Require c-kzg and trusted setup)
  ;;; These tests are skipped if library/setup not available
  ;;; =========================================================================

  ;; Note: Full integration tests would require:
  ;; 1. c-kzg library installed
  ;; 2. Ethereum mainnet trusted setup file
  ;; Example test structure for when available:
  ;;
  ;; (test-case "full blob commitment/proof cycle"
  ;;   (let ((setup-path "/path/to/trusted_setup.txt"))
  ;;     (when (probe-file setup-path)
  ;;       (let ((ctx-result (coalton:coalton (web3/kzg:make-kzg-context setup-path))))
  ;;         (when (result-ok-p ctx-result)
  ;;           (let* ((ctx (result-value ctx-result))
  ;;                  (blob (coalton:coalton (web3/blob:empty-blob coalton:Unit)))
  ;;                  (commit-result (coalton:coalton
  ;;                                  (web3/kzg:blob-to-commitment
  ;;                                   (coalton:lisp web3/types:Bytes () blob)
  ;;                                   (coalton:lisp web3/kzg:KZGContext () ctx)))))
  ;;             (assert (result-ok-p commit-result))
  ;;             (let* ((commitment (result-value commit-result))
  ;;                    (proof-result (coalton:coalton
  ;;                                   (web3/kzg:compute-blob-proof
  ;;                                    (coalton:lisp web3/types:Bytes () blob)
  ;;                                    (coalton:lisp web3/kzg:KZGCommitment () commitment)
  ;;                                    (coalton:lisp web3/kzg:KZGContext () ctx)))))
  ;;               (assert (result-ok-p proof-result))
  ;;               (let ((proof (result-value proof-result)))
  ;;                 (assert (eq (coalton:coalton
  ;;                              (web3/kzg:verify-blob-proof
  ;;                               (coalton:lisp web3/types:Bytes () blob)
  ;;                               (coalton:lisp web3/kzg:KZGCommitment () commitment)
  ;;                               (coalton:lisp web3/kzg:KZGProof () proof)
  ;;                               (coalton:lisp web3/kzg:KZGContext () ctx)))
  ;;                             coalton:True)))))))))

  (format t "  Note: Full KZG tests require c-kzg library and trusted setup~%"))
