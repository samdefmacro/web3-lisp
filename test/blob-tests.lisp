;;; Blob module tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Blob Tests
;;; =========================================================================

(defun run-blob-tests ()
  (format t "~%=== Blob Tests ===~%")

  ;;; =========================================================================
  ;;; Constants Tests
  ;;; =========================================================================

  (test-case "blob constants are correct"
    (assert (= (coalton:coalton web3/blob:+bytes-per-field-element+) 32))
    (assert (= (coalton:coalton web3/blob:+field-elements-per-blob+) 4096))
    (assert (= (coalton:coalton web3/blob:+bytes-per-blob+) 131072))
    (assert (= (coalton:coalton web3/blob:+max-blobs-per-block+) 6))
    (assert (= (coalton:coalton web3/blob:+usable-bytes-per-field-element+) 31)))

  ;;; =========================================================================
  ;;; empty-blob Tests
  ;;; =========================================================================

  (test-case "empty-blob has correct size"
    (let ((blob (coalton:coalton (web3/blob:empty-blob coalton:Unit))))
      (assert (= (length blob) 131072))))

  (test-case "empty-blob is all zeros"
    (let ((blob (coalton:coalton (web3/blob:empty-blob coalton:Unit))))
      (assert (every (lambda (b) (= b 0)) (coerce blob 'list)))))

  ;;; =========================================================================
  ;;; valid-blob? Tests
  ;;; =========================================================================

  (test-case "valid-blob? returns true for correct size"
    (let ((blob (coalton:coalton (web3/blob:empty-blob coalton:Unit))))
      (assert (eq (coalton:coalton
                   (web3/blob:valid-blob?
                    (coalton:lisp web3/types:Bytes () blob)))
                  coalton:True))))

  (test-case "valid-blob? returns false for wrong size"
    (let ((bad-blob (make-array 100 :element-type 't :fill-pointer 100 :adjustable t :initial-element 0)))
      (assert (eq (coalton:coalton
                   (web3/blob:valid-blob?
                    (coalton:lisp web3/types:Bytes () bad-blob)))
                  coalton:False))))

  ;;; =========================================================================
  ;;; blob-count-for-data Tests
  ;;; =========================================================================

  (test-case "blob-count-for-data returns 1 for small data"
    (assert (= (coalton:coalton (web3/blob:blob-count-for-data 100)) 1)))

  (test-case "blob-count-for-data returns 1 for max single blob data"
    ;; 126976 - 1 (terminator) = 126975 bytes fit in one blob
    (assert (= (coalton:coalton (web3/blob:blob-count-for-data 126975)) 1)))

  (test-case "blob-count-for-data returns 2 for data exceeding one blob"
    (assert (= (coalton:coalton (web3/blob:blob-count-for-data 126976)) 2)))

  ;;; =========================================================================
  ;;; to-blobs Tests
  ;;; =========================================================================

  (test-case "to-blobs produces single blob for small data"
    (let* ((data (make-array 100 :element-type 't :fill-pointer 100 :adjustable t :initial-element #x42))
           (blobs (coalton:coalton
                   (web3/blob:to-blobs
                    (coalton:lisp web3/types:Bytes () data)))))
      (assert (= (length blobs) 1))
      (let ((blob (first blobs)))
        (assert (= (length blob) 131072)))))

  (test-case "to-blobs field element starts with zero"
    (let* ((data (make-array 10 :element-type 't :fill-pointer 10 :adjustable t :initial-element #xff))
           (blobs (coalton:coalton
                   (web3/blob:to-blobs
                    (coalton:lisp web3/types:Bytes () data)))))
      (let ((blob (first blobs)))
        ;; First byte of each field element should be 0
        (assert (= (aref blob 0) 0))
        (assert (= (aref blob 32) 0))
        (assert (= (aref blob 64) 0)))))

  (test-case "to-blobs places data correctly"
    (let* ((data (make-array 5 :element-type 't :fill-pointer 5 :adjustable t
                             :initial-contents '(#x01 #x02 #x03 #x04 #x05)))
           (blobs (coalton:coalton
                   (web3/blob:to-blobs
                    (coalton:lisp web3/types:Bytes () data)))))
      (let ((blob (first blobs)))
        ;; Data should be at positions 1-5 (after the 0x00 prefix)
        (assert (= (aref blob 1) #x01))
        (assert (= (aref blob 2) #x02))
        (assert (= (aref blob 3) #x03))
        (assert (= (aref blob 4) #x04))
        (assert (= (aref blob 5) #x05))
        ;; Terminator at position 6
        (assert (= (aref blob 6) #x80)))))

  (test-case "to-blobs handles empty data"
    (let* ((data (make-array 0 :element-type 't :fill-pointer 0 :adjustable t))
           (blobs (coalton:coalton
                   (web3/blob:to-blobs
                    (coalton:lisp web3/types:Bytes () data)))))
      (assert (= (length blobs) 1))
      (let ((blob (first blobs)))
        ;; Should have terminator at position 1
        (assert (= (aref blob 1) #x80)))))

  ;;; =========================================================================
  ;;; from-blobs Tests
  ;;; =========================================================================

  (test-case "from-blobs recovers simple data"
    (let* ((original (make-array 10 :element-type 't :fill-pointer 10 :adjustable t
                                 :initial-contents '(#x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0a)))
           (blobs (coalton:coalton
                   (web3/blob:to-blobs
                    (coalton:lisp web3/types:Bytes () original))))
           (recovered (coalton:coalton
                       (web3/blob:from-blobs
                        (coalton:lisp (coalton:List web3/types:Bytes) () blobs)))))
      (assert (= (length recovered) 10))
      (assert (equalp recovered original))))

  (test-case "to-blobs and from-blobs roundtrip"
    ;; Test with various data sizes
    (dolist (size '(0 1 31 32 100 1000))
      (let* ((original (make-array size :element-type 't :fill-pointer size :adjustable t
                                   :initial-element #x42))
             (blobs (coalton:coalton
                     (web3/blob:to-blobs
                      (coalton:lisp web3/types:Bytes () original))))
             (recovered (coalton:coalton
                         (web3/blob:from-blobs
                          (coalton:lisp (coalton:List web3/types:Bytes) () blobs)))))
        (assert (= (length recovered) size))
        (assert (equalp recovered original)))))

  (test-case "to-blobs and from-blobs roundtrip with field element boundary"
    ;; 31 bytes fills exactly one field element's data portion
    (let* ((original (make-array 31 :element-type 't :fill-pointer 31 :adjustable t :initial-element #xab))
           (blobs (coalton:coalton
                   (web3/blob:to-blobs
                    (coalton:lisp web3/types:Bytes () original))))
           (recovered (coalton:coalton
                       (web3/blob:from-blobs
                        (coalton:lisp (coalton:List web3/types:Bytes) () blobs)))))
      (assert (= (length recovered) 31))
      (assert (equalp recovered original))))

  ;;; =========================================================================
  ;;; commitment-to-versioned-hash Tests
  ;;; =========================================================================

  (test-case "commitment-to-versioned-hash produces 32 bytes"
    (let* ((commitment (make-array 48 :element-type 't :fill-pointer 48 :adjustable t :initial-element #x01))
           (hash (coalton:coalton
                  (web3/blob:commitment-to-versioned-hash
                   (coalton:lisp web3/types:Bytes () commitment)))))
      (assert (= (length hash) 32))))

  (test-case "commitment-to-versioned-hash has 0x01 version prefix"
    (let* ((commitment (make-array 48 :element-type 't :fill-pointer 48 :adjustable t :initial-element #xab))
           (hash (coalton:coalton
                  (web3/blob:commitment-to-versioned-hash
                   (coalton:lisp web3/types:Bytes () commitment)))))
      (assert (= (aref hash 0) #x01))))

  (test-case "commitment-to-versioned-hash is deterministic"
    (let* ((commitment (make-array 48 :element-type 't :fill-pointer 48 :adjustable t :initial-element #xcd))
           (hash1 (coalton:coalton
                   (web3/blob:commitment-to-versioned-hash
                    (coalton:lisp web3/types:Bytes () commitment))))
           (hash2 (coalton:coalton
                   (web3/blob:commitment-to-versioned-hash
                    (coalton:lisp web3/types:Bytes () commitment)))))
      (assert (equalp hash1 hash2))))

  (test-case "commitment-to-versioned-hash differs for different inputs"
    (let* ((commitment1 (make-array 48 :element-type 't :fill-pointer 48 :adjustable t :initial-element #x01))
           (commitment2 (make-array 48 :element-type 't :fill-pointer 48 :adjustable t :initial-element #x02))
           (hash1 (coalton:coalton
                   (web3/blob:commitment-to-versioned-hash
                    (coalton:lisp web3/types:Bytes () commitment1))))
           (hash2 (coalton:coalton
                   (web3/blob:commitment-to-versioned-hash
                    (coalton:lisp web3/types:Bytes () commitment2)))))
      (assert (not (equalp hash1 hash2))))))
