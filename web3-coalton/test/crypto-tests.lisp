;;; Crypto module tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Keccak256 Tests
;;; =========================================================================

(defun run-crypto-tests ()
  (format t "~%=== Crypto Tests ===~%")

  (test-case "keccak256 empty input"
    (assert (eq (web3-tests:test-keccak256-empty coalton:Unit) coalton:True)))

  (test-case "keccak256 'hello'"
    (assert (eq (web3-tests:test-keccak256-hello coalton:Unit) coalton:True)))

  (test-case "keccak256 produces 32 bytes"
    (let ((result (coalton:coalton
                   (web3/crypto:keccak256 (web3/types:make-bytes 100)))))
      (assert (= (length result) 32))))

  (test-case "keccak256 empty - full hash (direct CL)"
    ;; keccak256("") = c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470
    (let ((result (coalton:coalton
                   (web3/crypto:keccak256 (web3/types:bytes-empty coalton:Unit)))))
      (assert (= (aref result 0) #xc5))
      (assert (= (aref result 1) #xd2))
      (assert (= (aref result 2) #x46))
      (assert (= (aref result 3) #x01))
      (assert (= (aref result 31) #x70))))

  (test-case "keccak256 deterministic"
    (let ((h1 (coalton:coalton
               (web3/crypto:keccak256
                (web3/types:bytes-from-list (coalton:Cons 1 (coalton:Cons 2 coalton:Nil))))))
          (h2 (coalton:coalton
               (web3/crypto:keccak256
                (web3/types:bytes-from-list (coalton:Cons 1 (coalton:Cons 2 coalton:Nil)))))))
      (assert (bytes-equal h1 h2))))

  (test-case "keccak256 different inputs produce different hashes"
    (let ((h1 (coalton:coalton
               (web3/crypto:keccak256
                (web3/types:bytes-from-list (coalton:Cons 1 coalton:Nil)))))
          (h2 (coalton:coalton
               (web3/crypto:keccak256
                (web3/types:bytes-from-list (coalton:Cons 2 coalton:Nil))))))
      (assert (not (bytes-equal h1 h2))))))
