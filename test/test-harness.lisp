;;; CL test harness — used by every test/*-tests.lisp file
;;; Owns the test-case macro, pass/fail counters, and Coalton-value predicates.

(in-package #:web3-tests/runner)

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defmacro test-case (name &body body)
  "Run a test case and track results"
  `(handler-case
       (progn
         ,@body
         (incf *tests-passed*)
         (format t "  ~A: PASS~%" ,name))
     (error (e)
       (incf *tests-failed*)
       (format t "  ~A: FAIL - ~A~%" ,name e))))

(defun bytes-equal (a b)
  "Check if two byte arrays are equal"
  (and (= (length a) (length b))
       (every #'= (coerce a 'list) (coerce b 'list))))

;; Helper to check type by string to avoid package lock issues
(defun %type-contains (obj type-substr)
  "Check if obj's type name contains type-substr"
  (search type-substr (symbol-name (type-of obj))))

(defun result-ok-p (r)
  "Check if a Coalton Result is Ok"
  (%type-contains r "OK"))

(defun result-err-p (r)
  "Check if a Coalton Result is Err"
  (%type-contains r "ERR"))

(defun optional-some-p (opt)
  "Check if a Coalton Optional is Some"
  ;; Some values are not eq to None
  (not (eq opt coalton-library/classes:None)))

(defun optional-none-p (opt)
  "Check if a Coalton Optional is None"
  ;; None is a singleton - use eq comparison
  (eq opt coalton-library/classes:None))

(defun result-value (r)
  "Extract value from a Coalton Result/Ok or Optional/Some"
  (slot-value r 'coalton-library/classes::_0))

(defun is-ok (r)
  "Alias for result-ok-p"
  (result-ok-p r))
