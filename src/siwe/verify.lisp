;;;; SIWE verification
;;;; Signature validation and message verification

(in-package #:web3/siwe)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Time Utilities
  ;;; =========================================================================

  (declare %current-iso8601 (Unit -> String))
  (define (%current-iso8601 _)
    "Get current time in ISO 8601 format (UTC)"
    (lisp String ()
      (cl:multiple-value-bind (sec minute hour day month year)
          (cl:decode-universal-time (cl:get-universal-time) 0)
        (cl:format cl:nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
                   year month day hour minute sec))))

  (declare %parse-iso8601 (String -> Integer))
  (define (%parse-iso8601 timestamp)
    "Parse ISO 8601 timestamp to universal time"
    (lisp Integer (timestamp)
      (cl:handler-case
          ;; Simple ISO 8601 parser for common formats
          ;; Handles: YYYY-MM-DDTHH:MM:SSZ and YYYY-MM-DDTHH:MM:SS+00:00
          (cl:let* ((year (cl:parse-integer timestamp :start 0 :end 4))
                    (month (cl:parse-integer timestamp :start 5 :end 7))
                    (day (cl:parse-integer timestamp :start 8 :end 10))
                    (hour (cl:parse-integer timestamp :start 11 :end 13))
                    (minute (cl:parse-integer timestamp :start 14 :end 16))
                    (sec (cl:parse-integer timestamp :start 17 :end 19)))
            (cl:encode-universal-time sec minute hour day month year 0))
        (cl:error () 0))))

  (declare %current-universal-time (Unit -> Integer))
  (define (%current-universal-time _)
    "Get current universal time"
    (lisp Integer ()
      (cl:get-universal-time)))

  ;;; =========================================================================
  ;;; Time Validation
  ;;; =========================================================================

  (declare siwe-message-expired? (SiweMessage -> Boolean))
  (define (siwe-message-expired? msg)
    "Check if a SIWE message has expired.
     Returns True if current time is past expiration-time."
    (match (.siwe-expiration-time msg)
      ((None) False)  ;; No expiration = never expires
      ((Some exp-time)
       (let ((exp-universal (%parse-iso8601 exp-time))
             (now (%current-universal-time Unit)))
         (> now exp-universal)))))

  (declare siwe-message-not-yet-valid? (SiweMessage -> Boolean))
  (define (siwe-message-not-yet-valid? msg)
    "Check if a SIWE message is not yet valid (before not-before time).
     Returns True if current time is before not-before."
    (match (.siwe-not-before msg)
      ((None) False)  ;; No not-before = immediately valid
      ((Some not-before-time)
       (let ((nb-universal (%parse-iso8601 not-before-time))
             (now (%current-universal-time Unit)))
         (< now nb-universal)))))

  ;;; =========================================================================
  ;;; Signature Validation
  ;;; =========================================================================

  (declare %string-to-bytes (String -> types:Bytes))
  (define (%string-to-bytes s)
    "Convert string to bytes (UTF-8)"
    (lisp types:Bytes (s)
      (cl:let* ((octets (cl:map 'cl:vector #'cl:char-code s))
                (len (cl:length octets))
                (result (cl:make-array len :element-type 'cl:t
                                          :fill-pointer len
                                          :adjustable cl:t)))
        (cl:dotimes (i len result)
          (cl:setf (cl:aref result i) (cl:aref octets i))))))

  (declare validate-siwe-signature (String -> sig:Signature -> addr:Address -> Boolean))
  (define (validate-siwe-signature message-str signature expected-address)
    "Validate that a SIWE message was signed by the expected address.
     Uses EIP-191 personal sign verification."
    (let ((message-bytes (%string-to-bytes message-str)))
      (sig:verify-personal-signature message-bytes signature expected-address)))

  ;;; =========================================================================
  ;;; Full Message Verification
  ;;; =========================================================================

  (declare verify-siwe-message (SiweMessage -> sig:Signature -> (types:Web3Result Boolean)))
  (define (verify-siwe-message msg signature)
    "Fully verify a SIWE message:
     1. Check message is not expired
     2. Check message is not before valid time
     3. Verify signature matches the address in the message"
    (cond
      ;; Check expiration
      ((siwe-message-expired? msg)
       (Err (types:HexError "SIWE message has expired")))
      ;; Check not-before
      ((siwe-message-not-yet-valid? msg)
       (Err (types:HexError "SIWE message is not yet valid")))
      ;; Verify signature
      (True
       (let ((message-str (create-siwe-message msg))
             (expected-address (.siwe-address msg)))
         (if (validate-siwe-signature message-str signature expected-address)
             (Ok True)
             (Err (types:HexError "Invalid signature"))))))))


;;; =========================================================================
;;; Exports
;;; =========================================================================

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:export '(SiweMessage
               make-siwe-message
               siwe-domain
               siwe-address
               siwe-statement
               siwe-uri
               siwe-version
               siwe-chain-id
               siwe-nonce
               siwe-issued-at
               siwe-expiration-time
               siwe-not-before
               siwe-request-id
               siwe-resources
               create-siwe-message
               parse-siwe-message
               validate-siwe-signature
               verify-siwe-message
               siwe-message-expired?
               siwe-message-not-yet-valid?
               generate-siwe-nonce)
             (cl:find-package '#:web3/siwe)))
