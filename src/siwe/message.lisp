;;;; SIWE message creation and parsing
;;;; ERC-4361 message format handling

(in-package #:web3/siwe)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Nonce Generation
  ;;; =========================================================================

  (declare generate-siwe-nonce (Unit -> String))
  (define (generate-siwe-nonce _)
    "Generate a random alphanumeric nonce (16 characters).
     The nonce should be at least 8 alphanumeric characters."
    (lisp String ()
      (cl:let* ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
                (len (cl:length chars))
                (result (cl:make-string 16)))
        (cl:dotimes (i 16)
          (cl:setf (cl:char result i)
                   (cl:char chars (cl:random len))))
        result)))

  ;;; =========================================================================
  ;;; Message Creation
  ;;; =========================================================================

  (declare create-siwe-message (SiweMessage -> String))
  (define (create-siwe-message msg)
    "Create the ERC-4361 message string from a SiweMessage struct.
     This is the string that should be signed."
    (lisp String (msg)
      (cl:let* ((domain (siwe-domain msg))
                (address (addr:address-to-checksum-hex (siwe-address msg)))
                (statement (siwe-statement msg))
                (uri (siwe-uri msg))
                (version (siwe-version msg))
                (chain-id (siwe-chain-id msg))
                (nonce (siwe-nonce msg))
                (issued-at (siwe-issued-at msg))
                (expiration-time (siwe-expiration-time msg))
                (not-before (siwe-not-before msg))
                (request-id (siwe-request-id msg))
                (resources (siwe-resources msg))
                ;; Build message
                (lines (cl:list)))
        ;; Line 1: "{domain} wants you to sign in with your Ethereum account:"
        (cl:push (cl:format cl:nil "~A wants you to sign in with your Ethereum account:" domain) lines)
        ;; Line 2: address
        (cl:push address lines)
        ;; Line 3: empty line before statement (if statement exists)
        (cl:when (web3/types:%is-some-p statement)
          (cl:push "" lines)
          (cl:push (web3/types:%unwrap-some statement) lines))
        ;; Empty line before fields
        (cl:push "" lines)
        ;; Required fields
        (cl:push (cl:format cl:nil "URI: ~A" uri) lines)
        (cl:push (cl:format cl:nil "Version: ~A" version) lines)
        (cl:push (cl:format cl:nil "Chain ID: ~D" chain-id) lines)
        (cl:push (cl:format cl:nil "Nonce: ~A" nonce) lines)
        (cl:push (cl:format cl:nil "Issued At: ~A" issued-at) lines)
        ;; Optional fields
        (cl:when (web3/types:%is-some-p expiration-time)
          (cl:push (cl:format cl:nil "Expiration Time: ~A"
                              (web3/types:%unwrap-some expiration-time))
                   lines))
        (cl:when (web3/types:%is-some-p not-before)
          (cl:push (cl:format cl:nil "Not Before: ~A"
                              (web3/types:%unwrap-some not-before))
                   lines))
        (cl:when (web3/types:%is-some-p request-id)
          (cl:push (cl:format cl:nil "Request ID: ~A"
                              (web3/types:%unwrap-some request-id))
                   lines))
        ;; Resources
        (cl:when (web3/types:%is-some-p resources)
          (cl:let ((res-list (web3/types:%unwrap-some resources)))
            (cl:when res-list
              (cl:push "Resources:" lines)
              (cl:dolist (res res-list)
                (cl:push (cl:format cl:nil "- ~A" res) lines)))))
        ;; Join lines with newlines
        (cl:format cl:nil "~{~A~^~%~}" (cl:nreverse lines)))))

  ;;; =========================================================================
  ;;; Message Parsing
  ;;; =========================================================================

  (declare parse-siwe-message (String -> (types:Web3Result SiweMessage)))
  (define (parse-siwe-message message-str)
    "Parse an ERC-4361 message string into a SiweMessage struct."
    (lisp (types:Web3Result SiweMessage) (message-str)
      (cl:handler-case
          (cl:let* ((lines (split-sequence:split-sequence #\Newline message-str))
                    (line-idx 0)
                    (domain cl:nil)
                    (address cl:nil)
                    (statement cl:nil)
                    (uri cl:nil)
                    (version cl:nil)
                    (chain-id cl:nil)
                    (nonce cl:nil)
                    (issued-at cl:nil)
                    (expiration-time cl:nil)
                    (not-before cl:nil)
                    (request-id cl:nil)
                    (resources cl:nil))
            ;; Parse line 1: domain
            (cl:let ((line1 (cl:nth line-idx lines)))
              (cl:unless (cl:search "wants you to sign in with your Ethereum account:" line1)
                (cl:error "Invalid SIWE message format: missing header"))
              (cl:setf domain (cl:subseq line1 0
                                         (cl:search " wants you to sign in" line1))))
            (cl:incf line-idx)
            ;; Parse line 2: address
            (cl:setf address (cl:string-trim " " (cl:nth line-idx lines)))
            (cl:incf line-idx)
            ;; Skip empty lines and parse optional statement
            (cl:loop :while (cl:and (cl:< line-idx (cl:length lines))
                                    (cl:string= "" (cl:string-trim " " (cl:nth line-idx lines))))
                     :do (cl:incf line-idx))
            ;; Check if next line is a field or statement
            (cl:when (cl:< line-idx (cl:length lines))
              (cl:let ((next-line (cl:nth line-idx lines)))
                (cl:unless (cl:or (cl:search "URI:" next-line)
                                  (cl:search "Version:" next-line))
                  ;; It's a statement
                  (cl:setf statement next-line)
                  (cl:incf line-idx))))
            ;; Skip empty lines before fields
            (cl:loop :while (cl:and (cl:< line-idx (cl:length lines))
                                    (cl:string= "" (cl:string-trim " " (cl:nth line-idx lines))))
                     :do (cl:incf line-idx))
            ;; Parse fields
            (cl:loop :while (cl:< line-idx (cl:length lines))
                     :for line := (cl:nth line-idx lines)
                     :do (cl:cond
                           ((cl:search "URI: " line)
                            (cl:setf uri (cl:subseq line 5)))
                           ((cl:search "Version: " line)
                            (cl:setf version (cl:subseq line 9)))
                           ((cl:search "Chain ID: " line)
                            (cl:setf chain-id (cl:parse-integer (cl:subseq line 10))))
                           ((cl:search "Nonce: " line)
                            (cl:setf nonce (cl:subseq line 7)))
                           ((cl:search "Issued At: " line)
                            (cl:setf issued-at (cl:subseq line 11)))
                           ((cl:search "Expiration Time: " line)
                            (cl:setf expiration-time (cl:subseq line 17)))
                           ((cl:search "Not Before: " line)
                            (cl:setf not-before (cl:subseq line 12)))
                           ((cl:search "Request ID: " line)
                            (cl:setf request-id (cl:subseq line 12)))
                           ((cl:search "Resources:" line)
                            ;; Parse resources list
                            (cl:incf line-idx)
                            (cl:loop :while (cl:and (cl:< line-idx (cl:length lines))
                                                    (cl:search "- " (cl:nth line-idx lines)))
                                     :do (cl:push (cl:subseq (cl:nth line-idx lines) 2) resources)
                                         (cl:incf line-idx))
                            (cl:setf resources (cl:nreverse resources))
                            (cl:decf line-idx)))
                         (cl:incf line-idx))
            ;; Validate required fields
            (cl:unless (cl:and domain address uri version chain-id nonce issued-at)
              (cl:error "Missing required SIWE fields"))
            ;; Parse address
            (cl:let ((addr-result (coalton (addr:address-from-hex (lisp String () address)))))
              (cl:if (web3/types:%result-ok-p addr-result)
                     (coalton-prelude:Ok
                      (SiweMessage domain
                                   (web3/types:%unwrap-ok addr-result)
                                   (cl:if statement
                                          (coalton-prelude:Some statement)
                                          coalton-prelude:None)
                                   uri
                                   version
                                   chain-id
                                   nonce
                                   issued-at
                                   (cl:if expiration-time
                                          (coalton-prelude:Some expiration-time)
                                          coalton-prelude:None)
                                   (cl:if not-before
                                          (coalton-prelude:Some not-before)
                                          coalton-prelude:None)
                                   (cl:if request-id
                                          (coalton-prelude:Some request-id)
                                          coalton-prelude:None)
                                   (cl:if resources
                                          (coalton-prelude:Some resources)
                                          coalton-prelude:None)))
                     (cl:error "Invalid address"))))
        (cl:error (e)
          (coalton-prelude:Err
           (web3/types:HexError (cl:format cl:nil "Failed to parse SIWE message: ~A" e))))))))
