;;;; SIWE types
;;;; Type definitions for Sign-In with Ethereum

(in-package #:web3/siwe)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; SIWE Message Type (ERC-4361)
  ;;; =========================================================================

  (define-struct SiweMessage
    "Sign-In with Ethereum message as defined by ERC-4361"
    (siwe-domain String)                        ;; RFC 3986 authority (e.g., example.com)
    (siwe-address addr:Address)                 ;; Ethereum address
    (siwe-statement (Optional String))          ;; Human-readable statement
    (siwe-uri String)                           ;; RFC 3986 URI
    (siwe-version String)                       ;; Version (always \"1\")
    (siwe-chain-id UFix)                        ;; EIP-155 chain ID
    (siwe-nonce String)                         ;; Randomized token (8+ alphanumeric)
    (siwe-issued-at String)                     ;; ISO 8601 datetime
    (siwe-expiration-time (Optional String))    ;; ISO 8601 datetime (optional)
    (siwe-not-before (Optional String))         ;; ISO 8601 datetime (optional)
    (siwe-request-id (Optional String))         ;; Application-specific ID (optional)
    (siwe-resources (Optional (List String))))  ;; List of resources (optional)

  ;;; =========================================================================
  ;;; Constructor Helper
  ;;; =========================================================================

  (declare make-siwe-message (String -> addr:Address -> String -> UFix -> String -> String -> SiweMessage))
  (define (make-siwe-message domain address uri chain-id nonce issued-at)
    "Create a minimal SIWE message with required fields.
     Optional fields are set to None."
    (SiweMessage domain
                 address
                 None                    ;; statement
                 uri
                 "1"                     ;; version (always 1)
                 chain-id
                 nonce
                 issued-at
                 None                    ;; expiration-time
                 None                    ;; not-before
                 None                    ;; request-id
                 None))                  ;; resources

  ;;; =========================================================================
  ;;; Explicit Accessors
  ;;; =========================================================================

  (declare siwe-domain (SiweMessage -> String))
  (define (siwe-domain msg)
    "Get the domain from a SIWE message"
    (.siwe-domain msg))

  (declare siwe-address (SiweMessage -> addr:Address))
  (define (siwe-address msg)
    "Get the address from a SIWE message"
    (.siwe-address msg))

  (declare siwe-statement (SiweMessage -> (Optional String)))
  (define (siwe-statement msg)
    "Get the statement from a SIWE message"
    (.siwe-statement msg))

  (declare siwe-uri (SiweMessage -> String))
  (define (siwe-uri msg)
    "Get the URI from a SIWE message"
    (.siwe-uri msg))

  (declare siwe-version (SiweMessage -> String))
  (define (siwe-version msg)
    "Get the version from a SIWE message"
    (.siwe-version msg))

  (declare siwe-chain-id (SiweMessage -> UFix))
  (define (siwe-chain-id msg)
    "Get the chain ID from a SIWE message"
    (.siwe-chain-id msg))

  (declare siwe-nonce (SiweMessage -> String))
  (define (siwe-nonce msg)
    "Get the nonce from a SIWE message"
    (.siwe-nonce msg))

  (declare siwe-issued-at (SiweMessage -> String))
  (define (siwe-issued-at msg)
    "Get the issued-at time from a SIWE message"
    (.siwe-issued-at msg))

  (declare siwe-expiration-time (SiweMessage -> (Optional String)))
  (define (siwe-expiration-time msg)
    "Get the expiration time from a SIWE message"
    (.siwe-expiration-time msg))

  (declare siwe-not-before (SiweMessage -> (Optional String)))
  (define (siwe-not-before msg)
    "Get the not-before time from a SIWE message"
    (.siwe-not-before msg))

  (declare siwe-request-id (SiweMessage -> (Optional String)))
  (define (siwe-request-id msg)
    "Get the request ID from a SIWE message"
    (.siwe-request-id msg))

  (declare siwe-resources (SiweMessage -> (Optional (List String))))
  (define (siwe-resources msg)
    "Get the resources list from a SIWE message"
    (.siwe-resources msg)))
