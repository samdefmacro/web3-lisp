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

)
