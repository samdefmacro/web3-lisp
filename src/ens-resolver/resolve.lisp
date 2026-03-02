;;;; ENS Resolver - Live ENS resolution via JSON-RPC provider
;;;;
;;;; Combines web3/ens (calldata builders, decoders) with web3/provider
;;;; (eth-call) to perform live ENS name resolution.

(in-package #:web3/ens-resolver)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Internal Helpers
  ;;; =========================================================================

  (declare %is-zero-address (addr:Address -> Boolean))
  (define (%is-zero-address address)
    "Check if an address is the zero address (all 20 bytes are zero)."
    (types:bytes-equal? (addr:address-bytes address)
                        (addr:address-bytes addr:address-zero)))

  (declare %parse-registry-address (types:Web3Result addr:Address))
  (define %parse-registry-address
    "Parse the ENS registry address constant."
    (addr:address-from-hex ens:ens-registry-address))

  ;; Use types:bytes-append (exported 2-arg concat) instead of local wrapper

  (declare %contenthash-calldata (types:Bytes -> types:Bytes))
  (define (%contenthash-calldata node)
    "Build calldata for contenthash(bytes32 node)."
    (types:bytes-append
     ens:resolver-contenthash-selector
     (types:bytes-pad-left (types:bytes-length node) node)))

  (declare %is-empty-string (String -> Boolean))
  (define (%is-empty-string s)
    (lisp Boolean (s)
      (cl:if (cl:zerop (cl:length s))
             coalton:True
             coalton:False)))

  (declare %is-empty-bytes (types:Bytes -> Boolean))
  (define (%is-empty-bytes b)
    (== (types:bytes-length b) 0))

  ;;; =========================================================================
  ;;; Core Resolution Functions
  ;;; =========================================================================

  (declare get-resolver (provider:HttpProvider -> String -> (types:Web3Result (Optional addr:Address))))
  (define (get-resolver provider name)
    "Get the resolver address for an ENS name.
     Returns Ok None if no resolver is set."
    (match %parse-registry-address
      ((Err e) (Err e))
      ((Ok registry)
       (let ((calldata (ens:ens-resolver-calldata (ens:namehash name))))
         (match (provider:eth-call provider None registry calldata)
           ((Err e) (Err e))
           ((Ok result)
            (match (ens:decode-address-result result)
              ((Err e) (Err e))
              ((Ok resolver-addr)
               (if (%is-zero-address resolver-addr)
                   (Ok None)
                   (Ok (Some resolver-addr)))))))))))

  (declare resolve-name (provider:HttpProvider -> String -> (types:Web3Result (Optional addr:Address))))
  (define (resolve-name provider name)
    "Resolve an ENS name to an Ethereum address.
     Returns Ok None if the name has no resolver or no address record."
    (match (get-resolver provider name)
      ((Err e) (Err e))
      ((Ok (None)) (Ok None))
      ((Ok (Some resolver-addr))
       (let ((calldata (ens:resolver-addr-calldata (ens:namehash name))))
         (match (provider:eth-call provider None resolver-addr calldata)
           ((Err e) (Err e))
           ((Ok result)
            (match (ens:decode-address-result result)
              ((Err e) (Err e))
              ((Ok address)
               (if (%is-zero-address address)
                   (Ok None)
                   (Ok (Some address)))))))))))

  (declare lookup-address (provider:HttpProvider -> addr:Address -> (types:Web3Result (Optional String))))
  (define (lookup-address provider address)
    "Reverse-resolve an Ethereum address to an ENS name.
     Returns Ok None if no reverse record is set."
    (let ((addr-hex (addr:address-to-hex address)))
      (match %parse-registry-address
        ((Err e) (Err e))
        ((Ok registry)
         (let ((rev-node (ens:reverse-node addr-hex))
               (calldata (ens:ens-resolver-calldata rev-node)))
           (match (provider:eth-call provider None registry calldata)
             ((Err e) (Err e))
             ((Ok result)
              (match (ens:decode-address-result result)
                ((Err e) (Err e))
                ((Ok resolver-addr)
                 (if (%is-zero-address resolver-addr)
                     (Ok None)
                     (let ((name-calldata (ens:resolver-name-calldata rev-node)))
                       (match (provider:eth-call provider None resolver-addr name-calldata)
                         ((Err e) (Err e))
                         ((Ok name-result)
                          (match (ens:decode-name-result name-result)
                            ((Err e) (Err e))
                            ((Ok name)
                             (if (%is-empty-string name)
                                 (Ok None)
                                 (Ok (Some name))))))))))))))))))

  (declare get-text-record (provider:HttpProvider -> String -> String -> (types:Web3Result (Optional String))))
  (define (get-text-record provider name key)
    "Get a text record for an ENS name (e.g., 'url', 'avatar', 'com.twitter').
     Returns Ok None if no resolver or no text record."
    (match (get-resolver provider name)
      ((Err e) (Err e))
      ((Ok opt-resolver)
       (match opt-resolver
         ((None) (Ok None))
         ((Some resolver-addr)
          (let ((calldata (ens:resolver-text-calldata (ens:namehash name) key)))
            (match (provider:eth-call provider None resolver-addr calldata)
              ((Err e) (Err e))
              ((Ok result)
               (match (ens:decode-text-result result)
                 ((Err e) (Err e))
                 ((Ok text)
                  (if (%is-empty-string text)
                      (Ok None)
                      (Ok (Some text)))))))))))))

  (declare get-contenthash (provider:HttpProvider -> String -> (types:Web3Result (Optional types:Bytes))))
  (define (get-contenthash provider name)
    "Get the content hash for an ENS name (IPFS/Swarm).
     Returns Ok None if no resolver or no contenthash record.
     The raw bytes are returned; caller interprets the IPFS vs Swarm prefix."
    (match (get-resolver provider name)
      ((Err e) (Err e))
      ((Ok opt-resolver)
       (match opt-resolver
         ((None) (Ok None))
         ((Some resolver-addr)
          (let ((calldata (%contenthash-calldata (ens:namehash name))))
            (match (provider:eth-call provider None resolver-addr calldata)
              ((Err e) (Err e))
              ((Ok result)
               (if (%is-empty-bytes result)
                   (Ok None)
                   (Ok (Some result))))))))))))
