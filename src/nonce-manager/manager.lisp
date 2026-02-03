;;;; Nonce Manager implementation
;;;; Core nonce management operations

(in-package #:web3/nonce-manager)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Constructor
  ;;; =========================================================================

  (declare make-nonce-manager (provider:HttpProvider -> NonceManager))
  (define (make-nonce-manager prov)
    "Create a new nonce manager with the given provider"
    (NonceManager prov
                  (cell:new Nil)
                  (cell:new Nil)))

  (declare nonce-manager-provider (NonceManager -> provider:HttpProvider))
  (define (nonce-manager-provider nm)
    "Get the provider from a nonce manager"
    (.nm-provider nm))

  ;;; =========================================================================
  ;;; Internal Helpers
  ;;; =========================================================================

  (declare %lookup-in-list (NonceKey -> (List (Tuple NonceKey U64)) -> (Optional U64)))
  (define (%lookup-in-list key entries)
    "Look up a value by key in an association list"
    (match entries
      ((Nil) None)
      ((Cons (Tuple k v) rest)
       (if (nonce-key-equal? k key)
           (Some v)
           (%lookup-in-list key rest)))))

  (declare %update-in-list (NonceKey -> U64 -> (List (Tuple NonceKey U64)) -> (List (Tuple NonceKey U64))))
  (define (%update-in-list key new-value entries)
    "Update or insert a value in an association list"
    (lisp (List (Tuple NonceKey U64)) (key new-value entries)
      (cl:let ((found cl:nil)
               (result cl:nil))
        (cl:dolist (entry entries)
          (cl:let ((k (cl:slot-value entry 'coalton-library/classes::_0))
                   (v (cl:slot-value entry 'coalton-library/classes::_1)))
            (cl:if (coalton (nonce-key-equal? (lisp NonceKey () key)
                                               (lisp NonceKey () k)))
                   (cl:progn
                     (cl:setf found cl:t)
                     (cl:push (coalton-prelude:Tuple key new-value) result))
                   (cl:push entry result))))
        (cl:unless found
          (cl:push (coalton-prelude:Tuple key new-value) result))
        (cl:nreverse result))))

  (declare %remove-from-list (NonceKey -> (List (Tuple NonceKey U64)) -> (List (Tuple NonceKey U64))))
  (define (%remove-from-list key entries)
    "Remove an entry from an association list"
    (lisp (List (Tuple NonceKey U64)) (key entries)
      (cl:let ((result cl:nil))
        (cl:dolist (entry entries)
          (cl:let ((k (cl:slot-value entry 'coalton-library/classes::_0)))
            (cl:unless (coalton (nonce-key-equal? (lisp NonceKey () key)
                                                   (lisp NonceKey () k)))
              (cl:push entry result))))
        (cl:nreverse result))))

  ;;; =========================================================================
  ;;; Core Operations
  ;;; =========================================================================

  (declare nonce-sync (addr:Address -> UFix -> NonceManager -> (types:Web3Result Unit)))
  (define (nonce-sync address chain-id nm)
    "Sync the cached nonce for an address from the network.
     This fetches the confirmed transaction count and resets local deltas."
    (let ((key (make-nonce-key address chain-id)))
      (match (provider:eth-get-transaction-count (.nm-provider nm) address)
        ((Err e) (Err e))
        ((Ok nonce)
         ;; Update cache with confirmed nonce
         (cell:write! (.nm-cache nm)
                      (%update-in-list key nonce (cell:read (.nm-cache nm))))
         ;; Reset delta for this address/chain
         (cell:write! (.nm-deltas nm)
                      (%remove-from-list key (cell:read (.nm-deltas nm))))
         (Ok Unit)))))

  (declare nonce-get (addr:Address -> UFix -> NonceManager -> (types:Web3Result U64)))
  (define (nonce-get address chain-id nm)
    "Get the next nonce for an address on a chain.
     Returns cached value + deltas, fetching from network if not cached."
    (let ((key (make-nonce-key address chain-id)))
      ;; Check if we have a cached value
      (match (%lookup-in-list key (cell:read (.nm-cache nm)))
        ((Some base-nonce)
         ;; Get delta (pending transaction count)
         (let ((delta (match (%lookup-in-list key (cell:read (.nm-deltas nm)))
                        ((Some d) d)
                        ((None) 0))))
           (Ok (+ base-nonce delta))))
        ((None)
         ;; Need to fetch from network
         (match (nonce-sync address chain-id nm)
           ((Err e) (Err e))
           ((Ok _)
            ;; Now get from cache
            (match (%lookup-in-list key (cell:read (.nm-cache nm)))
              ((Some nonce) (Ok nonce))
              ((None) (Err (types:ProviderError "Failed to cache nonce"))))))))))

  (declare nonce-peek (addr:Address -> UFix -> NonceManager -> (Optional U64)))
  (define (nonce-peek address chain-id nm)
    "Peek at the cached nonce without fetching from network.
     Returns None if not cached."
    (let ((key (make-nonce-key address chain-id)))
      (match (%lookup-in-list key (cell:read (.nm-cache nm)))
        ((Some base-nonce)
         (let ((delta (match (%lookup-in-list key (cell:read (.nm-deltas nm)))
                        ((Some d) d)
                        ((None) 0))))
           (Some (+ base-nonce delta))))
        ((None) None))))

  (declare nonce-consume (addr:Address -> UFix -> NonceManager -> (types:Web3Result U64)))
  (define (nonce-consume address chain-id nm)
    "Get the next nonce and increment the local delta.
     Use this when sending a transaction to reserve a nonce."
    (let ((key (make-nonce-key address chain-id)))
      ;; First get the current nonce
      (match (nonce-get address chain-id nm)
        ((Err e) (Err e))
        ((Ok nonce)
         ;; Increment delta
         (let ((current-delta (match (%lookup-in-list key (cell:read (.nm-deltas nm)))
                                ((Some d) d)
                                ((None) 0))))
           (cell:write! (.nm-deltas nm)
                        (%update-in-list key (+ current-delta 1) (cell:read (.nm-deltas nm)))))
         (Ok nonce)))))

  (declare nonce-reset (addr:Address -> UFix -> NonceManager -> Unit))
  (define (nonce-reset address chain-id nm)
    "Reset cached nonce and deltas for an address.
     Use this after a transaction is confirmed or on errors."
    (let ((key (make-nonce-key address chain-id)))
      (cell:write! (.nm-cache nm)
                   (%remove-from-list key (cell:read (.nm-cache nm))))
      (cell:write! (.nm-deltas nm)
                   (%remove-from-list key (cell:read (.nm-deltas nm))))
      Unit)))


;;; =========================================================================
;;; Exports
;;; =========================================================================

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:export '(NonceManager
               NonceKey
               make-nonce-manager
               nonce-get
               nonce-consume
               nonce-reset
               nonce-sync
               nonce-peek
               nonce-manager-provider)
             (cl:find-package '#:web3/nonce-manager)))
