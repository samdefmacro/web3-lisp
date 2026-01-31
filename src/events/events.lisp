(in-package #:web3/events)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Event Log Type
  ;;; =========================================================================

  (define-struct EventLog
    "Represents an Ethereum event log"
    (address       addr:Address)      ; Contract that emitted the event
    (topics        (List types:Bytes)) ; Event topics (first is signature hash)
    (data          types:Bytes)        ; Non-indexed parameters (ABI encoded)
    (block-number  (Optional types:U256))
    (tx-hash       (Optional types:Bytes))
    (log-index     (Optional UFix)))

  ;;; =========================================================================
  ;;; Event Signature Utilities
  ;;; =========================================================================

  (declare event-signature (String -> types:Bytes))
  (define (event-signature sig)
    "Compute the keccak256 hash of an event signature.
     Example: (event-signature \"Transfer(address,address,uint256)\")"
    (let ((sig-bytes (lisp types:Bytes (sig)
                       (cl:let* ((str (cl:the cl:string sig))
                                 (len (cl:length str))
                                 (arr (cl:make-array len :fill-pointer len :adjustable cl:t)))
                         (cl:dotimes (i len arr)
                           (cl:setf (cl:aref arr i) (cl:char-code (cl:char str i))))))))
      (crypto:keccak256 sig-bytes)))

  (declare event-topic (String -> types:Bytes))
  (define (event-topic sig)
    "Alias for event-signature - get topic0 for an event"
    (event-signature sig))

  (declare matches-event-signature? (types:Bytes -> EventLog -> Boolean))
  (define (matches-event-signature? expected-topic log)
    "Check if the event log matches an expected event signature"
    (match (.topics log)
      ((Cons topic0 _)
       (types:bytes-equal? topic0 expected-topic))
      ((Nil) False)))

  ;;; =========================================================================
  ;;; Common Event Signatures (Topic 0)
  ;;; =========================================================================

  ;; ERC-20 Events
  (declare erc20-transfer-topic (Unit -> types:Bytes))
  (define (erc20-transfer-topic)
    "Topic0 for Transfer(address indexed from, address indexed to, uint256 value)"
    (event-signature "Transfer(address,address,uint256)"))

  (declare erc20-approval-topic (Unit -> types:Bytes))
  (define (erc20-approval-topic)
    "Topic0 for Approval(address indexed owner, address indexed spender, uint256 value)"
    (event-signature "Approval(address,address,uint256)"))

  ;; ERC-721 Events (same signatures as ERC-20 for Transfer/Approval)
  (declare erc721-transfer-topic (Unit -> types:Bytes))
  (define (erc721-transfer-topic)
    "Topic0 for Transfer(address indexed from, address indexed to, uint256 indexed tokenId)"
    (event-signature "Transfer(address,address,uint256)"))

  (declare erc721-approval-topic (Unit -> types:Bytes))
  (define (erc721-approval-topic)
    "Topic0 for Approval(address indexed owner, address indexed approved, uint256 indexed tokenId)"
    (event-signature "Approval(address,address,uint256)"))

  (declare erc721-approval-for-all-topic (Unit -> types:Bytes))
  (define (erc721-approval-for-all-topic)
    "Topic0 for ApprovalForAll(address indexed owner, address indexed operator, bool approved)"
    (event-signature "ApprovalForAll(address,address,bool)"))

  ;; ERC-1155 Events
  (declare erc1155-transfer-single-topic (Unit -> types:Bytes))
  (define (erc1155-transfer-single-topic)
    "Topic0 for TransferSingle(address indexed operator, address indexed from, address indexed to, uint256 id, uint256 value)"
    (event-signature "TransferSingle(address,address,address,uint256,uint256)"))

  (declare erc1155-transfer-batch-topic (Unit -> types:Bytes))
  (define (erc1155-transfer-batch-topic)
    "Topic0 for TransferBatch(address indexed operator, address indexed from, address indexed to, uint256[] ids, uint256[] values)"
    (event-signature "TransferBatch(address,address,address,uint256[],uint256[])"))

  (declare erc1155-approval-for-all-topic (Unit -> types:Bytes))
  (define (erc1155-approval-for-all-topic)
    "Topic0 for ApprovalForAll(address indexed account, address indexed operator, bool approved)"
    (event-signature "ApprovalForAll(address,address,bool)"))

  ;;; =========================================================================
  ;;; Parameter Decoding
  ;;; =========================================================================

  (declare decode-indexed-address (types:Bytes -> (types:Web3Result addr:Address)))
  (define (decode-indexed-address topic)
    "Decode an indexed address parameter from a 32-byte topic.
     Addresses are right-padded to 32 bytes, so we take the last 20 bytes."
    (if (< (types:bytes-length topic) 32)
        (Err (types:AbiError "Topic too short for address"))
        (let ((addr-bytes (types:bytes-drop 12 topic)))
          (addr:address-from-bytes (types:bytes-take 20 addr-bytes)))))

  (declare decode-indexed-uint256 (types:Bytes -> (types:Web3Result types:U256)))
  (define (decode-indexed-uint256 topic)
    "Decode an indexed uint256 parameter from a 32-byte topic"
    (if (< (types:bytes-length topic) 32)
        (Err (types:AbiError "Topic too short for uint256"))
        (types:u256-from-bytes topic)))

  (declare decode-non-indexed-params ((List abi:AbiType) -> types:Bytes -> (types:Web3Result (List abi:AbiValue))))
  (define (decode-non-indexed-params types data)
    "Decode non-indexed parameters from the data field"
    (abi:abi-decode types data))

  ;;; =========================================================================
  ;;; Decoded Event Types
  ;;; =========================================================================

  (define-type DecodedEvent
    "Represents a decoded event with typed fields"
    (Erc20Transfer addr:Address addr:Address types:U256)         ; from, to, value
    (Erc20Approval addr:Address addr:Address types:U256)         ; owner, spender, value
    (Erc721Transfer addr:Address addr:Address types:U256)        ; from, to, tokenId
    (Erc721Approval addr:Address addr:Address types:U256)        ; owner, approved, tokenId
    (Erc1155TransferSingle addr:Address addr:Address addr:Address types:U256 types:U256) ; operator, from, to, id, value
    (UnknownEvent types:Bytes))  ; topic0 for unknown events

  ;;; =========================================================================
  ;;; High-Level Event Decoders
  ;;; =========================================================================

  (declare decode-erc20-transfer (EventLog -> (types:Web3Result DecodedEvent)))
  (define (decode-erc20-transfer log)
    "Decode an ERC-20 Transfer event.
     Transfer(address indexed from, address indexed to, uint256 value)
     - topics[0] = signature hash
     - topics[1] = from (indexed)
     - topics[2] = to (indexed)
     - data = value (non-indexed)"
    (match (.topics log)
      ((Cons _ (Cons from-topic (Cons to-topic (Nil))))
       (match (decode-indexed-address from-topic)
         ((Err e) (Err e))
         ((Ok from-addr)
          (match (decode-indexed-address to-topic)
            ((Err e) (Err e))
            ((Ok to-addr)
             (match (abi:abi-decode (Cons (abi:AbiUint 256) Nil) (.data log))
               ((Err e) (Err e))
               ((Ok decoded)
                (match decoded
                  ((Cons (abi:AbiUintVal value) (Nil))
                   (Ok (Erc20Transfer from-addr to-addr value)))
                  (_ (Err (types:AbiError "Invalid Transfer data")))))))))))
      (_ (Err (types:AbiError "Invalid Transfer topics")))))

  (declare decode-erc20-approval (EventLog -> (types:Web3Result DecodedEvent)))
  (define (decode-erc20-approval log)
    "Decode an ERC-20 Approval event.
     Approval(address indexed owner, address indexed spender, uint256 value)
     - topics[0] = signature hash
     - topics[1] = owner (indexed)
     - topics[2] = spender (indexed)
     - data = value (non-indexed)"
    (match (.topics log)
      ((Cons _ (Cons owner-topic (Cons spender-topic (Nil))))
       (match (decode-indexed-address owner-topic)
         ((Err e) (Err e))
         ((Ok owner-addr)
          (match (decode-indexed-address spender-topic)
            ((Err e) (Err e))
            ((Ok spender-addr)
             (match (abi:abi-decode (Cons (abi:AbiUint 256) Nil) (.data log))
               ((Err e) (Err e))
               ((Ok decoded)
                (match decoded
                  ((Cons (abi:AbiUintVal value) (Nil))
                   (Ok (Erc20Approval owner-addr spender-addr value)))
                  (_ (Err (types:AbiError "Invalid Approval data")))))))))))
      (_ (Err (types:AbiError "Invalid Approval topics")))))

  (declare decode-erc721-transfer (EventLog -> (types:Web3Result DecodedEvent)))
  (define (decode-erc721-transfer log)
    "Decode an ERC-721 Transfer event.
     Transfer(address indexed from, address indexed to, uint256 indexed tokenId)
     - topics[0] = signature hash
     - topics[1] = from (indexed)
     - topics[2] = to (indexed)
     - topics[3] = tokenId (indexed)
     - data = empty"
    (match (.topics log)
      ((Cons _ (Cons from-topic (Cons to-topic (Cons token-id-topic (Nil)))))
       (match (decode-indexed-address from-topic)
         ((Err e) (Err e))
         ((Ok from-addr)
          (match (decode-indexed-address to-topic)
            ((Err e) (Err e))
            ((Ok to-addr)
             (match (decode-indexed-uint256 token-id-topic)
               ((Err e) (Err e))
               ((Ok token-id)
                (Ok (Erc721Transfer from-addr to-addr token-id)))))))))
      (_ (Err (types:AbiError "Invalid ERC-721 Transfer topics")))))

  (declare decode-erc1155-transfer-single (EventLog -> (types:Web3Result DecodedEvent)))
  (define (decode-erc1155-transfer-single log)
    "Decode an ERC-1155 TransferSingle event.
     TransferSingle(address indexed operator, address indexed from, address indexed to, uint256 id, uint256 value)
     - topics[0] = signature hash
     - topics[1] = operator (indexed)
     - topics[2] = from (indexed)
     - topics[3] = to (indexed)
     - data = id, value (non-indexed)"
    (match (.topics log)
      ((Cons _ (Cons operator-topic (Cons from-topic (Cons to-topic (Nil)))))
       (match (decode-indexed-address operator-topic)
         ((Err e) (Err e))
         ((Ok operator-addr)
          (match (decode-indexed-address from-topic)
            ((Err e) (Err e))
            ((Ok from-addr)
             (match (decode-indexed-address to-topic)
               ((Err e) (Err e))
               ((Ok to-addr)
                (match (abi:abi-decode (Cons (abi:AbiUint 256)
                                             (Cons (abi:AbiUint 256) Nil))
                                       (.data log))
                  ((Err e) (Err e))
                  ((Ok decoded)
                   (match decoded
                     ((Cons (abi:AbiUintVal token-id)
                            (Cons (abi:AbiUintVal value) (Nil)))
                      (Ok (Erc1155TransferSingle operator-addr from-addr to-addr token-id value)))
                     (_ (Err (types:AbiError "Invalid TransferSingle data")))))))))))))
      (_ (Err (types:AbiError "Invalid TransferSingle topics")))))

  (declare decode-event (EventLog -> DecodedEvent))
  (define (decode-event log)
    "Attempt to decode an event log into a known event type.
     Returns UnknownEvent if the event signature is not recognized."
    (match (.topics log)
      ((Cons topic0 _)
       (cond
         ((types:bytes-equal? topic0 (erc20-transfer-topic))
          ;; Could be ERC-20 or ERC-721 Transfer - check topic count
          (match (.topics log)
            ((Cons _ (Cons _ (Cons _ (Cons _ (Nil)))))
             ;; 4 topics = ERC-721 (tokenId is indexed)
             (match (decode-erc721-transfer log)
               ((Ok decoded) decoded)
               ((Err _) (UnknownEvent topic0))))
            (_
             ;; 3 topics = ERC-20 (value is in data)
             (match (decode-erc20-transfer log)
               ((Ok decoded) decoded)
               ((Err _) (UnknownEvent topic0))))))
         ((types:bytes-equal? topic0 (erc20-approval-topic))
          (match (decode-erc20-approval log)
            ((Ok decoded) decoded)
            ((Err _) (UnknownEvent topic0))))
         ((types:bytes-equal? topic0 (erc1155-transfer-single-topic))
          (match (decode-erc1155-transfer-single log)
            ((Ok decoded) decoded)
            ((Err _) (UnknownEvent topic0))))
         (True (UnknownEvent topic0))))
      ((Nil) (UnknownEvent (types:bytes-empty))))))
