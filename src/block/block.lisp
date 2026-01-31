;;;; Block Parsing implementation
;;;; Parse Ethereum blocks and block headers from JSON-RPC

(in-package #:web3/block)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Block Tag Type
  ;;; =========================================================================

  (define-type BlockTag
    TagLatest
    TagPending
    TagFinalized
    TagSafe
    TagEarliest
    (TagNumber UFix))

  (declare block-tag-to-string (BlockTag -> String))
  (define (block-tag-to-string tag)
    "Convert block tag to JSON-RPC string"
    (match tag
      ((TagLatest) "latest")
      ((TagPending) "pending")
      ((TagFinalized) "finalized")
      ((TagSafe) "safe")
      ((TagEarliest) "earliest")
      ((TagNumber n)
       (lisp String (n)
         (cl:format cl:nil "0x~X" n)))))

  ;;; =========================================================================
  ;;; Withdrawal Type (EIP-4895)
  ;;; =========================================================================

  (define-type Withdrawal
    (Withdrawal UFix      ; index
                UFix      ; validator index
                addr:Address  ; address
                UFix))    ; amount in Gwei

  (declare make-withdrawal (UFix -> UFix -> addr:Address -> UFix -> Withdrawal))
  (define (make-withdrawal index validator-index address amount)
    "Create a withdrawal"
    (Withdrawal index validator-index address amount))

  (declare withdrawal-index (Withdrawal -> UFix))
  (define (withdrawal-index w)
    (match w ((Withdrawal i _ _ _) i)))

  (declare withdrawal-validator-index (Withdrawal -> UFix))
  (define (withdrawal-validator-index w)
    (match w ((Withdrawal _ vi _ _) vi)))

  (declare withdrawal-address (Withdrawal -> addr:Address))
  (define (withdrawal-address w)
    (match w ((Withdrawal _ _ a _) a)))

  (declare withdrawal-amount (Withdrawal -> UFix))
  (define (withdrawal-amount w)
    (match w ((Withdrawal _ _ _ amt) amt)))

  ;;; =========================================================================
  ;;; Block Header Type
  ;;; =========================================================================

  ;; Using a record-style approach with a lisp struct for the many fields
  (define-type BlockHeader
    (%BlockHeader
     UFix              ; number
     types:Bytes       ; hash
     types:Bytes       ; parent hash
     types:Bytes       ; nonce (8 bytes)
     types:Bytes       ; sha3Uncles
     types:Bytes       ; logsBloom
     types:Bytes       ; transactionsRoot
     types:Bytes       ; stateRoot
     types:Bytes       ; receiptsRoot
     addr:Address      ; miner
     types:U256        ; difficulty
     (Optional types:U256)  ; totalDifficulty (may be null)
     types:Bytes       ; extraData
     UFix              ; size
     UFix              ; gasLimit
     UFix              ; gasUsed
     UFix              ; timestamp
     (Optional UFix)   ; baseFeePerGas (EIP-1559)
     (Optional types:Bytes)  ; withdrawalsRoot (Shanghai)
     (Optional UFix)   ; blobGasUsed (Cancun)
     (Optional UFix))) ; excessBlobGas (Cancun)

  (declare make-block-header
           (UFix -> types:Bytes -> types:Bytes -> types:Bytes -> types:Bytes ->
            types:Bytes -> types:Bytes -> types:Bytes -> types:Bytes ->
            addr:Address -> types:U256 -> (Optional types:U256) ->
            types:Bytes -> UFix -> UFix -> UFix -> UFix ->
            (Optional UFix) -> (Optional types:Bytes) ->
            (Optional UFix) -> (Optional UFix) -> BlockHeader))
  (define (make-block-header number hash parent-hash nonce sha3-uncles
                             logs-bloom tx-root state-root receipts-root
                             miner difficulty total-difficulty
                             extra-data size gas-limit gas-used timestamp
                             base-fee withdrawals-root blob-gas-used excess-blob-gas)
    (%BlockHeader number hash parent-hash nonce sha3-uncles
                  logs-bloom tx-root state-root receipts-root
                  miner difficulty total-difficulty
                  extra-data size gas-limit gas-used timestamp
                  base-fee withdrawals-root blob-gas-used excess-blob-gas))

  ;; Accessors
  (declare header-number (BlockHeader -> UFix))
  (define (header-number h)
    (match h ((%BlockHeader n _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) n)))

  (declare header-hash (BlockHeader -> types:Bytes))
  (define (header-hash h)
    (match h ((%BlockHeader _ hash _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) hash)))

  (declare header-parent-hash (BlockHeader -> types:Bytes))
  (define (header-parent-hash h)
    (match h ((%BlockHeader _ _ ph _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) ph)))

  (declare header-nonce (BlockHeader -> types:Bytes))
  (define (header-nonce h)
    (match h ((%BlockHeader _ _ _ n _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) n)))

  (declare header-sha3-uncles (BlockHeader -> types:Bytes))
  (define (header-sha3-uncles h)
    (match h ((%BlockHeader _ _ _ _ u _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) u)))

  (declare header-logs-bloom (BlockHeader -> types:Bytes))
  (define (header-logs-bloom h)
    (match h ((%BlockHeader _ _ _ _ _ lb _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) lb)))

  (declare header-transactions-root (BlockHeader -> types:Bytes))
  (define (header-transactions-root h)
    (match h ((%BlockHeader _ _ _ _ _ _ tr _ _ _ _ _ _ _ _ _ _ _ _ _ _) tr)))

  (declare header-state-root (BlockHeader -> types:Bytes))
  (define (header-state-root h)
    (match h ((%BlockHeader _ _ _ _ _ _ _ sr _ _ _ _ _ _ _ _ _ _ _ _ _) sr)))

  (declare header-receipts-root (BlockHeader -> types:Bytes))
  (define (header-receipts-root h)
    (match h ((%BlockHeader _ _ _ _ _ _ _ _ rr _ _ _ _ _ _ _ _ _ _ _ _) rr)))

  (declare header-miner (BlockHeader -> addr:Address))
  (define (header-miner h)
    (match h ((%BlockHeader _ _ _ _ _ _ _ _ _ m _ _ _ _ _ _ _ _ _ _ _) m)))

  (declare header-difficulty (BlockHeader -> types:U256))
  (define (header-difficulty h)
    (match h ((%BlockHeader _ _ _ _ _ _ _ _ _ _ d _ _ _ _ _ _ _ _ _ _) d)))

  (declare header-total-difficulty (BlockHeader -> (Optional types:U256)))
  (define (header-total-difficulty h)
    (match h ((%BlockHeader _ _ _ _ _ _ _ _ _ _ _ td _ _ _ _ _ _ _ _ _) td)))

  (declare header-extra-data (BlockHeader -> types:Bytes))
  (define (header-extra-data h)
    (match h ((%BlockHeader _ _ _ _ _ _ _ _ _ _ _ _ ed _ _ _ _ _ _ _ _) ed)))

  (declare header-size (BlockHeader -> UFix))
  (define (header-size h)
    (match h ((%BlockHeader _ _ _ _ _ _ _ _ _ _ _ _ _ sz _ _ _ _ _ _ _) sz)))

  (declare header-gas-limit (BlockHeader -> UFix))
  (define (header-gas-limit h)
    (match h ((%BlockHeader _ _ _ _ _ _ _ _ _ _ _ _ _ _ gl _ _ _ _ _ _) gl)))

  (declare header-gas-used (BlockHeader -> UFix))
  (define (header-gas-used h)
    (match h ((%BlockHeader _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ gu _ _ _ _ _) gu)))

  (declare header-timestamp (BlockHeader -> UFix))
  (define (header-timestamp h)
    (match h ((%BlockHeader _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ ts _ _ _ _) ts)))

  (declare header-base-fee (BlockHeader -> (Optional UFix)))
  (define (header-base-fee h)
    (match h ((%BlockHeader _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ bf _ _ _) bf)))

  (declare header-withdrawals-root (BlockHeader -> (Optional types:Bytes)))
  (define (header-withdrawals-root h)
    (match h ((%BlockHeader _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ wr _ _) wr)))

  (declare header-blob-gas-used (BlockHeader -> (Optional UFix)))
  (define (header-blob-gas-used h)
    (match h ((%BlockHeader _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ bgu _) bgu)))

  (declare header-excess-blob-gas (BlockHeader -> (Optional UFix)))
  (define (header-excess-blob-gas h)
    (match h ((%BlockHeader _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ ebg) ebg)))

  ;;; =========================================================================
  ;;; Block Transaction Type
  ;;; =========================================================================

  (define-type BlockTx
    (TxHash types:Bytes)      ; Transaction hash only
    (TxFull types:Bytes))     ; Full transaction object (stored as raw JSON bytes for now)

  ;;; =========================================================================
  ;;; Full Block Type
  ;;; =========================================================================

  (define-type Block
    (%Block BlockHeader
            (List BlockTx)
            (List types:Bytes)      ; uncle hashes
            (List Withdrawal)))     ; withdrawals (Shanghai+)

  (declare make-block (BlockHeader -> (List BlockTx) -> (List types:Bytes) -> (List Withdrawal) -> Block))
  (define (make-block header txs uncles withdrawals)
    (%Block header txs uncles withdrawals))

  (declare block-header (Block -> BlockHeader))
  (define (block-header b)
    (match b ((%Block h _ _ _) h)))

  (declare block-transactions (Block -> (List BlockTx)))
  (define (block-transactions b)
    (match b ((%Block _ txs _ _) txs)))

  (declare block-uncles (Block -> (List types:Bytes)))
  (define (block-uncles b)
    (match b ((%Block _ _ uncles _) uncles)))

  (declare block-withdrawals (Block -> (List Withdrawal)))
  (define (block-withdrawals b)
    (match b ((%Block _ _ _ ws) ws)))

  ;;; =========================================================================
  ;;; Utility Functions
  ;;; =========================================================================

  (declare is-post-merge (BlockHeader -> Boolean))
  (define (is-post-merge header)
    "Check if block is post-merge (difficulty = 0)"
    (types:u256-zero? (header-difficulty header)))

  (declare is-post-shanghai (BlockHeader -> Boolean))
  (define (is-post-shanghai header)
    "Check if block is post-Shanghai (has withdrawals root)"
    (match (header-withdrawals-root header)
      ((Some _) True)
      ((None) False)))

  (declare is-post-cancun (BlockHeader -> Boolean))
  (define (is-post-cancun header)
    "Check if block is post-Cancun (has blob gas fields)"
    (match (header-blob-gas-used header)
      ((Some _) True)
      ((None) False)))

  (declare block-age (BlockHeader -> UFix -> UFix))
  (define (block-age header current-timestamp)
    "Calculate block age in seconds"
    (- current-timestamp (header-timestamp header)))

  (declare gas-utilization (BlockHeader -> UFix))
  (define (gas-utilization header)
    "Calculate gas utilization as percentage (0-100)"
    (let ((limit (header-gas-limit header))
          (used (header-gas-used header)))
      (if (== limit 0)
          0
          (lisp UFix (used limit)
            (cl:floor (cl:* 100 used) limit))))))


;;; =========================================================================
;;; Parsing Functions (Common Lisp)
;;; =========================================================================

(cl:defun parse-hex-to-ufix (hex-str)
  "Parse hex string to UFix"
  (cl:if (cl:or (cl:null hex-str) (cl:string= hex-str ""))
         0
         (cl:let ((clean (cl:if (cl:and (cl:>= (cl:length hex-str) 2)
                                        (cl:string= (cl:subseq hex-str 0 2) "0x"))
                                (cl:subseq hex-str 2)
                                hex-str)))
           (cl:if (cl:string= clean "")
                  0
                  (cl:parse-integer clean :radix 16 :junk-allowed cl:t)))))

(cl:defun parse-hex-to-bytes (hex-str)
  "Parse hex string to Bytes"
  (cl:if (cl:or (cl:null hex-str) (cl:string= hex-str ""))
         (cl:make-array 0 :element-type 'cl:t :fill-pointer 0 :adjustable cl:t)
         (cl:let* ((clean (cl:if (cl:and (cl:>= (cl:length hex-str) 2)
                                         (cl:string= (cl:subseq hex-str 0 2) "0x"))
                                 (cl:subseq hex-str 2)
                                 hex-str))
                   (len (cl:floor (cl:length clean) 2))
                   (result (cl:make-array len :element-type 'cl:t
                                          :fill-pointer len :adjustable cl:t)))
           (cl:dotimes (i len result)
             (cl:setf (cl:aref result i)
                      (cl:parse-integer clean :start (cl:* i 2) :end (cl:+ (cl:* i 2) 2)
                                        :radix 16))))))

(cl:defun parse-hex-to-u256 (hex-str)
  "Parse hex string to U256"
  (cl:if (cl:or (cl:null hex-str) (cl:string= hex-str ""))
         (web3/types::u256-from-integer 0)
         (cl:let ((clean (cl:if (cl:and (cl:>= (cl:length hex-str) 2)
                                        (cl:string= (cl:subseq hex-str 0 2) "0x"))
                                (cl:subseq hex-str 2)
                                hex-str)))
           (cl:if (cl:string= clean "")
                  (web3/types::u256-from-integer 0)
                  (web3/types::u256-from-integer
                   (cl:parse-integer clean :radix 16 :junk-allowed cl:t))))))

(cl:defun make-coalton-some (value)
  "Create a Coalton Some value"
  (cl:make-instance 'coalton-library/classes::optional/some
                    'coalton-library/classes::|_0| value))

(cl:defun make-coalton-none ()
  "Create a Coalton None value"
  (cl:make-instance 'coalton-library/classes::optional/none))

(cl:defun camel-to-lisp-case (str)
  "Convert camelCase to LISP-CASE"
  (cl:with-output-to-string (out)
    (cl:dotimes (i (cl:length str))
      (cl:let ((c (cl:char str i)))
        (cl:cond
          ((cl:and (cl:upper-case-p c) (cl:> i 0))
           (cl:write-char #\- out)
           (cl:write-char (cl:char-upcase c) out))
          (cl:t
           (cl:write-char (cl:char-upcase c) out)))))))

(cl:defun json-get (obj key)
  "Get value from JSON alist (cl-json uses LISP-CASE keywords)"
  (cl:let ((keyword (cl:intern (camel-to-lisp-case key) :keyword)))
    (cl:cdr (cl:assoc keyword obj))))

(cl:defun parse-optional-ufix (hex-str)
  "Parse optional hex string to Optional UFix"
  (cl:if (cl:or (cl:null hex-str) (cl:string= hex-str ""))
         (make-coalton-none)
         (make-coalton-some (parse-hex-to-ufix hex-str))))

(cl:defun parse-optional-u256 (hex-str)
  "Parse optional hex string to Optional U256"
  (cl:if (cl:or (cl:null hex-str) (cl:string= hex-str ""))
         (make-coalton-none)
         (make-coalton-some (parse-hex-to-u256 hex-str))))

(cl:defun parse-optional-bytes (hex-str)
  "Parse optional hex string to Optional Bytes"
  (cl:if (cl:or (cl:null hex-str) (cl:string= hex-str ""))
         (make-coalton-none)
         (make-coalton-some (parse-hex-to-bytes hex-str))))

(cl:defun parse-address-from-hex (hex-str)
  "Parse address from hex string"
  (cl:let ((result (coalton:coalton
                    (web3/address:address-from-hex
                     (coalton:lisp coalton:String () hex-str)))))
    (cl:if (cl:typep result 'coalton-library/classes::result/ok)
           (cl:slot-value result 'coalton-library/classes::|_0|)
           ;; Return zero address on error
           (coalton:coalton (web3/address:address-zero coalton:Unit)))))


(coalton-toplevel

  ;;; =========================================================================
  ;;; Withdrawal Parsing
  ;;; =========================================================================

  (declare parse-withdrawal (types:Bytes -> (types:Web3Result Withdrawal)))
  (define (parse-withdrawal json-bytes)
    "Parse a withdrawal from JSON bytes"
    (lisp (types:Web3Result Withdrawal) (json-bytes)
      (cl:handler-case
          (cl:let* ((json-str (cl:map 'cl:string #'cl:code-char json-bytes))
                    (obj (cl:ignore-errors (json:decode-json-from-string json-str))))
            (cl:if (cl:null obj)
                   (cl:make-instance 'coalton-library/classes::result/err
                                     'coalton-library/classes::|_0|
                                     (types:ProviderError "Invalid JSON"))
                   (cl:let ((w (Withdrawal
                                (parse-hex-to-ufix (json-get obj "index"))
                                (parse-hex-to-ufix (json-get obj "validatorIndex"))
                                (parse-address-from-hex (json-get obj "address"))
                                (parse-hex-to-ufix (json-get obj "amount")))))
                     (cl:make-instance 'coalton-library/classes::result/ok
                                       'coalton-library/classes::|_0| w))))
        (cl:error (e)
          (cl:declare (cl:ignore e))
          (cl:make-instance 'coalton-library/classes::result/err
                            'coalton-library/classes::|_0|
                            (types:ProviderError "Parse error"))))))

  ;;; =========================================================================
  ;;; Block Header Parsing
  ;;; =========================================================================

  (declare parse-block-header (types:Bytes -> (types:Web3Result BlockHeader)))
  (define (parse-block-header json-bytes)
    "Parse a block header from JSON bytes"
    (lisp (types:Web3Result BlockHeader) (json-bytes)
      (cl:handler-case
          (cl:let* ((json-str (cl:map 'cl:string #'cl:code-char json-bytes))
                    (obj (cl:ignore-errors (json:decode-json-from-string json-str))))
            (cl:if (cl:null obj)
                   (cl:make-instance 'coalton-library/classes::result/err
                                     'coalton-library/classes::|_0|
                                     (types:ProviderError "Invalid JSON"))
                   (cl:let ((header
                             (%BlockHeader
                              (parse-hex-to-ufix (json-get obj "number"))
                              (parse-hex-to-bytes (json-get obj "hash"))
                              (parse-hex-to-bytes (json-get obj "parentHash"))
                              (parse-hex-to-bytes (json-get obj "nonce"))
                              (parse-hex-to-bytes (json-get obj "sha3Uncles"))
                              (parse-hex-to-bytes (json-get obj "logsBloom"))
                              (parse-hex-to-bytes (json-get obj "transactionsRoot"))
                              (parse-hex-to-bytes (json-get obj "stateRoot"))
                              (parse-hex-to-bytes (json-get obj "receiptsRoot"))
                              (parse-address-from-hex (json-get obj "miner"))
                              (parse-hex-to-u256 (json-get obj "difficulty"))
                              (parse-optional-u256 (json-get obj "totalDifficulty"))
                              (parse-hex-to-bytes (json-get obj "extraData"))
                              (parse-hex-to-ufix (json-get obj "size"))
                              (parse-hex-to-ufix (json-get obj "gasLimit"))
                              (parse-hex-to-ufix (json-get obj "gasUsed"))
                              (parse-hex-to-ufix (json-get obj "timestamp"))
                              (parse-optional-ufix (json-get obj "baseFeePerGas"))
                              (parse-optional-bytes (json-get obj "withdrawalsRoot"))
                              (parse-optional-ufix (json-get obj "blobGasUsed"))
                              (parse-optional-ufix (json-get obj "excessBlobGas")))))
                     (cl:make-instance 'coalton-library/classes::result/ok
                                       'coalton-library/classes::|_0| header))))
        (cl:error (e)
          (cl:declare (cl:ignore e))
          (cl:make-instance 'coalton-library/classes::result/err
                            'coalton-library/classes::|_0|
                            (types:ProviderError "Parse error"))))))

  ;;; =========================================================================
  ;;; Full Block Parsing
  ;;; =========================================================================

  (declare parse-block (types:Bytes -> Boolean -> (types:Web3Result Block)))
  (define (parse-block json-bytes full-txs)
    "Parse a full block from JSON bytes. full-txs indicates if transactions are full objects."
    (lisp (types:Web3Result Block) (json-bytes full-txs)
      (cl:handler-case
          (cl:let* ((json-str (cl:map 'cl:string #'cl:code-char json-bytes))
                    (obj (cl:ignore-errors (json:decode-json-from-string json-str))))
            (cl:if (cl:null obj)
                   (cl:make-instance 'coalton-library/classes::result/err
                                     'coalton-library/classes::|_0|
                                     (types:ProviderError "Invalid JSON"))
                   (cl:let* ((header
                              (%BlockHeader
                               (parse-hex-to-ufix (json-get obj "number"))
                               (parse-hex-to-bytes (json-get obj "hash"))
                               (parse-hex-to-bytes (json-get obj "parentHash"))
                               (parse-hex-to-bytes (json-get obj "nonce"))
                               (parse-hex-to-bytes (json-get obj "sha3Uncles"))
                               (parse-hex-to-bytes (json-get obj "logsBloom"))
                               (parse-hex-to-bytes (json-get obj "transactionsRoot"))
                               (parse-hex-to-bytes (json-get obj "stateRoot"))
                               (parse-hex-to-bytes (json-get obj "receiptsRoot"))
                               (parse-address-from-hex (json-get obj "miner"))
                               (parse-hex-to-u256 (json-get obj "difficulty"))
                               (parse-optional-u256 (json-get obj "totalDifficulty"))
                               (parse-hex-to-bytes (json-get obj "extraData"))
                               (parse-hex-to-ufix (json-get obj "size"))
                               (parse-hex-to-ufix (json-get obj "gasLimit"))
                               (parse-hex-to-ufix (json-get obj "gasUsed"))
                               (parse-hex-to-ufix (json-get obj "timestamp"))
                               (parse-optional-ufix (json-get obj "baseFeePerGas"))
                               (parse-optional-bytes (json-get obj "withdrawalsRoot"))
                               (parse-optional-ufix (json-get obj "blobGasUsed"))
                               (parse-optional-ufix (json-get obj "excessBlobGas"))))
                            ;; Parse transactions
                            (raw-txs (json-get obj "transactions"))
                            (txs (cl:if (cl:null raw-txs)
                                        Nil
                                        (cl:reduce
                                         (cl:lambda (acc tx)
                                           (Cons (cl:if full-txs
                                                        (TxFull (parse-hex-to-bytes
                                                                 (cl:or (json-get tx "hash") "")))
                                                        (TxHash (parse-hex-to-bytes tx)))
                                                 acc))
                                         raw-txs
                                         :initial-value Nil
                                         :from-end cl:t)))
                            ;; Parse uncles
                            (raw-uncles (json-get obj "uncles"))
                            (uncles (cl:if (cl:null raw-uncles)
                                           Nil
                                           (cl:reduce
                                            (cl:lambda (acc u)
                                              (Cons (parse-hex-to-bytes u) acc))
                                            raw-uncles
                                            :initial-value Nil
                                            :from-end cl:t)))
                            ;; Parse withdrawals
                            (raw-withdrawals (json-get obj "withdrawals"))
                            (withdrawals
                             (cl:if (cl:null raw-withdrawals)
                                    Nil
                                    (cl:reduce
                                     (cl:lambda (acc w)
                                       (Cons (Withdrawal
                                              (parse-hex-to-ufix (json-get w "index"))
                                              (parse-hex-to-ufix (json-get w "validatorIndex"))
                                              (parse-address-from-hex (json-get w "address"))
                                              (parse-hex-to-ufix (json-get w "amount")))
                                             acc))
                                     raw-withdrawals
                                     :initial-value Nil
                                     :from-end cl:t)))
                            (block (%Block header txs uncles withdrawals)))
                     (cl:make-instance 'coalton-library/classes::result/ok
                                       'coalton-library/classes::|_0| block))))
        (cl:error (e)
          (cl:declare (cl:ignore e))
          (cl:make-instance 'coalton-library/classes::result/err
                            'coalton-library/classes::|_0|
                            (types:ProviderError "Parse error"))))))

  ;;; =========================================================================
  ;;; JSON-RPC Request Encoding
  ;;; =========================================================================

  (declare encode-get-block-by-number-request (BlockTag -> Boolean -> UFix -> String))
  (define (encode-get-block-by-number-request tag full-txs request-id)
    "Encode eth_getBlockByNumber request"
    (let ((tag-str (block-tag-to-string tag))
          (full-str (if full-txs "true" "false")))
      (lisp String (tag-str full-str request-id)
        (json:encode-json-to-string
         `(("jsonrpc" . "2.0")
           ("method" . "eth_getBlockByNumber")
           ("params" . (,tag-str ,(cl:if (cl:string= full-str "true") cl:t cl:nil)))
           ("id" . ,request-id))))))

  (declare encode-get-block-by-hash-request (types:Bytes -> Boolean -> UFix -> String))
  (define (encode-get-block-by-hash-request hash full-txs request-id)
    "Encode eth_getBlockByHash request"
    (let ((hash-hex (types:hex-encode-prefixed hash))
          (full-str (if full-txs "true" "false")))
      (lisp String (hash-hex full-str request-id)
        (json:encode-json-to-string
         `(("jsonrpc" . "2.0")
           ("method" . "eth_getBlockByHash")
           ("params" . (,hash-hex ,(cl:if (cl:string= full-str "true") cl:t cl:nil)))
           ("id" . ,request-id))))))

  (declare parse-get-block-response (String -> Boolean -> (types:Web3Result (Optional Block))))
  (define (parse-get-block-response json-str full-txs)
    "Parse eth_getBlockByNumber/Hash response"
    (lisp (types:Web3Result (Optional Block)) (json-str full-txs)
      (cl:handler-case
          (cl:let* ((obj (json:decode-json-from-string json-str))
                    (result (json-get obj "result"))
                    (rpc-err (json-get obj "error")))
            (cl:cond
              (rpc-err
               (cl:make-instance 'coalton-library/classes::result/err
                                 'coalton-library/classes::|_0|
                                 (types:ProviderError
                                  (cl:or (json-get rpc-err "message") "Unknown error"))))
              ((cl:null result)
               ;; Block not found
               (cl:make-instance 'coalton-library/classes::result/ok
                                 'coalton-library/classes::|_0|
                                 (make-coalton-none)))
              (cl:t
               ;; Parse the block
               (cl:let* ((header
                          (%BlockHeader
                           (parse-hex-to-ufix (json-get result "number"))
                           (parse-hex-to-bytes (json-get result "hash"))
                           (parse-hex-to-bytes (json-get result "parentHash"))
                           (parse-hex-to-bytes (json-get result "nonce"))
                           (parse-hex-to-bytes (json-get result "sha3Uncles"))
                           (parse-hex-to-bytes (json-get result "logsBloom"))
                           (parse-hex-to-bytes (json-get result "transactionsRoot"))
                           (parse-hex-to-bytes (json-get result "stateRoot"))
                           (parse-hex-to-bytes (json-get result "receiptsRoot"))
                           (parse-address-from-hex (json-get result "miner"))
                           (parse-hex-to-u256 (json-get result "difficulty"))
                           (parse-optional-u256 (json-get result "totalDifficulty"))
                           (parse-hex-to-bytes (json-get result "extraData"))
                           (parse-hex-to-ufix (json-get result "size"))
                           (parse-hex-to-ufix (json-get result "gasLimit"))
                           (parse-hex-to-ufix (json-get result "gasUsed"))
                           (parse-hex-to-ufix (json-get result "timestamp"))
                           (parse-optional-ufix (json-get result "baseFeePerGas"))
                           (parse-optional-bytes (json-get result "withdrawalsRoot"))
                           (parse-optional-ufix (json-get result "blobGasUsed"))
                           (parse-optional-ufix (json-get result "excessBlobGas"))))
                         (raw-txs (json-get result "transactions"))
                         (txs (cl:if (cl:null raw-txs)
                                     Nil
                                     (cl:reduce
                                      (cl:lambda (acc tx)
                                        (Cons (cl:if full-txs
                                                     (TxFull (parse-hex-to-bytes
                                                              (cl:or (json-get tx "hash") "")))
                                                     (TxHash (parse-hex-to-bytes tx)))
                                              acc))
                                      raw-txs
                                      :initial-value Nil
                                      :from-end cl:t)))
                         (raw-uncles (json-get result "uncles"))
                         (uncles (cl:if (cl:null raw-uncles)
                                        Nil
                                        (cl:reduce
                                         (cl:lambda (acc u)
                                           (Cons (parse-hex-to-bytes u) acc))
                                         raw-uncles
                                         :initial-value Nil
                                         :from-end cl:t)))
                         (raw-withdrawals (json-get result "withdrawals"))
                         (withdrawals
                          (cl:if (cl:null raw-withdrawals)
                                 Nil
                                 (cl:reduce
                                  (cl:lambda (acc w)
                                    (Cons (Withdrawal
                                           (parse-hex-to-ufix (json-get w "index"))
                                           (parse-hex-to-ufix (json-get w "validatorIndex"))
                                           (parse-address-from-hex (json-get w "address"))
                                           (parse-hex-to-ufix (json-get w "amount")))
                                          acc))
                                  raw-withdrawals
                                  :initial-value Nil
                                  :from-end cl:t)))
                         (block (%Block header txs uncles withdrawals)))
                 (cl:make-instance 'coalton-library/classes::result/ok
                                   'coalton-library/classes::|_0|
                                   (make-coalton-some block))))))
        (cl:error (e)
          (cl:declare (cl:ignore e))
          (cl:make-instance 'coalton-library/classes::result/err
                            'coalton-library/classes::|_0|
                            (types:ProviderError "Parse error")))))))


;;; =========================================================================
;;; Exports
;;; =========================================================================

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:export '(BlockHeader
               make-block-header
               header-number
               header-hash
               header-parent-hash
               header-nonce
               header-sha3-uncles
               header-logs-bloom
               header-transactions-root
               header-state-root
               header-receipts-root
               header-miner
               header-difficulty
               header-total-difficulty
               header-extra-data
               header-size
               header-gas-limit
               header-gas-used
               header-timestamp
               header-base-fee
               header-withdrawals-root
               header-blob-gas-used
               header-excess-blob-gas
               Block
               make-block
               block-header
               block-transactions
               block-uncles
               block-withdrawals
               BlockTx
               TxHash
               TxFull
               Withdrawal
               make-withdrawal
               withdrawal-index
               withdrawal-validator-index
               withdrawal-address
               withdrawal-amount
               parse-block-header
               parse-block
               parse-withdrawal
               encode-get-block-by-number-request
               encode-get-block-by-hash-request
               parse-get-block-response
               BlockTag
               TagLatest
               TagPending
               TagFinalized
               TagSafe
               TagEarliest
               TagNumber
               block-tag-to-string
               is-post-merge
               is-post-shanghai
               is-post-cancun
               block-age
               gas-utilization)
             (cl:find-package '#:web3/block)))
