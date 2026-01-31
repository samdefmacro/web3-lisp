;;;; Gas Utilities implementation
;;;; EIP-1559 fee calculation, gas estimation, and fee suggestions

(in-package #:web3/gas)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Fee Types
  ;;; =========================================================================

  (define-struct GasFees
    "EIP-1559 gas fees"
    (max-fee-per-gas types:U256)
    (max-priority-fee-per-gas types:U256))

  (declare make-gas-fees (types:U256 -> types:U256 -> GasFees))
  (define (make-gas-fees max-fee priority-fee)
    (GasFees max-fee priority-fee))

  (define-struct LegacyGasPrice
    "Legacy (pre-EIP-1559) gas price"
    (gas-price types:U256))

  (declare make-legacy-gas-price (types:U256 -> LegacyGasPrice))
  (define (make-legacy-gas-price price)
    (LegacyGasPrice price))

  (define-type FeeEstimate
    "Either EIP-1559 or legacy fee estimate"
    (Eip1559Fees GasFees)
    (LegacyFees LegacyGasPrice))

  ;;; =========================================================================
  ;;; Fee History Types
  ;;; =========================================================================

  (define-struct FeeHistoryBlock
    "Fee data for a single historical block"
    (block-base-fee types:U256)
    (block-gas-used-ratio UFix)      ; Percentage (0-100) as integer
    (block-priority-fees (List types:U256)))  ; Priority fees at requested percentiles

  (declare make-fee-history-block (types:U256 -> UFix -> (List types:U256) -> FeeHistoryBlock))
  (define (make-fee-history-block base-fee ratio priority-fees)
    (FeeHistoryBlock base-fee ratio priority-fees))

  (define-struct FeeHistory
    "Fee history from eth_feeHistory"
    (history-oldest-block UFix)
    (history-blocks (List FeeHistoryBlock))
    (history-base-fee-next types:U256))  ; Predicted next block base fee

  (declare make-fee-history (UFix -> (List FeeHistoryBlock) -> types:U256 -> FeeHistory))
  (define (make-fee-history oldest blocks next-base-fee)
    (FeeHistory oldest blocks next-base-fee))

  ;;; =========================================================================
  ;;; Fee Presets
  ;;; =========================================================================

  (define-type FeePreset
    "Preset fee levels for different urgency"
    FeeSlow      ; Lower fees, may take longer
    FeeNormal    ; Standard fees
    FeeFast      ; Higher fees for faster inclusion
    FeeInstant)  ; Highest fees for immediate inclusion

  (declare preset-multiplier (FeePreset -> UFix))
  (define (preset-multiplier preset)
    "Get the priority fee multiplier for a preset (percentage)"
    (match preset
      ((FeeSlow) 80)
      ((FeeNormal) 100)
      ((FeeFast) 150)
      ((FeeInstant) 200)))

  ;;; =========================================================================
  ;;; Base Fee Calculation (EIP-1559)
  ;;; =========================================================================

  (declare calculate-next-base-fee (types:U256 -> UFix -> types:U256))
  (define (calculate-next-base-fee current-base-fee gas-used-ratio)
    "Calculate the next block's base fee based on current base fee and gas usage.
     gas-used-ratio is percentage (0-100) of block gas limit used.
     EIP-1559: base fee changes by up to 12.5% per block."
    (lisp types:U256 (current-base-fee gas-used-ratio)
      (cl:let* ((base-int (web3/types::u256-to-integer current-base-fee))
                (target-ratio 50)  ; Target is 50% gas usage
                (max-change-denom 8)  ; 12.5% = 1/8
                (diff (cl:- gas-used-ratio target-ratio))
                ;; Calculate change: base * |diff| / 50 / 8
                (change (cl:floor (cl:* base-int (cl:abs diff)) (cl:* target-ratio max-change-denom)))
                (new-base (cl:if (cl:>= gas-used-ratio target-ratio)
                                 (cl:+ base-int change)
                                 (cl:max 0 (cl:- base-int change)))))
        (web3/types::u256-from-integer new-base))))

  (declare estimate-base-fee-trend (FeeHistory -> types:U256))
  (define (estimate-base-fee-trend history)
    "Estimate base fee trend from history. Returns suggested base fee."
    ;; Use the predicted next base fee from the history
    (.history-base-fee-next history))

  ;;; =========================================================================
  ;;; Priority Fee Suggestions
  ;;; =========================================================================

  (declare suggest-priority-fee ((List types:U256) -> types:U256))
  (define (suggest-priority-fee recent-priority-fees)
    "Suggest a priority fee based on recent block data.
     Takes median of recent priority fees."
    (if (list:null? recent-priority-fees)
        (types:u256-from-integer 1000000000)  ; Default 1 gwei in wei
        (let ((sorted (sort-u256-list recent-priority-fees))
              (len (list:length recent-priority-fees)))
          (list-ref-u256 sorted (ufix-div len 2)))))

  (declare priority-fee-percentile ((List FeeHistoryBlock) -> UFix -> types:U256))
  (define (priority-fee-percentile blocks percentile-idx)
    "Get priority fee at specific percentile index from history blocks.
     percentile-idx corresponds to the index in the reward arrays."
    (let ((fees (collect-priority-fees blocks percentile-idx)))
      (suggest-priority-fee fees)))

  (declare collect-priority-fees ((List FeeHistoryBlock) -> UFix -> (List types:U256)))
  (define (collect-priority-fees blocks idx)
    "Collect priority fees at given percentile index from all blocks"
    (match blocks
      ((Nil) Nil)
      ((Cons block rest)
       (let ((fees (.block-priority-fees block)))
         (match (list-ref-u256-opt fees idx)
           ((Some fee) (Cons fee (collect-priority-fees rest idx)))
           ((None) (collect-priority-fees rest idx)))))))

  ;;; =========================================================================
  ;;; Complete Fee Suggestions
  ;;; =========================================================================

  (declare suggest-fees (types:U256 -> types:U256 -> FeePreset -> GasFees))
  (define (suggest-fees base-fee priority-fee preset)
    "Suggest complete EIP-1559 fees based on base fee, priority fee, and preset.
     max-fee = base-fee * 2 + priority-fee (with preset adjustment)"
    (let ((adjusted-priority (apply-preset-multiplier priority-fee preset)))
      ;; max-fee-per-gas should cover 2x base fee increase plus priority
      (let ((max-fee (u256-add (u256-mul-2 base-fee) adjusted-priority)))
        (GasFees max-fee adjusted-priority))))

  (declare suggest-fees-from-history (FeeHistory -> FeePreset -> GasFees))
  (define (suggest-fees-from-history history preset)
    "Suggest fees based on fee history and preset"
    (let ((base-fee (.history-base-fee-next history))
          (priority-fee (priority-fee-percentile (.history-blocks history) 0)))
      (suggest-fees base-fee priority-fee preset)))

  ;;; =========================================================================
  ;;; Gas Limit Helpers
  ;;; =========================================================================

  (declare add-gas-buffer (UFix -> UFix -> UFix))
  (define (add-gas-buffer estimated-gas buffer-percent)
    "Add a safety buffer to estimated gas. buffer-percent is e.g. 20 for 20%"
    (+ estimated-gas (ufix-div (* estimated-gas buffer-percent) 100)))

  (declare standard-gas-limits (Unit -> (List (Tuple String UFix))))
  (define (standard-gas-limits _)
    "Common gas limits for standard operations"
    (make-list
     (Tuple "eth-transfer" 21000)
     (Tuple "erc20-transfer" 65000)
     (Tuple "erc20-approve" 46000)
     (Tuple "erc721-transfer" 85000)
     (Tuple "uniswap-swap" 150000)
     (Tuple "contract-deploy" 1000000)))

  ;;; =========================================================================
  ;;; Fee Comparison
  ;;; =========================================================================

  (declare fees-sufficient? (GasFees -> types:U256 -> types:U256 -> Boolean))
  (define (fees-sufficient? fees current-base-fee min-priority-fee)
    "Check if fees are sufficient for current network conditions"
    (and (types:u256-greater-than? (.max-fee-per-gas fees)
                                    current-base-fee)
         (types:u256-greater-than? (.max-priority-fee-per-gas fees)
                                    min-priority-fee)))

  (declare compare-fees (GasFees -> GasFees -> Integer))
  (define (compare-fees a b)
    "Compare two fee estimates. Returns -1, 0, or 1"
    (let ((max-a (.max-fee-per-gas a))
          (max-b (.max-fee-per-gas b)))
      (cond
        ((types:u256-less-than? max-a max-b) -1)
        ((types:u256-greater-than? max-a max-b) 1)
        (True 0))))

  ;;; =========================================================================
  ;;; Helper Functions
  ;;; =========================================================================

  (declare apply-preset-multiplier (types:U256 -> FeePreset -> types:U256))
  (define (apply-preset-multiplier fee preset)
    "Apply preset multiplier to a fee"
    (let ((mult (preset-multiplier preset)))
      (lisp types:U256 (fee mult)
        (cl:let* ((fee-int (web3/types::u256-to-integer fee))
                  (adjusted (cl:floor (cl:* fee-int mult) 100)))
          (web3/types::u256-from-integer adjusted)))))

  (declare u256-mul-2 (types:U256 -> types:U256))
  (define (u256-mul-2 x)
    "Multiply U256 by 2"
    (types:u256-add x x))

  (declare u256-add (types:U256 -> types:U256 -> types:U256))
  (define (u256-add a b)
    (types:u256-add a b))

  (declare sort-u256-list ((List types:U256) -> (List types:U256)))
  (define (sort-u256-list lst)
    "Sort a list of U256 values (simple insertion sort)"
    (lisp (List types:U256) (lst)
      (cl:labels ((to-cl-list (l)
                    (cl:if (cl:eq l coalton:Nil)
                           cl:nil
                           (cl:cons (cl:car l) (to-cl-list (cl:cdr l)))))
                  (from-cl-list (l)
                    (cl:if (cl:null l)
                           coalton:Nil
                           (coalton:Cons (cl:car l) (from-cl-list (cl:cdr l)))))
                  (u256-< (a b)
                    (cl:< (web3/types::u256-to-integer a)
                          (web3/types::u256-to-integer b))))
        (from-cl-list (cl:sort (to-cl-list lst) #'u256-<)))))

  (declare ufix-div (UFix -> UFix -> UFix))
  (define (ufix-div a b)
    "Integer division of two UFix values"
    (lisp UFix (a b)
      (cl:floor a b)))

  (declare list-ref-u256 ((List types:U256) -> UFix -> types:U256))
  (define (list-ref-u256 lst idx)
    "Get element at index, or zero if out of bounds"
    (match (list-ref-u256-opt lst idx)
      ((Some v) v)
      ((None) (types:u256-zero Unit))))

  (declare list-ref-u256-opt ((List types:U256) -> UFix -> (Optional types:U256)))
  (define (list-ref-u256-opt lst idx)
    "Get element at index as Optional"
    (match lst
      ((Nil) None)
      ((Cons x rest)
       (if (== idx 0)
           (Some x)
           (list-ref-u256-opt rest (- idx 1))))))

  ;;; =========================================================================
  ;;; Unit Conversions (convenience wrappers that work with UFix)
  ;;; =========================================================================

  (declare gwei-to-wei (UFix -> types:U256))
  (define (gwei-to-wei gwei)
    "Convert gwei (as UFix) to wei (as U256)"
    (lisp types:U256 (gwei)
      (web3/types::u256-from-integer (cl:* gwei 1000000000))))

  (declare wei-to-gwei (types:U256 -> UFix))
  (define (wei-to-gwei wei)
    "Convert wei (as U256) to gwei (as UFix, truncated)"
    (lisp UFix (wei)
      (cl:let ((wei-int (web3/types::u256-to-integer wei)))
        (cl:floor wei-int 1000000000)))))


;;; =========================================================================
;;; JSON Encoding/Parsing for RPC
;;; =========================================================================

(coalton-toplevel

  (declare encode-fee-history-request (UFix -> String -> (List UFix) -> String))
  (define (encode-fee-history-request block-count newest-block percentiles)
    "Encode eth_feeHistory request parameters"
    (lisp String (block-count newest-block percentiles)
      (%encode-fee-history-request block-count newest-block percentiles)))

  (declare parse-fee-history-response (String -> (types:Web3Result FeeHistory)))
  (define (parse-fee-history-response json-str)
    "Parse eth_feeHistory response"
    (lisp (types:Web3Result FeeHistory) (json-str)
      (%parse-fee-history-response json-str)))

  (declare parse-gas-price-response (String -> (types:Web3Result types:U256)))
  (define (parse-gas-price-response json-str)
    "Parse eth_gasPrice response"
    (lisp (types:Web3Result types:U256) (json-str)
      (%parse-gas-price-response json-str))))


;;; =========================================================================
;;; CL-Level JSON Functions
;;; =========================================================================

(cl:defun %encode-fee-history-request (block-count newest-block percentiles)
  "Encode fee history request params as JSON"
  (cl:let ((percentile-list (%coalton-list-to-cl percentiles)))
    (cl:format cl:nil "[\"0x~X\",\"~A\",[~{~A~^,~}]]"
               block-count
               newest-block
               percentile-list)))

(cl:defun %coalton-list-to-cl (lst)
  "Convert Coalton list to CL list"
  (cl:if (cl:eq lst coalton:Nil)
         cl:nil
         (cl:cons (cl:car lst) (%coalton-list-to-cl (cl:cdr lst)))))

(cl:defun %parse-fee-history-response (json-str)
  "Parse fee history response"
  (cl:handler-case
      (cl:let* ((parsed (cl-json:decode-json-from-string json-str))
                (result (cl:cdr (cl:assoc :result parsed))))
        (cl:if result
               (cl:let* ((oldest-hex (cl:cdr (cl:assoc :oldest-block result)))
                         (base-fees-hex (cl:cdr (cl:assoc :base-fee-per-gas result)))
                         (gas-ratios (cl:cdr (cl:assoc :gas-used-ratio result)))
                         (rewards (cl:cdr (cl:assoc :reward result)))
                         (oldest (%parse-hex-ufix oldest-hex))
                         (blocks (%build-fee-blocks base-fees-hex gas-ratios rewards))
                         ;; Next base fee is the last element of baseFeePerGas
                         (next-base-fee (%parse-hex-u256 (cl:car (cl:last base-fees-hex)))))
                 (Ok (web3/gas::make-fee-history oldest blocks next-base-fee)))
               (Err (web3/types:ProviderError "No result in response"))))
    (cl:error (e)
      (Err (web3/types:ProviderError
            (cl:format cl:nil "Failed to parse fee history: ~A" e))))))

(cl:defun %build-fee-blocks (base-fees-hex gas-ratios rewards)
  "Build list of FeeHistoryBlock from parsed arrays"
  ;; base-fees has N+1 elements, gas-ratios and rewards have N elements
  (cl:if (cl:or (cl:null base-fees-hex) (cl:null gas-ratios))
         coalton:Nil
         (cl:let ((base-fee (%parse-hex-u256 (cl:first base-fees-hex)))
                  (ratio (%parse-gas-ratio (cl:first gas-ratios)))
                  (priority-fees (cl:if rewards
                                        (%parse-priority-fees (cl:first rewards))
                                        coalton:Nil)))
           (coalton:Cons
            (web3/gas::make-fee-history-block base-fee ratio priority-fees)
            (%build-fee-blocks (cl:rest base-fees-hex) (cl:rest gas-ratios)
                               (cl:if rewards (cl:rest rewards) cl:nil))))))

(cl:defun %parse-priority-fees (fees-list)
  "Parse list of priority fee hex strings"
  (cl:if (cl:null fees-list)
         coalton:Nil
         (coalton:Cons (%parse-hex-u256 (cl:first fees-list))
                       (%parse-priority-fees (cl:rest fees-list)))))

(cl:defun %parse-hex-u256 (hex-str)
  "Parse hex string to U256"
  (cl:if (cl:and hex-str (cl:> (cl:length hex-str) 2))
         (cl:let ((int-val (cl:parse-integer (cl:subseq hex-str 2) :radix 16)))
           (web3/types::u256-from-integer int-val))
         (web3/types::u256-from-integer 0)))

(cl:defun %parse-hex-ufix (hex-str)
  "Parse hex string to UFix"
  (cl:if (cl:and hex-str (cl:> (cl:length hex-str) 2))
         (cl:parse-integer (cl:subseq hex-str 2) :radix 16)
         0))

(cl:defun %parse-gas-ratio (ratio)
  "Parse gas used ratio (float 0-1) to percentage integer (0-100)"
  (cl:if ratio
         (cl:round (cl:* ratio 100))
         50))

(cl:defun %parse-gas-price-response (json-str)
  "Parse gas price response"
  (cl:handler-case
      (cl:let* ((parsed (cl-json:decode-json-from-string json-str))
                (result (cl:cdr (cl:assoc :result parsed))))
        (cl:if result
               (Ok (%parse-hex-u256 result))
               (Err (web3/types:ProviderError "No result in response"))))
    (cl:error (e)
      (Err (web3/types:ProviderError
            (cl:format cl:nil "Failed to parse gas price: ~A" e))))))


;;; =========================================================================
;;; Exports
;;; =========================================================================

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:export '(GasFees
               make-gas-fees
               .max-fee-per-gas
               .max-priority-fee-per-gas
               LegacyGasPrice
               make-legacy-gas-price
               .gas-price
               FeeEstimate
               Eip1559Fees
               LegacyFees
               FeeHistoryBlock
               make-fee-history-block
               .block-base-fee
               .block-gas-used-ratio
               .block-priority-fees
               FeeHistory
               make-fee-history
               .history-oldest-block
               .history-blocks
               .history-base-fee-next
               calculate-next-base-fee
               estimate-base-fee-trend
               suggest-priority-fee
               priority-fee-percentile
               suggest-fees
               suggest-fees-from-history
               FeePreset
               FeeSlow
               FeeNormal
               FeeFast
               FeeInstant
               preset-multiplier
               add-gas-buffer
               standard-gas-limits
               gwei-to-wei
               wei-to-gwei
               fees-sufficient?
               compare-fees
               encode-fee-history-request
               parse-fee-history-response
               parse-gas-price-response)
             (cl:find-package '#:web3/gas)))
