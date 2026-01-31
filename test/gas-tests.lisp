;;; Gas Utilities tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Gas Utilities Tests
;;; =========================================================================

(defun run-gas-tests ()
  (format t "~%=== Gas Utilities Tests ===~%")

  ;;; =========================================================================
  ;;; Fee Types Tests
  ;;; =========================================================================

  (test-case "make-gas-fees creates EIP-1559 fees"
    (let* ((max-fee (coalton:coalton (web3/gas:gwei-to-wei 100)))
           (priority-fee (coalton:coalton (web3/gas:gwei-to-wei 2)))
           (fees (coalton:coalton
                  (web3/gas:make-gas-fees
                   (coalton:lisp web3/types:U256 () max-fee)
                   (coalton:lisp web3/types:U256 () priority-fee)))))
      (assert fees)))

  (test-case "make-legacy-gas-price creates legacy price"
    (let* ((price (coalton:coalton (web3/gas:gwei-to-wei 50)))
           (legacy (coalton:coalton
                    (web3/gas:make-legacy-gas-price
                     (coalton:lisp web3/types:U256 () price)))))
      (assert legacy)))

  ;;; =========================================================================
  ;;; Base Fee Calculation Tests
  ;;; =========================================================================

  (test-case "calculate-next-base-fee increases when gas > 50%"
    (let* ((current (coalton:coalton (web3/gas:gwei-to-wei 100)))
           (next (coalton:coalton
                  (web3/gas:calculate-next-base-fee
                   (coalton:lisp web3/types:U256 () current)
                   75)))  ; 75% gas used > 50% target
           (current-gwei (coalton:coalton
                          (web3/gas:wei-to-gwei
                           (coalton:lisp web3/types:U256 () current))))
           (next-gwei (coalton:coalton
                       (web3/gas:wei-to-gwei
                        (coalton:lisp web3/types:U256 () next)))))
      ;; Next should be higher than current
      (assert (> next-gwei current-gwei))))

  (test-case "calculate-next-base-fee decreases when gas < 50%"
    (let* ((current (coalton:coalton (web3/gas:gwei-to-wei 100)))
           (next (coalton:coalton
                  (web3/gas:calculate-next-base-fee
                   (coalton:lisp web3/types:U256 () current)
                   25)))  ; 25% gas used < 50% target
           (current-gwei (coalton:coalton
                          (web3/gas:wei-to-gwei
                           (coalton:lisp web3/types:U256 () current))))
           (next-gwei (coalton:coalton
                       (web3/gas:wei-to-gwei
                        (coalton:lisp web3/types:U256 () next)))))
      ;; Next should be lower than current
      (assert (< next-gwei current-gwei))))

  (test-case "calculate-next-base-fee unchanged at 50%"
    (let* ((current (coalton:coalton (web3/gas:gwei-to-wei 100)))
           (next (coalton:coalton
                  (web3/gas:calculate-next-base-fee
                   (coalton:lisp web3/types:U256 () current)
                   50)))  ; Exactly at target
           (current-gwei (coalton:coalton
                          (web3/gas:wei-to-gwei
                           (coalton:lisp web3/types:U256 () current))))
           (next-gwei (coalton:coalton
                       (web3/gas:wei-to-gwei
                        (coalton:lisp web3/types:U256 () next)))))
      ;; Should be the same
      (assert (= next-gwei current-gwei))))

  ;;; =========================================================================
  ;;; Priority Fee Tests
  ;;; =========================================================================

  (test-case "suggest-priority-fee returns median"
    (let* ((fee1 (coalton:coalton (web3/gas:gwei-to-wei 1)))
           (fee2 (coalton:coalton (web3/gas:gwei-to-wei 2)))
           (fee3 (coalton:coalton (web3/gas:gwei-to-wei 3)))
           (fees (cl:list fee1 fee2 fee3))
           (suggested (coalton:coalton
                       (web3/gas:suggest-priority-fee
                        (coalton:lisp (coalton:List web3/types:U256) () fees))))
           (suggested-gwei (coalton:coalton
                            (web3/gas:wei-to-gwei
                             (coalton:lisp web3/types:U256 () suggested)))))
      ;; Median of [1, 2, 3] is 2
      (assert (= suggested-gwei 2))))

  (test-case "suggest-priority-fee returns default for empty list"
    (let* ((suggested (coalton:coalton
                       (web3/gas:suggest-priority-fee coalton:Nil)))
           (suggested-gwei (coalton:coalton
                            (web3/gas:wei-to-gwei
                             (coalton:lisp web3/types:U256 () suggested)))))
      ;; Default is 1 gwei
      (assert (= suggested-gwei 1))))

  ;;; =========================================================================
  ;;; Fee Preset Tests
  ;;; =========================================================================

  (test-case "preset-multiplier returns correct values"
    (assert (= (coalton:coalton (web3/gas:preset-multiplier web3/gas:FeeSlow)) 80))
    (assert (= (coalton:coalton (web3/gas:preset-multiplier web3/gas:FeeNormal)) 100))
    (assert (= (coalton:coalton (web3/gas:preset-multiplier web3/gas:FeeFast)) 150))
    (assert (= (coalton:coalton (web3/gas:preset-multiplier web3/gas:FeeInstant)) 200)))

  ;;; =========================================================================
  ;;; Complete Fee Suggestion Tests
  ;;; =========================================================================

  (test-case "suggest-fees creates valid EIP-1559 fees"
    (let* ((base-fee (coalton:coalton (web3/gas:gwei-to-wei 50)))
           (priority-fee (coalton:coalton (web3/gas:gwei-to-wei 2)))
           (fees (coalton:coalton
                  (web3/gas:suggest-fees
                   (coalton:lisp web3/types:U256 () base-fee)
                   (coalton:lisp web3/types:U256 () priority-fee)
                   web3/gas:FeeNormal)))
           (max-fee (coalton:coalton
                     (web3/gas:.max-fee-per-gas
                      (coalton:lisp web3/gas:GasFees () fees))))
           (max-priority (coalton:coalton
                          (web3/gas:.max-priority-fee-per-gas
                           (coalton:lisp web3/gas:GasFees () fees))))
           (max-fee-gwei (coalton:coalton
                          (web3/gas:wei-to-gwei
                           (coalton:lisp web3/types:U256 () max-fee))))
           (max-priority-gwei (coalton:coalton
                               (web3/gas:wei-to-gwei
                                (coalton:lisp web3/types:U256 () max-priority)))))
      ;; max-fee should be base*2 + priority = 50*2 + 2 = 102
      (assert (= max-fee-gwei 102))
      ;; priority should be 2 gwei (100% multiplier for Normal)
      (assert (= max-priority-gwei 2))))

  (test-case "suggest-fees with Fast preset increases priority"
    (let* ((base-fee (coalton:coalton (web3/gas:gwei-to-wei 50)))
           (priority-fee (coalton:coalton (web3/gas:gwei-to-wei 2)))
           (fees (coalton:coalton
                  (web3/gas:suggest-fees
                   (coalton:lisp web3/types:U256 () base-fee)
                   (coalton:lisp web3/types:U256 () priority-fee)
                   web3/gas:FeeFast)))
           (max-priority (coalton:coalton
                          (web3/gas:.max-priority-fee-per-gas
                           (coalton:lisp web3/gas:GasFees () fees))))
           (max-priority-gwei (coalton:coalton
                               (web3/gas:wei-to-gwei
                                (coalton:lisp web3/types:U256 () max-priority)))))
      ;; priority should be 2 * 1.5 = 3 gwei (150% multiplier for Fast)
      (assert (= max-priority-gwei 3))))

  ;;; =========================================================================
  ;;; Gas Limit Helper Tests
  ;;; =========================================================================

  (test-case "add-gas-buffer adds correct buffer"
    (let ((estimated 100000)
          (buffer-percent 20))
      (assert (= (coalton:coalton
                  (web3/gas:add-gas-buffer
                   (coalton:lisp coalton:UFix () estimated)
                   (coalton:lisp coalton:UFix () buffer-percent)))
                 120000))))  ; 100000 + 20% = 120000

  (test-case "standard-gas-limits returns list"
    (let ((limits (coalton:coalton (web3/gas:standard-gas-limits coalton:Unit))))
      (assert limits)
      ;; Just check that we got something back
      (assert (not (eq limits nil)))))

  ;;; =========================================================================
  ;;; Fee Comparison Tests
  ;;; =========================================================================

  (test-case "fees-sufficient? checks both conditions"
    (let* ((max-fee (coalton:coalton (web3/gas:gwei-to-wei 100)))
           (priority-fee (coalton:coalton (web3/gas:gwei-to-wei 2)))
           (fees (coalton:coalton
                  (web3/gas:make-gas-fees
                   (coalton:lisp web3/types:U256 () max-fee)
                   (coalton:lisp web3/types:U256 () priority-fee))))
           (current-base (coalton:coalton (web3/gas:gwei-to-wei 50)))
           (min-priority (coalton:coalton (web3/gas:gwei-to-wei 1)))
           (sufficient (coalton:coalton
                        (web3/gas:fees-sufficient?
                         (coalton:lisp web3/gas:GasFees () fees)
                         (coalton:lisp web3/types:U256 () current-base)
                         (coalton:lisp web3/types:U256 () min-priority)))))
      ;; Should be sufficient (100 > 50 and 2 > 1)
      (assert (eq sufficient t))))

  (test-case "fees-sufficient? returns false when insufficient"
    (let* ((max-fee (coalton:coalton (web3/gas:gwei-to-wei 30)))
           (priority-fee (coalton:coalton (web3/gas:gwei-to-wei 2)))
           (fees (coalton:coalton
                  (web3/gas:make-gas-fees
                   (coalton:lisp web3/types:U256 () max-fee)
                   (coalton:lisp web3/types:U256 () priority-fee))))
           (current-base (coalton:coalton (web3/gas:gwei-to-wei 50)))
           (min-priority (coalton:coalton (web3/gas:gwei-to-wei 1)))
           (sufficient (coalton:coalton
                        (web3/gas:fees-sufficient?
                         (coalton:lisp web3/gas:GasFees () fees)
                         (coalton:lisp web3/types:U256 () current-base)
                         (coalton:lisp web3/types:U256 () min-priority)))))
      ;; Should be insufficient (30 < 50)
      (assert (eq sufficient nil))))

  (test-case "compare-fees returns correct ordering"
    (let* ((low-fee (coalton:coalton (web3/gas:gwei-to-wei 50)))
           (high-fee (coalton:coalton (web3/gas:gwei-to-wei 100)))
           (priority (coalton:coalton (web3/gas:gwei-to-wei 2)))
           (fees-a (coalton:coalton
                    (web3/gas:make-gas-fees
                     (coalton:lisp web3/types:U256 () low-fee)
                     (coalton:lisp web3/types:U256 () priority))))
           (fees-b (coalton:coalton
                    (web3/gas:make-gas-fees
                     (coalton:lisp web3/types:U256 () high-fee)
                     (coalton:lisp web3/types:U256 () priority))))
           (cmp (coalton:coalton
                 (web3/gas:compare-fees
                  (coalton:lisp web3/gas:GasFees () fees-a)
                  (coalton:lisp web3/gas:GasFees () fees-b)))))
      ;; a < b, so should return -1
      (assert (= cmp -1))))

  ;;; =========================================================================
  ;;; JSON Parsing Tests
  ;;; =========================================================================

  (test-case "encode-fee-history-request creates valid JSON params"
    (let* ((percentiles (cl:list 25 50 75))
           (request (coalton:coalton
                     (web3/gas:encode-fee-history-request
                      4
                      "latest"
                      (coalton:lisp (coalton:List coalton:UFix) () percentiles)))))
      (assert (search "0x4" request))  ; block count
      (assert (search "latest" request))
      (assert (search "25" request))))

  (test-case "parse-gas-price-response parses valid response"
    (let* ((response "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":\"0x2540be400\"}")  ; 10 gwei
           (result (coalton:coalton
                    (web3/gas:parse-gas-price-response
                     (coalton:lisp coalton:String () response)))))
      (assert (result-ok-p result))
      (let* ((price (result-value result))
             (gwei (coalton:coalton
                    (web3/gas:wei-to-gwei
                     (coalton:lisp web3/types:U256 () price)))))
        (assert (= gwei 10)))))

  (test-case "parse-fee-history-response parses valid response"
    (let* ((response "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"oldestBlock\":\"0x100\",\"baseFeePerGas\":[\"0x2540be400\",\"0x2540be400\"],\"gasUsedRatio\":[0.5],\"reward\":[[\"0x3b9aca00\"]]}}")
           (result (coalton:coalton
                    (web3/gas:parse-fee-history-response
                     (coalton:lisp coalton:String () response)))))
      (assert (result-ok-p result)))))
