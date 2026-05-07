;;;; Tests for the web3/easy CL convenience surface, including the
;;;; defcontract macro. Network-dependent paths are skipped (they need
;;;; WEB3_INTEGRATION=1 like the rest of the suite).

(in-package #:web3-tests/runner)

;;; -------------------------------------------------------------------------
;;; Inline ERC-20 ABI used by the defcontract tests.
;;; -------------------------------------------------------------------------

;; eval-when: defcontract reads the symbol value during macro expansion,
;; which happens at file-compile time. defparameter's effect is normally a
;; load-time side-effect, so we promote it to compile-time too.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *erc20-abi-json*
  "[
    {\"type\":\"function\",\"name\":\"name\",\"inputs\":[],\"outputs\":[{\"name\":\"\",\"type\":\"string\"}],\"stateMutability\":\"view\"},
    {\"type\":\"function\",\"name\":\"symbol\",\"inputs\":[],\"outputs\":[{\"name\":\"\",\"type\":\"string\"}],\"stateMutability\":\"view\"},
    {\"type\":\"function\",\"name\":\"decimals\",\"inputs\":[],\"outputs\":[{\"name\":\"\",\"type\":\"uint8\"}],\"stateMutability\":\"view\"},
    {\"type\":\"function\",\"name\":\"totalSupply\",\"inputs\":[],\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"stateMutability\":\"view\"},
    {\"type\":\"function\",\"name\":\"balanceOf\",\"inputs\":[{\"name\":\"owner\",\"type\":\"address\"}],\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"stateMutability\":\"view\"},
    {\"type\":\"function\",\"name\":\"allowance\",\"inputs\":[{\"name\":\"owner\",\"type\":\"address\"},{\"name\":\"spender\",\"type\":\"address\"}],\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"stateMutability\":\"view\"},
    {\"type\":\"function\",\"name\":\"transfer\",\"inputs\":[{\"name\":\"to\",\"type\":\"address\"},{\"name\":\"amount\",\"type\":\"uint256\"}],\"outputs\":[{\"name\":\"\",\"type\":\"bool\"}],\"stateMutability\":\"nonpayable\"},
    {\"type\":\"function\",\"name\":\"approve\",\"inputs\":[{\"name\":\"spender\",\"type\":\"address\"},{\"name\":\"amount\",\"type\":\"uint256\"}],\"outputs\":[{\"name\":\"\",\"type\":\"bool\"}],\"stateMutability\":\"nonpayable\"},
    {\"type\":\"function\",\"name\":\"transferFrom\",\"inputs\":[{\"name\":\"from\",\"type\":\"address\"},{\"name\":\"to\",\"type\":\"address\"},{\"name\":\"amount\",\"type\":\"uint256\"}],\"outputs\":[{\"name\":\"\",\"type\":\"bool\"}],\"stateMutability\":\"nonpayable\"},
    {\"type\":\"event\",\"name\":\"Transfer\",\"anonymous\":false,
     \"inputs\":[{\"name\":\"from\",\"type\":\"address\",\"indexed\":true},
                 {\"name\":\"to\",\"type\":\"address\",\"indexed\":true},
                 {\"name\":\"value\",\"type\":\"uint256\",\"indexed\":false}]},
    {\"type\":\"event\",\"name\":\"Approval\",\"anonymous\":false,
     \"inputs\":[{\"name\":\"owner\",\"type\":\"address\",\"indexed\":true},
                 {\"name\":\"spender\",\"type\":\"address\",\"indexed\":true},
                 {\"name\":\"value\",\"type\":\"uint256\",\"indexed\":false}]}
   ]"))

;; Generate wrappers once at compile/load time so the tests can call them.
(web3:defcontract testtoken
  :address "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"
  :abi *erc20-abi-json*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *erc1155-abi-json*
    "[
     {\"type\":\"function\",\"name\":\"balanceOfBatch\",
      \"inputs\":[{\"name\":\"owners\",\"type\":\"address[]\"},
                  {\"name\":\"ids\",\"type\":\"uint256[]\"}],
      \"outputs\":[{\"name\":\"\",\"type\":\"uint256[]\"}],
      \"stateMutability\":\"view\"},
     {\"type\":\"function\",\"name\":\"safeBatchTransferFrom\",
      \"inputs\":[{\"name\":\"from\",\"type\":\"address\"},
                  {\"name\":\"to\",\"type\":\"address\"},
                  {\"name\":\"ids\",\"type\":\"uint256[]\"},
                  {\"name\":\"amounts\",\"type\":\"uint256[]\"},
                  {\"name\":\"data\",\"type\":\"bytes\"}],
      \"outputs\":[],
      \"stateMutability\":\"nonpayable\"}
     ]"))

;; Bindings used by the array-support tests below.
(web3:defcontract erc1155
  :address "0x0000000000000000000000000000000000000001"
  :abi *erc1155-abi-json*)

(defun run-easy-tests ()
  (format t "~&~%=== web3/easy convenience layer tests ===~%")

  ;;; Address checksum + zero
  (test-case "address-zero is the all-zero address"
    (assert (string= (web3:address-zero)
                     "0x0000000000000000000000000000000000000000")))

  (test-case "checksum-address produces EIP-55 mixed case"
    (assert (string= (web3:checksum-address
                      "0xd8da6bf26964af9d7eed9e03e53415d37aa96045")
                     "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045")))

  (test-case "checksum-address signals web3-error on garbage input"
    (handler-case (progn (web3:checksum-address "not-hex") nil)
      (web3:web3-error () (assert t))
      (:no-error (_)
        (declare (ignore _))
        (error "expected web3-error to be signaled"))))

  ;;; Unit conversion
  (test-case "parse-ether 1.5 = 1.5e18 wei"
    (assert (= (web3:parse-ether "1.5") 1500000000000000000)))

  (test-case "format-ether round-trips parse-ether"
    (assert (string= (web3:format-ether 1500000000000000000) "1.5")))

  (test-case "parse-gwei 30 = 3e10 wei"
    (assert (= (web3:parse-gwei "30") 30000000000)))

  (test-case "parse-units honors decimals (USDC = 6)"
    (assert (= (web3:parse-units "100.5" 6) 100500000)))

  (test-case "format-ether signals on negative input"
    (handler-case (progn (web3:format-ether -1) nil)
      (web3:web3-error () (assert t))
      (:no-error (_)
        (declare (ignore _))
        (error "expected web3-error on negative input"))))

  ;;; Hashing
  (test-case "keccak256 of empty string matches the known constant"
    (assert (string= (web3:keccak256 "0x")
                     "0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470")))

  ;;; defcontract: smoke
  (test-case "defcontract generates read wrappers, calldata builders, and send wrappers"
    (dolist (sym '(;; reads (view/pure)
                   testtoken-name testtoken-symbol testtoken-decimals
                   testtoken-total-supply testtoken-balance-of testtoken-allowance
                   ;; calldata builders (non-view -data)
                   testtoken-transfer-data testtoken-approve-data
                   testtoken-transfer-from-data
                   ;; send wrappers (non-view, take a wallet)
                   testtoken-transfer testtoken-approve
                   testtoken-transfer-from))
      (assert (fboundp sym))))

  ;;; defcontract: transfer selector matches the canonical 0xa9059cbb
  (test-case "defcontract transfer-data starts with 0xa9059cbb"
    (let ((data (testtoken-transfer-data
                 "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045"
                 1500)))
      (assert (string= (subseq data 0 10) "0xa9059cbb"))))

  (test-case "defcontract approve-data starts with 0x095ea7b3"
    (let ((data (testtoken-approve-data
                 "0x0000000000000000000000000000000000000001"
                 1000)))
      (assert (string= (subseq data 0 10) "0x095ea7b3"))))

  (test-case "defcontract transfer-from-data starts with 0x23b872dd"
    (let ((data (testtoken-transfer-from-data
                 "0x1111111111111111111111111111111111111111"
                 "0x2222222222222222222222222222222222222222"
                 42)))
      (assert (string= (subseq data 0 10) "0x23b872dd"))))

  ;;; defcontract: array support (T[], T[N])
  (test-case "defcontract array-using ABI generates the expected wrappers"
    (dolist (sym '(erc1155-balance-of-batch                    ; (address[],uint256[]) -> uint256[]
                   erc1155-safe-batch-transfer-from            ; send wrapper
                   erc1155-safe-batch-transfer-from-data))     ; calldata builder
      (assert (fboundp sym))))

  (test-case "defcontract uint256[] inputs encode equivalently to the typed helper"
    (let* ((macro-out
             (erc1155-safe-batch-transfer-from-data
              "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045"
              "0x70997970C51812dc3A010C7d01b50e0d17dc79C8"
              '(1 2 3) '(10 20 30) "0x"))
           ;; Assemble the same calldata via the existing Coalton helper.
           (typed-out
             (let* ((from-coa (web3/easy::%parse-address
                               "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045"))
                    (to-coa   (web3/easy::%parse-address
                               "0x70997970C51812dc3A010C7d01b50e0d17dc79C8"))
                    (ids-coa  (mapcar #'web3/easy::%integer-to-u256 '(1 2 3)))
                    (amts-coa (mapcar #'web3/easy::%integer-to-u256 '(10 20 30)))
                    (empty    (web3/easy::%hex-to-bytes "0x"))
                    (calldata (coalton:coalton
                                (web3/erc1155:erc1155-safe-batch-transfer-from-data
                                 (coalton:lisp web3/address:Address () from-coa)
                                 (coalton:lisp web3/address:Address () to-coa)
                                 (coalton:lisp (coalton:List web3/types:U256) () ids-coa)
                                 (coalton:lisp (coalton:List web3/types:U256) () amts-coa)
                                 (coalton:lisp web3/types:Bytes () empty)))))
               (web3/easy::%bytes-to-hex calldata))))
      (assert (string= macro-out typed-out))))

  (test-case "defcontract calldata encodes uint and address in declared order"
    (let* ((data (testtoken-transfer-data
                  "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045"
                  1500))
           ;; Skip "0x" + 8 hex chars (4-byte selector).
           (rest (subseq data 10))
           ;; First 32-byte word should be the address (right-padded to 20 bytes
           ;; left-padded to 32 with zeros). Second word should be 0x5dc (1500).
           (word1 (subseq rest  0 64))
           (word2 (subseq rest 64 128)))
      (assert (string-equal word1
                            "000000000000000000000000d8da6bf26964af9d7eed9e03e53415d37aa96045"))
      (assert (string-equal word2
                            "00000000000000000000000000000000000000000000000000000000000005dc"))))

  ;;; defcontract: events
  (test-case "defcontract event-* helpers are generated for each ABI event"
    (dolist (sym '(testtoken-event-transfer-topic
                   testtoken-event-transfer
                   testtoken-event-approval-topic
                   testtoken-event-approval))
      (assert (fboundp sym))))

  (test-case "defcontract event-Transfer topic equals web3/events:erc20-transfer-topic"
    (let ((macro-topic (testtoken-event-transfer-topic))
          (canonical (coalton:coalton
                       (web3/types:hex-encode-prefixed
                        web3/events:erc20-transfer-topic))))
      (assert (string-equal macro-topic canonical))))

  (test-case "defcontract event decoder splits indexed/non-indexed and recovers values"
    (let* ((topics (list (testtoken-event-transfer-topic)
                         "0x000000000000000000000000d8da6bf26964af9d7eed9e03e53415d37aa96045"
                         "0x00000000000000000000000070997970c51812dc3a010c7d01b50e0d17dc79c8"))
           (data   "0x00000000000000000000000000000000000000000000000000000000000005dc")
           (decoded (testtoken-event-transfer topics data)))
      (assert (string-equal (getf decoded :from)
                            "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045"))
      (assert (string-equal (getf decoded :to)
                            "0x70997970C51812dc3A010C7d01b50e0d17dc79C8"))
      (assert (= (getf decoded :value) 1500))))

  ;;; with-fallback (retry + URL fallback)
  (test-case "with-fallback returns the first successful URL's result"
    (let* ((calls '())
           (result (web3:with-fallback
                    '("primary" "backup")
                    (lambda (url) (push url calls) (* 2 (length url))))))
      (assert (= result (* 2 (length "primary"))))
      (assert (equal (reverse calls) '("primary")))))

  (test-case "with-fallback retries transient errors then moves to next URL"
    (let ((attempts '()))
      (handler-case
          (web3:with-fallback
           '("a" "b")
           (lambda (url)
             (push url attempts)
             (error 'web3:web3-error :message "HTTP 503 Service Unavailable"))
           :max-retries-per-url 2
           :initial-backoff-ms 0
           :max-backoff-ms 0)
        (web3:web3-error () (assert t)))
      ;; 2 retries on "a", then 2 retries on "b" = 4 attempts total
      (assert (= (length attempts) 4))
      (assert (equal (reverse attempts) '("a" "a" "b" "b")))))

  (test-case "with-fallback short-circuits on non-retryable errors"
    (let ((attempts 0))
      (handler-case
          (web3:with-fallback
           '("a" "b")
           (lambda (url)
             (declare (ignore url))
             (incf attempts)
             (error 'web3:web3-error :message "Invalid hex string"))
           :max-retries-per-url 5
           :initial-backoff-ms 0)
        (web3:web3-error () (assert t)))
      ;; Permanent error: no retry, no fallback. Single attempt.
      (assert (= attempts 1))))

  (test-case "with-fallback signals when given an empty URL list"
    (handler-case (progn (web3:with-fallback '() (lambda (u) u)) nil)
      (web3:web3-error () (assert t))
      (:no-error (_)
        (declare (ignore _))
        (error "expected web3-error on empty URL list"))))

  ;;; Multicall encoding (offline; full call needs a network)
  (test-case "multicall canonical address constant matches Multicall3 deployment"
    (assert (string= web3:*multicall3-address*
                     "0xcA11bde05977b3631167028862bE2a173976CA11")))

  (test-case "multicall decode handles a known aggregate3 response"
    ;; Hand-built response: 1 result, success=true, data=0x... (32 bytes of value)
    ;; Layout: array offset, array length, tuple offset, success bool,
    ;;         bytes offset, bytes length, padded bytes data.
    (let* ((hex (concatenate
                 'string
                 ;; array offset = 32
                 "0x0000000000000000000000000000000000000000000000000000000000000020"
                 ;; array length = 1
                 "0000000000000000000000000000000000000000000000000000000000000001"
                 ;; tuple offset (within array) = 32
                 "0000000000000000000000000000000000000000000000000000000000000020"
                 ;; success = true
                 "0000000000000000000000000000000000000000000000000000000000000001"
                 ;; bytes offset (within tuple) = 64
                 "0000000000000000000000000000000000000000000000000000000000000040"
                 ;; bytes length = 32
                 "0000000000000000000000000000000000000000000000000000000000000020"
                 ;; bytes value (uint256 = 1500)
                 "00000000000000000000000000000000000000000000000000000000000005dc"))
           (raw (web3/easy::%hex-to-bytes hex))
           (decoded
             (web3/easy::%check (d (coalton:coalton
                                     (web3/easy-bridge:decode-aggregate3-response
                                      (coalton:lisp web3/types:Bytes () raw))))
               d)))
      ;; decoded is a CL list of (Tuple Boolean Bytes); one entry expected.
      (assert (= (length decoded) 1))))

  ;;; Block tag normalization
  (test-case "get-block tag normalization handles ints, keywords and strings"
    (assert (string= (web3/easy::%block-tag-to-string 1234) "0x4D2"))
    (assert (string= (web3/easy::%block-tag-to-string :latest) "latest"))
    (assert (string= (web3/easy::%block-tag-to-string :pending) "pending"))
    (assert (string= (web3/easy::%block-tag-to-string :finalized) "finalized"))
    (assert (string= (web3/easy::%block-tag-to-string "0xDEADBEEF") "0xDEADBEEF"))
    (handler-case (progn (web3/easy::%block-tag-to-string -1) nil)
      (web3:web3-error () (assert t))
      (:no-error (_)
        (declare (ignore _))
        (error "negative integer tag should signal"))))

  (cond ((uiop:getenv "WEB3_INTEGRATION")
         ;; If the user has an Anvil/local node up, exercise a read.
         ;; Skipped by default to keep the suite hermetic.
         (let ((url (or (uiop:getenv "WEB3_TEST_RPC_URL") "http://127.0.0.1:8545")))
           (test-case "easy:get-block-number returns a non-negative integer"
             (let ((n (web3:get-block-number url)))
               (assert (and (integerp n) (>= n 0)))))))
        (t
         (format t "  Note: set WEB3_INTEGRATION=1 for live easy/defcontract integration~%"))))
