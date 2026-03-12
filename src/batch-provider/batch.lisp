(in-package #:web3/batch-provider)
(named-readtables:in-readtable coalton:coalton)

;;; =========================================================================
;;; CL-level batch HTTP implementation
;;; =========================================================================

(cl:defun %build-batch-json (requests)
  "Build a JSON-RPC batch request body from a list of (req-id method params) triples."
  (cl:let ((parts
             (cl:loop :for req :in requests
                      :for req-id := (cl:first req)
                      :for rpc-method := (cl:second req)
                      :for params := (cl:third req)
                      :collect (cl:format cl:nil
                                 "{\"jsonrpc\":\"2.0\",\"method\":\"~A\",\"params\":~A,\"id\":~A}"
                                 rpc-method params req-id))))
    (cl:format cl:nil "[~{~A~^,~}]" parts)))

(cl:defun %parse-batch-response (json-string)
  "Parse a JSON-RPC batch response string. Returns a list of (req-id error-val result-val) triples,
   sorted by req-id."
  (cl:let* ((parsed (cl-json:decode-json-from-string json-string))
            (results
              (cl:loop :for entry :in parsed
                       :for req-id := (cl:cdr (cl:assoc :id entry))
                       :for error-val := (cl:cdr (cl:assoc :error entry))
                       :for result-val := (cl:cdr (cl:assoc :result entry))
                       :collect (cl:list req-id error-val result-val))))
    ;; Sort by req-id to match request order
    (cl:sort results #'cl:< :key #'cl:first)))

(cl:defun %batch-rpc-raw (url requests)
  "Execute a batch JSON-RPC call. REQUESTS is a CL list of (id method params) triples.
   Returns a CL list of (id error-val result-val) triples sorted by id."
  (cl:let* ((body (%build-batch-json requests))
            (response
              (dexador:post url
                            :content body
                            :headers '(("Content-Type" . "application/json")))))
    (%parse-batch-response response)))

(cl:defun %coalton-list-to-cl-triples (requests)
  "Convert a Coalton list of RpcRequest to CL list of (id method params) triples.
   Coalton List compiles to standard CL cons cells."
  (cl:loop :for inner :in requests
           :collect (cl:list
                     (cl:slot-value inner 'web3/batch-provider::|_0|)
                     (cl:slot-value inner 'web3/batch-provider::|_1|)
                     (cl:slot-value inner 'web3/batch-provider::|_2|))))

;;; =========================================================================
;;; Coalton types
;;; =========================================================================

(coalton-toplevel

  ;; A single RPC request in a batch
  (define-type RpcRequest
    "A JSON-RPC request to include in a batch"
    (%RpcRequest UFix String String))   ; id, method, params-json

  (declare make-rpc-request (UFix -> String -> String -> RpcRequest))
  (define (make-rpc-request id method params)
    "Create a batch RPC request with an ID, method name, and JSON params string"
    (%RpcRequest id method params))

  ;; A single result from a batch response
  (define-type BatchResult
    "A result from a batch JSON-RPC response"
    (BatchOk UFix String)        ; id, result-json
    (BatchErr UFix String))      ; id, error-message

  (declare batch-result-id (BatchResult -> UFix))
  (define (batch-result-id r)
    "Get the request ID from a batch result"
    (match r
      ((BatchOk id _) id)
      ((BatchErr id _) id)))

  (declare batch-result-value (BatchResult -> (types:Web3Result String)))
  (define (batch-result-value r)
    "Extract the result value, converting errors to Web3Result"
    (match r
      ((BatchOk _ val) (Ok val))
      ((BatchErr _ msg) (Err (types:ProviderError msg))))))

;;; =========================================================================
;;; CL-level batch call (must come after Coalton types are defined)
;;; =========================================================================

(cl:defun %format-rpc-error-val (error-val)
  "Format an RPC error value into a human-readable string."
  (cl:format cl:nil "RPC error: ~A"
             (cl:if (cl:listp error-val)
                    (cl:or (cl:cdr (cl:assoc :message error-val))
                           (cl:format cl:nil "~A" error-val))
                    (cl:format cl:nil "~A" error-val))))

(cl:defun %batch-call-impl (provider requests)
  "Execute batch JSON-RPC. Provider is a Coalton HttpProvider, requests is a Coalton list of RpcRequest."
  (cl:handler-case
      (cl:let* ((url (cl:slot-value provider 'web3/provider::|_0|))
                (cl-requests (%coalton-list-to-cl-triples requests))
                (raw-results (%batch-rpc-raw url cl-requests))
                (coalton-results cl:nil))
        (cl:dolist (triple (cl:reverse raw-results))
          (cl:let ((req-id (cl:first triple))
                   (error-val (cl:second triple))
                   (result-val (cl:third triple)))
            (cl:setf coalton-results
                     (cl:cons
                      (cl:if error-val
                             (BatchErr req-id (%format-rpc-error-val error-val))
                             (BatchOk req-id
                                      (cl:if (cl:stringp result-val)
                                             result-val
                                             (cl-json:encode-json-to-string result-val))))
                      coalton-results))))
        (Ok coalton-results))
    (cl:error (e)
      (Err (web3/types:ProviderError
            (cl:format cl:nil "Batch HTTP error: ~A" e))))))

;;; =========================================================================
;;; Coalton batch execution and convenience builders
;;; =========================================================================

(coalton-toplevel

  ;; =========================================================================
  ;; Core batch execution
  ;; =========================================================================

  (declare batch-call-raw (provider:HttpProvider -> (List RpcRequest) -> (types:Web3Result (List BatchResult))))
  (define (batch-call-raw provider requests)
    "Execute a batch of JSON-RPC requests and return raw results.
     Returns results sorted by request ID. Empty request list returns Ok Nil."
    (match requests
      ((Nil) (Ok Nil))
      (_ (lisp (types:Web3Result (List BatchResult)) (provider requests)
           (%batch-call-impl provider requests)))))

  (declare batch-call (provider:HttpProvider -> (List RpcRequest) -> (types:Web3Result (List (types:Web3Result String)))))
  (define (batch-call provider requests)
    "Execute a batch of JSON-RPC requests. Returns a list of Web3Result values
     in the same order as the input requests."
    (match (batch-call-raw provider requests)
      ((Err e) (Err e))
      ((Ok results) (Ok (map batch-result-value results)))))

  ;; =========================================================================
  ;; Convenience request builders
  ;; =========================================================================

  (declare batch-eth-chain-id (UFix -> RpcRequest))
  (define (batch-eth-chain-id id)
    "Create a batch request for eth_chainId"
    (make-rpc-request id "eth_chainId" "[]"))

  (declare batch-eth-block-number (UFix -> RpcRequest))
  (define (batch-eth-block-number id)
    "Create a batch request for eth_blockNumber"
    (make-rpc-request id "eth_blockNumber" "[]"))

  (declare batch-eth-get-balance (UFix -> addr:Address -> RpcRequest))
  (define (batch-eth-get-balance id address)
    "Create a batch request for eth_getBalance"
    (let ((addr-hex (addr:address-to-hex address)))
      (make-rpc-request id "eth_getBalance"
                        (lisp String (addr-hex)
                          (cl:format cl:nil "[\"~A\",\"latest\"]" addr-hex)))))

  (declare batch-eth-get-transaction-count (UFix -> addr:Address -> RpcRequest))
  (define (batch-eth-get-transaction-count id address)
    "Create a batch request for eth_getTransactionCount"
    (let ((addr-hex (addr:address-to-hex address)))
      (make-rpc-request id "eth_getTransactionCount"
                        (lisp String (addr-hex)
                          (cl:format cl:nil "[\"~A\",\"latest\"]" addr-hex)))))

  (declare batch-eth-gas-price (UFix -> RpcRequest))
  (define (batch-eth-gas-price id)
    "Create a batch request for eth_gasPrice"
    (make-rpc-request id "eth_gasPrice" "[]"))

  (declare batch-eth-call (UFix -> (Optional addr:Address) -> addr:Address -> types:Bytes -> RpcRequest))
  (define (batch-eth-call id from to data)
    "Create a batch request for eth_call"
    (let ((to-hex (addr:address-to-hex to))
          (data-hex (types:hex-encode-prefixed data)))
      (make-rpc-request id "eth_call"
                        (match from
                          ((Some from-addr)
                           (let ((from-hex (addr:address-to-hex from-addr)))
                             (lisp String (from-hex to-hex data-hex)
                               (cl:format cl:nil "[{\"from\":\"~A\",\"to\":\"~A\",\"data\":\"~A\"},\"latest\"]"
                                          from-hex to-hex data-hex))))
                          ((None)
                           (lisp String (to-hex data-hex)
                             (cl:format cl:nil "[{\"to\":\"~A\",\"data\":\"~A\"},\"latest\"]"
                                        to-hex data-hex)))))))

  (declare batch-eth-get-code (UFix -> addr:Address -> RpcRequest))
  (define (batch-eth-get-code id address)
    "Create a batch request for eth_getCode"
    (let ((addr-hex (addr:address-to-hex address)))
      (make-rpc-request id "eth_getCode"
                        (lisp String (addr-hex)
                          (cl:format cl:nil "[\"~A\",\"latest\"]" addr-hex)))))

  (declare batch-eth-get-storage-at (UFix -> addr:Address -> types:U256 -> RpcRequest))
  (define (batch-eth-get-storage-at id address slot)
    "Create a batch request for eth_getStorageAt"
    (let ((addr-hex (addr:address-to-hex address))
          (slot-hex (types:hex-encode-prefixed (types:u256-to-bytes slot))))
      (make-rpc-request id "eth_getStorageAt"
                        (lisp String (addr-hex slot-hex)
                          (cl:format cl:nil "[\"~A\",\"~A\",\"latest\"]" addr-hex slot-hex)))))

  (declare batch-eth-max-priority-fee-per-gas (UFix -> RpcRequest))
  (define (batch-eth-max-priority-fee-per-gas id)
    "Create a batch request for eth_maxPriorityFeePerGas"
    (make-rpc-request id "eth_maxPriorityFeePerGas" "[]")))
