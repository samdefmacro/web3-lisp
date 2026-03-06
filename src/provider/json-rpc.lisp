(in-package #:web3/provider)
(named-readtables:in-readtable coalton:coalton)

;;; Shared CL helper for JSON-RPC HTTP calls

(cl:defun %format-rpc-error (error-val)
  "Extract a human-readable error message from a JSON-RPC error value."
  (cl:format cl:nil "RPC error: ~A"
             (cl:if (cl:listp error-val)
                    (cl:or (cl:cdr (cl:assoc :message error-val))
                           (cl:format cl:nil "~A" error-val))
                    (cl:format cl:nil "~A" error-val))))

(cl:defun %json-rpc-raw (url method params)
  "Make a JSON-RPC HTTP call and return (values error-val result-pair result-val).
   Signals CL errors on HTTP/parse failures for the caller to handle."
  (cl:let* ((request-body
              (cl:format cl:nil
                         "{\"jsonrpc\":\"2.0\",\"method\":\"~A\",\"params\":~A,\"id\":1}"
                         method params))
            (response
              (dexador:post url
                            :content request-body
                            :headers '(("Content-Type" . "application/json"))))
            (json (cl-json:decode-json-from-string response))
            (error-val (cl:cdr (cl:assoc :error json)))
            (result-pair (cl:assoc :result json))
            (result-val (cl:cdr result-pair)))
    (cl:values error-val result-pair result-val)))

(cl:defun %stringify-result (result-val)
  "Convert a JSON result value to a string."
  (cl:if (cl:stringp result-val)
         result-val
         (cl-json:encode-json-to-string result-val)))

(coalton-toplevel

  ;;; HTTP Provider type

  (define-type HttpProvider
    "JSON-RPC HTTP provider"
    (%HttpProvider String))   ; URL

  (declare make-http-provider (String -> HttpProvider))
  (define (make-http-provider url)
    "Create a new HTTP provider with the given RPC URL"
    (%HttpProvider url))

  ;;; JSON-RPC call (low-level)

  (declare json-rpc-call (HttpProvider -> String -> String -> (types:Web3Result String)))
  (define (json-rpc-call provider method params)
    "Make a JSON-RPC call and return the result as a JSON string"
    (match provider
      ((%HttpProvider url)
       (lisp (types:Web3Result String) (url method params)
         (cl:handler-case
             (cl:multiple-value-bind (error-val result-pair result-val)
                 (%json-rpc-raw url method params)
               (cl:declare (cl:ignore result-pair))
               (cl:if error-val
                      (coalton-prelude:Err
                       (types:ProviderError (%format-rpc-error error-val)))
                      (cl:if result-val
                             (coalton-prelude:Ok (%stringify-result result-val))
                             (coalton-prelude:Err
                              (types:ProviderError "No result in RPC response")))))
           (cl:error (e)
             (coalton-prelude:Err
              (types:ProviderError (cl:format cl:nil "HTTP error: ~A" e)))))))))

  ;;; Nullable JSON-RPC call (for methods that may return null)

  (declare json-rpc-call-nullable (HttpProvider -> String -> String -> (types:Web3Result (Optional String))))
  (define (json-rpc-call-nullable provider method params)
    "Make a JSON-RPC call where null is a valid result. Returns None for null/false results."
    (match provider
      ((%HttpProvider url)
       (lisp (types:Web3Result (Optional String)) (url method params)
         (cl:handler-case
             (cl:multiple-value-bind (error-val result-pair result-val)
                 (%json-rpc-raw url method params)
               (cl:cond
                 (error-val
                  (coalton-prelude:Err
                   (types:ProviderError (%format-rpc-error error-val))))
                 ((cl:and result-pair result-val)
                  (coalton-prelude:Ok
                   (coalton-prelude:Some (%stringify-result result-val))))
                 (result-pair
                  ;; Result key present but value is null/false
                  (coalton-prelude:Ok
                   (coalton:coalton (the (Optional coalton:String) None))))
                 (cl:t
                  (coalton-prelude:Err
                   (types:ProviderError "No result in RPC response")))))
           (cl:error (e)
             (coalton-prelude:Err
              (types:ProviderError (cl:format cl:nil "HTTP error: ~A" e))))))))))
