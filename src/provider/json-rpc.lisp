(in-package #:web3/provider)
(named-readtables:in-readtable coalton:coalton)

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
                       (result-val (cl:cdr (cl:assoc :result json))))
               (cl:if error-val
                      (coalton-prelude:Err
                       (types:ProviderError
                        (cl:format cl:nil "RPC error: ~A"
                                   (cl:if (cl:listp error-val)
                                          (cl:or (cl:cdr (cl:assoc :message error-val))
                                                 (cl:format cl:nil "~A" error-val))
                                          (cl:format cl:nil "~A" error-val)))))
                      (cl:if result-val
                             (coalton-prelude:Ok
                              (cl:if (cl:stringp result-val)
                                     result-val
                                     (cl-json:encode-json-to-string result-val)))
                             (coalton-prelude:Err
                              (types:ProviderError "No result in RPC response")))))
           (cl:error (e)
             (coalton-prelude:Err
              (types:ProviderError (cl:format cl:nil "HTTP error: ~A" e))))))))))
