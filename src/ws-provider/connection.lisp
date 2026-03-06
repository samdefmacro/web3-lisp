;;;; WebSocket I/O Layer
;;;; Actual WebSocket connection, subscription management, and message dispatch

(cl:in-package #:web3/ws-provider)

;;; =========================================================================
;;; Pending Request (request/response synchronization across threads)
;;; =========================================================================

(cl:defstruct ws-pending-request
  "A request awaiting a response from the server"
  (lock (bt2:make-lock :name "ws-pending"))
  (cv (bt2:make-condition-variable :name "ws-pending"))
  (result cl:nil)
  (done-p cl:nil))

;;; =========================================================================
;;; WebSocket Provider
;;; =========================================================================

(cl:defstruct ws-provider
  "WebSocket provider for real-time Ethereum subscriptions.
   Wraps websocket-driver client with subscription management."
  (client cl:nil)                                               ; websocket-driver client
  (state cl:nil)                                                ; ws-connection-state
  (handlers (cl:make-hash-table :test 'cl:equal))               ; sub-id → handler fn
  (pending (cl:make-hash-table :test 'cl:eql))                  ; request-id → ws-pending-request
  (lock (bt2:make-lock :name "ws-provider"))
  (connected-p cl:nil))

;;; =========================================================================
;;; Message Dispatch (called from websocket-driver read thread)
;;; =========================================================================

(cl:defun %ws-dispatch-message (provider msg)
  "Handle an incoming WebSocket text message.
   Routes responses to pending requests and notifications to subscription handlers."
  (cl:handler-case
      (cl:let* ((parsed (cl-json:decode-json-from-string msg))
                (msg-id (cl:cdr (cl:assoc :id parsed)))
                (rpc-method (cl:cdr (cl:assoc :method parsed)))
                (rpc-result (cl:cdr (cl:assoc :result parsed)))
                (error-val (cl:cdr (cl:assoc :error parsed)))
                (rpc-params (cl:cdr (cl:assoc :params parsed))))
        (cl:cond
          ;; JSON-RPC response (has id field) → resolve pending request
          (msg-id
           (cl:let ((req (bt2:with-lock-held ((ws-provider-lock provider))
                           (cl:gethash msg-id (ws-provider-pending provider)))))
             (cl:when req
               (bt2:with-lock-held ((ws-pending-request-lock req))
                 (cl:setf (ws-pending-request-result req)
                          (cl:if error-val
                                 (cl:list :error
                                          (cl:or (cl:cdr (cl:assoc :message error-val))
                                                 (cl:format cl:nil "~A" error-val)))
                                 (cl:list :ok rpc-result)))
                 (cl:setf (ws-pending-request-done-p req) cl:t)
                 (bt2:condition-notify (ws-pending-request-cv req))))))

          ;; Subscription notification (method = eth_subscription)
          ((cl:and rpc-method (cl:string= rpc-method "eth_subscription") rpc-params)
           (cl:let ((sub-id (cl:cdr (cl:assoc :subscription rpc-params)))
                    (sub-result (cl:cdr (cl:assoc :result rpc-params))))
             (cl:when (cl:and sub-id sub-result)
               (cl:let ((handler (bt2:with-lock-held ((ws-provider-lock provider))
                                   (cl:gethash sub-id (ws-provider-handlers provider)))))
                 (cl:when handler
                   (cl:handler-case
                       (cl:funcall handler (cl-json:encode-json-to-string sub-result))
                     (cl:error (e)
                       (cl:format cl:*error-output* "~&ws-provider: subscription handler error: ~A~%" e))))))))))
    (cl:error (e)
      (cl:format cl:*error-output* "~&ws-provider: dispatch error: ~A~%" e))))

;;; =========================================================================
;;; Send and Wait (synchronous request/response over async WebSocket)
;;; =========================================================================

(cl:defun %ws-send-and-wait (provider request request-id cl:&key (timeout 10))
  "Send a JSON-RPC request and block until the response arrives.
   Returns (:ok result) or (:error message)."
  (cl:unless (ws-provider-connected-p provider)
    (cl:return-from %ws-send-and-wait
      (cl:list :error "WebSocket not connected")))
  (cl:let ((req (make-ws-pending-request)))
    ;; Register pending request
    (bt2:with-lock-held ((ws-provider-lock provider))
      (cl:setf (cl:gethash request-id (ws-provider-pending provider)) req))
    (cl:unwind-protect
        (cl:progn
          ;; Send the request
          (websocket-driver:send-text (ws-provider-client provider) request)
          ;; Wait for response with timeout
          (bt2:with-lock-held ((ws-pending-request-lock req))
            (cl:let ((deadline (cl:+ (cl:get-internal-real-time)
                                     (cl:* timeout cl:internal-time-units-per-second))))
              (cl:loop :until (ws-pending-request-done-p req)
                       :do (cl:let ((remaining (cl:/ (cl:- deadline (cl:get-internal-real-time))
                                                     cl:internal-time-units-per-second)))
                             (cl:when (cl:<= remaining 0)
                               (cl:return-from %ws-send-and-wait
                                 (cl:list :error "Request timed out")))
                             (bt2:condition-wait (ws-pending-request-cv req)
                                                 (ws-pending-request-lock req)
                                                 :timeout remaining)))))
          (ws-pending-request-result req))
      ;; Cleanup
      (bt2:with-lock-held ((ws-provider-lock provider))
        (cl:remhash request-id (ws-provider-pending provider))))))

;;; =========================================================================
;;; Public API - Connection
;;; =========================================================================

(cl:defun ws-connect (url cl:&key (on-error cl:nil) (on-close cl:nil))
  "Connect to an Ethereum WebSocket endpoint (ws:// or wss://).
   Returns a ws-provider struct.

   Optional callbacks (called from the WebSocket read thread):
     on-error: (lambda (message-string))
     on-close: (lambda (&key code reason))

   Example:
     (ws-connect \"wss://mainnet.infura.io/ws/v3/YOUR-KEY\")"
  (cl:let* ((state (make-ws-connection-state :url url))
            (provider (make-ws-provider :state state))
            (client (websocket-driver:make-client url)))
    (cl:setf (ws-provider-client provider) client)

    ;; Connection opened
    (websocket-driver:on :open client
      (cl:lambda ()
        (cl:setf (ws-provider-connected-p provider) cl:t)))

    ;; Incoming messages → dispatch
    (websocket-driver:on :message client
      (cl:lambda (msg)
        (%ws-dispatch-message provider msg)))

    ;; Errors
    (websocket-driver:on :error client
      (cl:lambda (err)
        (cl:when on-error
          (cl:funcall on-error (cl:format cl:nil "~A" err)))))

    ;; Connection closed
    (websocket-driver:on :close client
      (cl:lambda (cl:&key code reason)
        (cl:setf (ws-provider-connected-p provider) cl:nil)
        (cl:when on-close
          (cl:funcall on-close :code code :reason reason))))

    ;; Connect (blocks until WebSocket handshake completes)
    (websocket-driver:start-connection client)
    provider))

(cl:defun ws-close (provider)
  "Close the WebSocket connection and clean up resources."
  (cl:let ((client (ws-provider-client provider)))
    (cl:when client
      (cl:handler-case (websocket-driver:close-connection client)
        (cl:error (e) (cl:declare (cl:ignore e))))))
  (cl:setf (ws-provider-connected-p provider) cl:nil)
  (cl:values))

;;; =========================================================================
;;; Public API - Subscriptions
;;; =========================================================================

(cl:defun ws-subscribe (provider sub-type handler cl:&key (timeout 10))
  "Subscribe to Ethereum events over WebSocket.

   provider:  ws-provider from ws-connect
   sub-type:  Coalton SubscriptionType (SubNewHeads, SubLogs, etc.)
   handler:   (lambda (json-string)) called for each notification
   timeout:   seconds to wait for confirmation (default 10)

   Returns the subscription ID string.
   Signals an error on failure.

   Example:
     (ws-subscribe provider
       (coalton:coalton web3/ws-provider:SubNewHeads)
       (lambda (json)
         (format t \"New block: ~A~%\" json)))"
  (cl:let* ((state (ws-provider-state provider))
            (request-id (ws-state-next-request-id state))
            (request (coalton:coalton
                       (encode-subscribe-request
                         (coalton:lisp coalton:UFix () request-id)
                         (coalton:lisp SubscriptionType () sub-type)))))
    (cl:let ((response (%ws-send-and-wait provider request request-id :timeout timeout)))
      (cl:destructuring-bind (status value) response
        (cl:ecase status
          (:ok
           (cl:let ((sub-id value))
             ;; Track subscription in state
             (ws-state-add-subscription state sub-id sub-type)
             ;; Register notification handler
             (bt2:with-lock-held ((ws-provider-lock provider))
               (cl:setf (cl:gethash sub-id (ws-provider-handlers provider)) handler))
             sub-id))
          (:error
           (cl:error "Subscribe failed: ~A" value)))))))

(cl:defun ws-unsubscribe (provider sub-id cl:&key (timeout 10))
  "Unsubscribe from a WebSocket subscription.
   Returns T on success, NIL on failure."
  (cl:let* ((state (ws-provider-state provider))
            (request-id (ws-state-next-request-id state))
            (request (coalton:coalton
                       (encode-unsubscribe-request
                         (coalton:lisp coalton:UFix () request-id)
                         (coalton:lisp coalton:String () sub-id)))))
    (cl:let ((response (%ws-send-and-wait provider request request-id :timeout timeout)))
      (cl:destructuring-bind (status value) response
        (cl:ecase status
          (:ok
           (ws-state-remove-subscription state sub-id)
           (bt2:with-lock-held ((ws-provider-lock provider))
             (cl:remhash sub-id (ws-provider-handlers provider)))
           (cl:eq value cl:t))
          (:error cl:nil))))))
