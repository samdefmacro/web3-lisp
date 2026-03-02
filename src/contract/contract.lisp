;;;; Contract Abstraction implementation
;;;; High-level interface for smart contract interactions

(in-package #:web3/contract)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Contract Type
  ;;; =========================================================================

  (define-struct Contract
    "A smart contract instance with parsed ABI"
    (contract-address addr:Address)
    (contract-abi (List abi-parser:AbiItem)))

  (declare make-contract (addr:Address -> (List abi-parser:AbiItem) -> Contract))
  (define (make-contract address abi-items)
    "Create a contract instance from address and parsed ABI items"
    (Contract address abi-items))

  (declare contract-from-abi-json (addr:Address -> String -> (types:Web3Result Contract)))
  (define (contract-from-abi-json address abi-json)
    "Create a contract instance from address and ABI JSON string"
    (match (abi-parser:parse-abi-json abi-json)
      ((Ok items) (Ok (Contract address items)))
      ((Err e) (Err e))))

  ;;; =========================================================================
  ;;; Function/Event Lookup
  ;;; =========================================================================

  (declare get-function (Contract -> String -> (Optional abi-parser:ParsedFunction)))
  (define (get-function contract name)
    "Get a function from the contract ABI by name"
    (find-function-in-items (.contract-abi contract) name))

  (declare find-function-in-items ((List abi-parser:AbiItem) -> String -> (Optional abi-parser:ParsedFunction)))
  (define (find-function-in-items items name)
    "Search for a function by name in ABI items"
    (match items
      ((Nil) None)
      ((Cons item rest)
       (match item
         ((abi-parser:AbiFunction fn)
          (if (lisp Boolean (fn name)
                (cl:string= (coalton:coalton
                             (abi-parser:.fn-name
                              (coalton:lisp abi-parser:ParsedFunction () fn)))
                            name))
              (Some fn)
              (find-function-in-items rest name)))
         (_ (find-function-in-items rest name))))))

  (declare get-event (Contract -> String -> (Optional abi-parser:ParsedEvent)))
  (define (get-event contract name)
    "Get an event from the contract ABI by name"
    (find-event-in-items (.contract-abi contract) name))

  (declare find-event-in-items ((List abi-parser:AbiItem) -> String -> (Optional abi-parser:ParsedEvent)))
  (define (find-event-in-items items name)
    "Search for an event by name in ABI items"
    (match items
      ((Nil) None)
      ((Cons item rest)
       (match item
         ((abi-parser:AbiEvent ev)
          (if (lisp Boolean (ev name)
                (cl:string= (coalton:coalton
                             (abi-parser:.event-name
                              (coalton:lisp abi-parser:ParsedEvent () ev)))
                            name))
              (Some ev)
              (find-event-in-items rest name)))
         (_ (find-event-in-items rest name))))))

  (declare get-function-by-selector (Contract -> types:Bytes -> (Optional abi-parser:ParsedFunction)))
  (define (get-function-by-selector contract selector)
    "Get a function from the contract ABI by its 4-byte selector"
    (find-function-by-selector-in-items (.contract-abi contract) selector))

  (declare find-function-by-selector-in-items ((List abi-parser:AbiItem) -> types:Bytes -> (Optional abi-parser:ParsedFunction)))
  (define (find-function-by-selector-in-items items selector)
    "Search for a function by selector in ABI items"
    (match items
      ((Nil) None)
      ((Cons item rest)
       (match item
         ((abi-parser:AbiFunction fn)
          (if (bytes-prefix-equal? (abi-parser:.fn-selector fn) selector 4)
              (Some fn)
              (find-function-by-selector-in-items rest selector)))
         (_ (find-function-by-selector-in-items rest selector))))))

  (declare get-event-by-topic (Contract -> types:Bytes -> (Optional abi-parser:ParsedEvent)))
  (define (get-event-by-topic contract topic)
    "Get an event from the contract ABI by its topic0 (32-byte keccak hash)"
    (find-event-by-topic-in-items (.contract-abi contract) topic))

  (declare find-event-by-topic-in-items ((List abi-parser:AbiItem) -> types:Bytes -> (Optional abi-parser:ParsedEvent)))
  (define (find-event-by-topic-in-items items topic)
    "Search for an event by topic in ABI items"
    (match items
      ((Nil) None)
      ((Cons item rest)
       (match item
         ((abi-parser:AbiEvent ev)
          (if (bytes-equal? (abi-parser:.event-topic ev) topic)
              (Some ev)
              (find-event-by-topic-in-items rest topic)))
         (_ (find-event-by-topic-in-items rest topic))))))

  ;; Byte comparison helpers
  (declare bytes-equal? (types:Bytes -> types:Bytes -> Boolean))
  (define (bytes-equal? a b)
    "Check if two byte arrays are equal"
    (lisp Boolean (a b)
      (cl:if (cl:equalp a b) cl:t cl:nil)))

  (declare bytes-prefix-equal? (types:Bytes -> types:Bytes -> UFix -> Boolean))
  (define (bytes-prefix-equal? a b len)
    "Check if first len bytes of two arrays are equal"
    (lisp Boolean (a b len)
      (cl:if (cl:and (cl:>= (cl:length a) len)
                     (cl:>= (cl:length b) len)
                     (cl:loop :for i :below len
                              :always (cl:= (cl:aref a i) (cl:aref b i))))
             cl:t cl:nil)))

  ;;; =========================================================================
  ;;; List Functions/Events
  ;;; =========================================================================

  (declare list-functions (Contract -> (List String)))
  (define (list-functions contract)
    "Get list of all function names in the contract ABI"
    (collect-function-names (.contract-abi contract)))

  (declare collect-function-names ((List abi-parser:AbiItem) -> (List String)))
  (define (collect-function-names items)
    (match items
      ((Nil) Nil)
      ((Cons item rest)
       (match item
         ((abi-parser:AbiFunction fn)
          (Cons (abi-parser:.fn-name fn) (collect-function-names rest)))
         (_ (collect-function-names rest))))))

  (declare list-events (Contract -> (List String)))
  (define (list-events contract)
    "Get list of all event names in the contract ABI"
    (collect-event-names (.contract-abi contract)))

  (declare collect-event-names ((List abi-parser:AbiItem) -> (List String)))
  (define (collect-event-names items)
    (match items
      ((Nil) Nil)
      ((Cons item rest)
       (match item
         ((abi-parser:AbiEvent ev)
          (Cons (abi-parser:.event-name ev) (collect-event-names rest)))
         (_ (collect-event-names rest))))))

  ;;; =========================================================================
  ;;; Encoding Functions
  ;;; =========================================================================

  (declare encode-function-call (abi-parser:ParsedFunction -> (List abi:AbiValue) -> types:Bytes))
  (define (encode-function-call fn args)
    "Encode a function call with arguments"
    (abi-parser:encode-function-call fn args))

  (declare encode-function-call-by-name (Contract -> String -> (List abi:AbiValue) -> (types:Web3Result types:Bytes)))
  (define (encode-function-call-by-name contract fn-name args)
    "Encode a function call by function name"
    (match (get-function contract fn-name)
      ((None) (Err (types:AbiError
                    (lisp String (fn-name)
                      (cl:format cl:nil "Function not found: ~A" fn-name)))))
      ((Some fn) (Ok (encode-function-call fn args)))))

  ;;; =========================================================================
  ;;; Decoding Functions
  ;;; =========================================================================

  (declare decode-function-output (abi-parser:ParsedFunction -> types:Bytes -> (types:Web3Result (List abi:AbiValue))))
  (define (decode-function-output fn data)
    "Decode function return values"
    (abi-parser:decode-function-output fn data))

  (declare decode-function-output-by-name (Contract -> String -> types:Bytes -> (types:Web3Result (List abi:AbiValue))))
  (define (decode-function-output-by-name contract fn-name data)
    "Decode function return values by function name"
    (match (get-function contract fn-name)
      ((None) (Err (types:AbiError
                    (lisp String (fn-name)
                      (cl:format cl:nil "Function not found: ~A" fn-name)))))
      ((Some fn) (decode-function-output fn data))))

  (declare decode-event (abi-parser:ParsedEvent -> (List types:Bytes) -> types:Bytes -> (types:Web3Result (List abi:AbiValue))))
  (define (decode-event ev topics data)
    "Decode event log data"
    (abi-parser:decode-event-log ev topics data))

  (declare decode-event-by-topic (Contract -> types:Bytes -> (List types:Bytes) -> types:Bytes -> (types:Web3Result (List abi:AbiValue))))
  (define (decode-event-by-topic contract topic0 topics data)
    "Decode event log by topic0"
    (match (get-event-by-topic contract topic0)
      ((None) (Err (types:AbiError "Event not found for topic")))
      ((Some ev) (decode-event ev topics data))))

  ;;; =========================================================================
  ;;; Call Builder - Fluent interface for building calls
  ;;; =========================================================================

  (define-struct CallBuilder
    "Builder for constructing function calls"
    (builder-contract Contract)
    (builder-function abi-parser:ParsedFunction)
    (builder-args (List abi:AbiValue)))

  (declare call-builder (Contract -> String -> (types:Web3Result CallBuilder)))
  (define (call-builder contract fn-name)
    "Start building a function call"
    (match (get-function contract fn-name)
      ((None) (Err (types:AbiError
                    (lisp String (fn-name)
                      (cl:format cl:nil "Function not found: ~A" fn-name)))))
      ((Some fn) (Ok (CallBuilder contract fn Nil)))))

  (declare with-arg (CallBuilder -> abi:AbiValue -> CallBuilder))
  (define (with-arg builder arg)
    "Add an argument to the call builder (prepends; reversed at build time)"
    (CallBuilder (.builder-contract builder)
                 (.builder-function builder)
                 (Cons arg (.builder-args builder))))

  (declare build-calldata (CallBuilder -> types:Bytes))
  (define (build-calldata builder)
    "Build the calldata from the builder"
    (encode-function-call (.builder-function builder) (reverse (.builder-args builder))))

  ;;; =========================================================================
  ;;; Call Request - For eth_call
  ;;; =========================================================================

  (define-struct CallRequest
    "A request for eth_call"
    (call-to addr:Address)
    (call-data types:Bytes))

  (declare build-call-request (CallBuilder -> CallRequest))
  (define (build-call-request builder)
    "Build a CallRequest from the builder"
    (CallRequest (.contract-address (.builder-contract builder))
                 (build-calldata builder))))

