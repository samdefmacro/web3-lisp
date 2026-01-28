(in-package #:web3/abi)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; ABI Decoding

  (declare abi-decode ((List AbiType) -> types:Bytes -> (types:Web3Result (List AbiValue))))
  (define (abi-decode type-list data)
    "Decode ABI-encoded data according to the given type descriptors"
    (%decode-tuple type-list data 0))

  (declare %decode-tuple ((List AbiType) -> types:Bytes -> UFix -> (types:Web3Result (List AbiValue))))
  (define (%decode-tuple type-list data base-offset)
    "Decode a tuple of values from data starting at base-offset"
    (match type-list
      ((Nil) (Ok Nil))
      ((Cons typ rest-types)
       (match (%decode-single typ data base-offset)
         ((Err e) (Err e))
         ((Ok val)
          (match (%decode-tuple rest-types data (+ base-offset 32))
            ((Err e) (Err e))
            ((Ok more-vals)
             (Ok (Cons val more-vals)))))))))

  (declare %decode-single (AbiType -> types:Bytes -> UFix -> (types:Web3Result AbiValue)))
  (define (%decode-single typ data offset)
    "Decode a single value from the given offset"
    (match typ
      ((AbiUint _)
       (if (> (+ offset 32) (types:bytes-length data))
           (Err (types:AbiError "Data too short for uint"))
           (let ((word (types:bytes-slice offset 32 data)))
             (match (types:u256-from-bytes word)
               ((Err e) (Err e))
               ((Ok u) (Ok (AbiUintVal u)))))))

      ((AbiInt _)
       (if (> (+ offset 32) (types:bytes-length data))
           (Err (types:AbiError "Data too short for int"))
           (Ok (AbiIntVal
                (lisp Integer (data offset)
                  (cl:let ((val 0))
                    (cl:loop :for i :from 0 :below 32
                             :do (cl:setf val (cl:+ (cl:ash val 8)
                                                    (cl:aref data (cl:+ offset i)))))
                    ;; Convert from unsigned to signed (two's complement)
                    (cl:if (cl:logbitp 255 val)
                           (cl:- val (cl:ash 1 256))
                           val)))))))

      ((AbiAddress)
       (if (> (+ offset 32) (types:bytes-length data))
           (Err (types:AbiError "Data too short for address"))
           ;; Address is in the last 20 bytes of the 32-byte word
           (Ok (AbiAddressVal (types:bytes-slice (+ offset 12) 20 data)))))

      ((AbiBool)
       (if (> (+ offset 32) (types:bytes-length data))
           (Err (types:AbiError "Data too short for bool"))
           (let ((b (types:bytes-ref-unsafe (+ offset 31) data)))
             (Ok (AbiBoolVal (/= b 0))))))

      ((AbiBytesFixed n)
       (if (> (+ offset 32) (types:bytes-length data))
           (Err (types:AbiError "Data too short for bytesN"))
           (Ok (AbiBytesFixedVal (types:bytes-slice offset n data)))))

      ((AbiBytes)
       ;; Dynamic: read offset, then decode at that location
       (%decode-dynamic-bytes data offset))

      ((AbiString)
       ;; Dynamic: same encoding as bytes
       (match (%decode-dynamic-bytes data offset)
         ((Err e) (Err e))
         ((Ok (AbiBytesVal bytes-data))
          (Ok (AbiStringVal
               (lisp String (bytes-data)
                 (sb-ext:octets-to-string
                  (cl:make-array (cl:length bytes-data)
                                 :element-type '(cl:unsigned-byte 8)
                                 :initial-contents bytes-data)
                  :external-format :utf-8)))))
         ((Ok _) (Err (types:AbiError "Unexpected decode result for string")))))

      ((AbiArray elem-type)
       ;; Dynamic array: read offset, then length, then elements
       (let ((data-offset (%read-uint256-as-ufix data offset)))
         (let ((count (%read-uint256-as-ufix data data-offset)))
           (match (%decode-array-elements elem-type data (+ data-offset 32) count)
             ((Err e) (Err e))
             ((Ok items) (Ok (AbiArrayVal items)))))))

      ((AbiFixedArray elem-type count)
       (let ((result
               (if (abi-type-dynamic? elem-type)
                   ;; Dynamic elements: head contains offsets
                   (let ((data-offset (%read-uint256-as-ufix data offset)))
                     (%decode-tuple
                      (lisp (List AbiType) (elem-type count)
                        (cl:loop :repeat count :collect elem-type))
                      data data-offset))
                   ;; Static elements: directly encoded
                   (%decode-array-elements elem-type data offset count))))
         (match result
           ((Err e) (Err e))
           ((Ok items) (Ok (AbiFixedArrayVal items))))))

      ((AbiTuple components)
       (let ((result
               (if (abi-type-dynamic? typ)
                   (let ((data-offset (%read-uint256-as-ufix data offset)))
                     (%decode-tuple components data data-offset))
                   (%decode-tuple components data offset))))
         (match result
           ((Err e) (Err e))
           ((Ok items) (Ok (AbiTupleVal items))))))))

  (declare %decode-dynamic-bytes (types:Bytes -> UFix -> (types:Web3Result AbiValue)))
  (define (%decode-dynamic-bytes data offset)
    "Decode dynamic bytes: read offset pointer, then length + data"
    (let ((data-offset (%read-uint256-as-ufix data offset)))
      (let ((byte-len (%read-uint256-as-ufix data data-offset)))
        (if (> (+ data-offset (+ 32 byte-len)) (types:bytes-length data))
            (Err (types:AbiError "Data too short for dynamic bytes"))
            (Ok (AbiBytesVal (types:bytes-slice (+ data-offset 32) byte-len data)))))))

  (declare %decode-array-elements (AbiType -> types:Bytes -> UFix -> UFix ->
                                   (types:Web3Result (List AbiValue))))
  (define (%decode-array-elements elem-type data start count)
    "Decode count array elements starting at offset"
    (if (== count 0)
        (Ok Nil)
        (match (%decode-single elem-type data start)
          ((Err e) (Err e))
          ((Ok val)
           (match (%decode-array-elements elem-type data (+ start 32) (- count 1))
             ((Err e) (Err e))
             ((Ok rest-vals) (Ok (Cons val rest-vals))))))))

  (declare %read-uint256-as-ufix (types:Bytes -> UFix -> UFix))
  (define (%read-uint256-as-ufix data offset)
    "Read a 32-byte big-endian uint256 as a UFix (truncated)"
    (lisp UFix (data offset)
      (cl:let ((result 0))
        (cl:loop :for i :from 0 :below 32
                 :do (cl:setf result (cl:+ (cl:ash result 8)
                                           (cl:aref data (cl:+ offset i)))))
        result))))
