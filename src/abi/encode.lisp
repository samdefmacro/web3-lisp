(in-package #:web3/abi)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; ABI Encoding (Solidity ABI specification)
  ;;; Each value is encoded as a 32-byte word or a sequence of words
  ;;; Dynamic types use a head/tail encoding scheme

  (declare abi-encode ((List AbiValue) -> types:Bytes))
  (define (abi-encode values)
    "ABI-encode a list of values"
    (%encode-tuple values))

  (declare abi-encode-with-selector (types:Bytes -> (List AbiValue) -> types:Bytes))
  (define (abi-encode-with-selector selector values)
    "ABI-encode with a 4-byte function selector prefix"
    (types:bytes-append (types:bytes-take 4 selector)
                        (abi-encode values)))

  ;;; Internal encoding

  (declare %encode-tuple ((List AbiValue) -> types:Bytes))
  (define (%encode-tuple values)
    "Encode a tuple of values using head/tail encoding"
    (let ((n (list:length values)))
      (if (== n 0)
          types:bytes-empty
          ;; Calculate head size: each value gets 32 bytes in the head
          ;; (either the value itself for static, or an offset for dynamic)
          (let ((head-size (* n 32)))
            ;; First pass: encode all values and collect their encoded forms
            (let ((encoded-values (map %encode-value values))
                  (is-dynamic (map %value-is-dynamic? values)))
              ;; Build head and tail
              (lisp types:Bytes (encoded-values is-dynamic head-size)
                (cl:let* ((head-parts cl:nil)
                          (tail-parts cl:nil)
                          (tail-offset head-size)
                          (enc-list encoded-values)
                          (dyn-list is-dynamic))
                  ;; Process each value
                  (cl:loop :while enc-list
                           :for enc := (cl:first enc-list)
                           :for dyn := (cl:first dyn-list)
                           :do (cl:if (cl:eq dyn coalton:True)
                                      ;; Dynamic: head = offset, tail = encoded data
                                      (cl:progn
                                        (cl:push (%encode-uint256-integer tail-offset) head-parts)
                                        (cl:push enc tail-parts)
                                        (cl:incf tail-offset (cl:length enc)))
                                      ;; Static: head = encoded data (padded to 32), no tail
                                      (cl:push (cl:let ((padded (cl:make-array 32 :fill-pointer 32
                                                                                  :adjustable cl:t
                                                                                  :initial-element 0)))
                                                 (cl:loop :for i :from 0 :below (cl:min 32 (cl:length enc))
                                                          :do (cl:setf (cl:aref padded i) (cl:aref enc i)))
                                                 padded)
                                               head-parts))
                               (cl:setf enc-list (cl:rest enc-list))
                               (cl:setf dyn-list (cl:rest dyn-list)))
                  ;; Concatenate head (reversed) + tail (reversed)
                  (cl:let* ((all-parts (cl:nconc (cl:nreverse head-parts)
                                                 (cl:nreverse tail-parts)))
                            (total-len (cl:reduce #'cl:+ all-parts :key #'cl:length))
                            (result (cl:make-array total-len :fill-pointer total-len
                                                             :adjustable cl:t))
                            (pos 0))
                    (cl:dolist (part all-parts)
                      (cl:loop :for i :from 0 :below (cl:length part)
                               :do (cl:setf (cl:aref result pos) (cl:aref part i))
                                   (cl:incf pos)))
                    result))))))))

  (declare %value-is-dynamic? (AbiValue -> Boolean))
  (define (%value-is-dynamic? val)
    "Check if a value requires dynamic encoding"
    (match val
      ((AbiUintVal _) False)
      ((AbiIntVal _) False)
      ((AbiAddressVal _) False)
      ((AbiBoolVal _) False)
      ((AbiBytesFixedVal _) False)
      ((AbiBytesVal _) True)
      ((AbiStringVal _) True)
      ((AbiArrayVal _) True)
      ((AbiFixedArrayVal items)
       (match items
         ((Nil) False)
         ((Cons first _) (%value-is-dynamic? first))))
      ((AbiTupleVal items)
       (list:any %value-is-dynamic? items))))

  (declare %encode-value (AbiValue -> types:Bytes))
  (define (%encode-value val)
    "Encode a single ABI value"
    (match val
      ((AbiUintVal u256)
       ;; uint256: big-endian, left-padded to 32 bytes
       (types:u256-to-bytes u256))

      ((AbiIntVal n)
       ;; int256: two's complement, left-padded to 32 bytes
       (lisp types:Bytes (n)
         (cl:let ((result (cl:make-array 32 :fill-pointer 32 :adjustable cl:t
                                            :initial-element (cl:if (cl:minusp n) #xff 0))))
           ;; Store the absolute value in two's complement
           (cl:let ((val (cl:if (cl:minusp n)
                                (cl:+ (cl:ash 1 256) n)
                                n)))
             (cl:loop :for i :from 0 :below 32
                      :do (cl:setf (cl:aref result (cl:- 31 i))
                                   (cl:ldb (cl:byte 8 (cl:* i 8)) val))))
           result)))

      ((AbiAddressVal addr-bytes)
       ;; address: right-aligned in 32 bytes
       (types:bytes-pad-left 32 addr-bytes))

      ((AbiBoolVal b)
       ;; bool: 0 or 1 in 32 bytes
       (let ((result (types:make-bytes 32)))
         (if b
             (progn
               (types:bytes-set! 31 1 result)
               result)
             result)))

      ((AbiBytesFixedVal data)
       ;; bytesN: right-padded to 32 bytes
       (types:bytes-pad-right 32 data))

      ((AbiBytesVal data)
       ;; bytes: length (32 bytes) + data (padded to 32-byte boundary)
       (let ((data-len (types:bytes-length data)))
         (let ((len-encoded (%encode-uint256-ufix data-len))
               (padded-data (types:bytes-pad-right (%ceil-to-32 data-len) data)))
           (types:bytes-append len-encoded padded-data))))

      ((AbiStringVal s)
       ;; string: same as bytes
       (%encode-value
        (AbiBytesVal
         (lisp types:Bytes (s)
           (cl:let* ((octets (sb-ext:string-to-octets s :external-format :utf-8))
                     (result (cl:make-array (cl:length octets)
                                            :fill-pointer (cl:length octets)
                                            :adjustable cl:t
                                            :initial-contents octets)))
             result)))))

      ((AbiArrayVal items)
       ;; T[]: length + encoded elements
       (let ((len-encoded (%encode-uint256-ufix (list:length items)))
             (elements-encoded (%encode-tuple items)))
         (types:bytes-append len-encoded elements-encoded)))

      ((AbiFixedArrayVal items)
       ;; T[N]: just encoded elements (no length prefix)
       (%encode-tuple items))

      ((AbiTupleVal items)
       (%encode-tuple items))))

  ;;; Helpers

  (declare %encode-uint256-ufix (UFix -> types:Bytes))
  (define (%encode-uint256-ufix n)
    "Encode a UFix as a big-endian 32-byte uint256"
    (lisp types:Bytes (n)
      (cl:let ((result (cl:make-array 32 :fill-pointer 32 :adjustable cl:t
                                         :initial-element 0)))
        (cl:loop :for i :from 0 :below (cl:min 8 32)
                 :do (cl:setf (cl:aref result (cl:- 31 i))
                              (cl:ldb (cl:byte 8 (cl:* i 8)) n)))
        result)))

  (declare %ceil-to-32 (UFix -> UFix))
  (define (%ceil-to-32 n)
    "Round up to next multiple of 32 (returns 0 for 0)"
    (if (== n 0)
        0
        (let ((rem (lisp UFix (n) (cl:mod n 32))))
          (if (== rem 0)
              n
              (+ n (- 32 rem)))))))

;;; CL helper for encoding integers
(cl:defun %encode-uint256-integer (n)
  "Encode an integer as a 32-byte big-endian uint256"
  (cl:let ((result (cl:make-array 32 :fill-pointer 32 :adjustable cl:t
                                     :initial-element 0)))
    (cl:loop :for i :from 0 :below 32
             :do (cl:setf (cl:aref result (cl:- 31 i))
                          (cl:ldb (cl:byte 8 (cl:* i 8)) n)))
    result))
