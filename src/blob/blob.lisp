;;;; Blob implementation
;;;; EIP-4844 blob data encoding

(in-package #:web3/blob)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Helper Functions
  ;;; =========================================================================

  (declare blob-count-for-data (UFix -> UFix))
  (define (blob-count-for-data data-length)
    "Calculate the number of blobs needed to store data of given length.
     Accounts for 0x80 terminator byte."
    (let ((total-bytes (+ data-length 1)))  ;; +1 for terminator
      (lisp UFix (total-bytes)
        (cl:ceiling total-bytes +usable-bytes-per-blob+))))

  (declare empty-blob (Unit -> types:Bytes))
  (define (empty-blob _)
    "Create an empty 128KB blob (all zeros)"
    (lisp types:Bytes ()
      (cl:make-array +bytes-per-blob+ :fill-pointer +bytes-per-blob+
                                       :adjustable cl:t
                                       :initial-element 0)))

  (declare valid-blob? (types:Bytes -> Boolean))
  (define (valid-blob? blob)
    "Check if a blob has the correct size (128KB)"
    (== (types:bytes-length blob) +bytes-per-blob+))

  ;;; =========================================================================
  ;;; Blob Encoding
  ;;; =========================================================================

  (declare %encode-field-element (types:Bytes -> UFix -> UFix -> types:Bytes))
  (define (%encode-field-element data start-idx data-len)
    "Encode up to 31 bytes of data into a 32-byte field element.
     First byte is always 0x00 (BLS12-381 modulus constraint)."
    (lisp types:Bytes (data start-idx data-len)
      (cl:let ((field-elem (cl:make-array 32 :fill-pointer 32 :adjustable cl:t
                                             :initial-element 0))
               (bytes-to-copy (cl:min 31 (cl:max 0 (cl:- data-len start-idx)))))
        ;; First byte is 0x00 (already initialized)
        ;; Copy up to 31 bytes of data starting at byte 1
        (cl:loop :for i :from 0 :below bytes-to-copy
                 :do (cl:setf (cl:aref field-elem (cl:1+ i))
                              (cl:aref data (cl:+ start-idx i))))
        field-elem)))

  (declare to-blobs (types:Bytes -> (List types:Bytes)))
  (define (to-blobs data)
    "Encode data into one or more 128KB blobs.
     Each field element starts with 0x00, data fills remaining 31 bytes.
     Data is terminated with 0x80 byte."
    (lisp (List types:Bytes) (data)
      (cl:let* ((data-len (cl:length data))
                ;; Add terminator byte (0x80) to mark end of data
                (total-len (cl:1+ data-len))
                (num-blobs (cl:ceiling total-len +usable-bytes-per-blob+))
                (blobs cl:nil)
                (data-idx 0))
        (cl:dotimes (blob-idx num-blobs)
          (cl:let ((blob (cl:make-array +bytes-per-blob+
                                        :fill-pointer +bytes-per-blob+
                                        :adjustable cl:t
                                        :initial-element 0))
                   (blob-offset 0))
            ;; Fill each field element in the blob
            (cl:dotimes (fe-idx +field-elements-per-blob+)
              (cl:let ((fe-start (cl:* fe-idx 32)))
                ;; First byte of field element is always 0 (already initialized)
                ;; Copy up to 31 bytes of data into remaining space
                (cl:loop :for i :from 0 :below 31
                         :while (cl:< data-idx total-len)
                         :do (cl:let ((byte-val
                                        (cl:if (cl:= data-idx data-len)
                                               #x80  ;; Terminator byte
                                               (cl:aref data data-idx))))
                               (cl:setf (cl:aref blob (cl:+ fe-start 1 i)) byte-val)
                               (cl:incf data-idx)))))
            (cl:push blob blobs)))
        ;; Return in correct order (reversed from push)
        (cl:nreverse blobs))))

  ;;; =========================================================================
  ;;; Blob Decoding
  ;;; =========================================================================

  (declare from-blobs ((List types:Bytes) -> types:Bytes))
  (define (from-blobs blobs)
    "Decode data from a list of blobs.
     Reads 31 bytes from each field element, stops at 0x80 terminator."
    (lisp types:Bytes (blobs)
      (cl:let ((output (cl:make-array 0 :fill-pointer 0 :adjustable cl:t))
               (done cl:nil))
        (cl:dolist (blob blobs)
          (cl:unless done
            ;; Extract data from each field element
            (cl:dotimes (fe-idx +field-elements-per-blob+)
              (cl:unless done
                (cl:let ((fe-start (cl:* fe-idx 32)))
                  ;; Read bytes 1-31 (skip byte 0 which is always 0x00)
                  (cl:loop :for i :from 1 :below 32
                           :for byte-val := (cl:aref blob (cl:+ fe-start i))
                           :do (cl:if (cl:= byte-val #x80)
                                      (cl:setf done cl:t)
                                      (cl:unless done
                                        (cl:vector-push-extend byte-val output)))))))))
        output)))

  ;;; =========================================================================
  ;;; Versioned Hash
  ;;; =========================================================================

  (declare commitment-to-versioned-hash (types:Bytes -> types:Bytes))
  (define (commitment-to-versioned-hash commitment)
    "Convert a KZG commitment (48 bytes) to a versioned hash (32 bytes).
     Uses SHA256 hash with version byte 0x01 prefix."
    (lisp types:Bytes (commitment)
      (cl:let* (;; Compute SHA256 of the commitment
                (digest (ironclad:make-digest :sha256))
                (vec (cl:make-array (cl:length commitment)
                                    :element-type '(cl:unsigned-byte 8)
                                    :initial-contents commitment))
                (hash-result (ironclad:digest-sequence digest vec))
                ;; Create versioned hash: 0x01 prefix + last 31 bytes of hash
                (result (cl:make-array 32 :fill-pointer 32 :adjustable cl:t)))
        ;; Set version byte (0x01 for KZG)
        (cl:setf (cl:aref result 0) #x01)
        ;; Copy last 31 bytes of hash
        (cl:loop :for i :from 1 :below 32
                 :do (cl:setf (cl:aref result i) (cl:aref hash-result i)))
        result))))


;;; =========================================================================
;;; Exports
;;; =========================================================================

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:export '(+bytes-per-field-element+
               +field-elements-per-blob+
               +bytes-per-blob+
               +max-blobs-per-block+
               +usable-bytes-per-field-element+
               to-blobs
               from-blobs
               commitment-to-versioned-hash
               empty-blob
               blob-count-for-data
               valid-blob?)
             (cl:find-package '#:web3/blob)))
