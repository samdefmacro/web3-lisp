;;;; KZG implementation
;;;; High-level KZG commitment functions

(in-package #:web3/kzg)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Context Management
  ;;; =========================================================================

  (declare make-kzg-context (String -> (types:Web3Result KZGContext)))
  (define (make-kzg-context trusted-setup-path)
    "Load a KZG trusted setup from file and create a context.
     The trusted setup file should be the Ethereum mainnet trusted setup."
    (lisp (types:Web3Result KZGContext) (trusted-setup-path)
      (cl:block make-kzg-context-block
        ;; Ensure library is loaded
        (ensure-kzg-library)
        (cl:unless *kzg-library-loaded*
          (cl:return-from make-kzg-context-block
            (coalton-prelude:Err
             (web3/types:CryptoError "c-kzg library not found. Install c-kzg-4844."))))
        ;; Check if file exists
        (cl:unless (cl:probe-file trusted-setup-path)
          (cl:return-from make-kzg-context-block
            (coalton-prelude:Err
             (web3/types:CryptoError
              (cl:format cl:nil "Trusted setup file not found: ~A" trusted-setup-path)))))
        ;; Allocate settings struct
        (cl:let ((settings (cffi:foreign-alloc :uint8 :count +kzg-settings-size+)))
          (cffi:with-foreign-string (path-ptr trusted-setup-path)
            (cl:let ((fp (cffi:foreign-funcall "fopen"
                                                :string trusted-setup-path
                                                :string "rb"
                                                :pointer)))
              (cl:when (cffi:null-pointer-p fp)
                (cffi:foreign-free settings)
                (cl:return-from make-kzg-context-block
                  (coalton-prelude:Err
                   (web3/types:CryptoError "Failed to open trusted setup file"))))
              (cl:let ((ret (%load-trusted-setup-file settings fp)))
                (cffi:foreign-funcall "fclose" :pointer fp :int)
                (cl:if (cl:eq ret :ok)
                       (coalton-prelude:Ok settings)
                       (cl:progn
                         (cffi:foreign-free settings)
                         (coalton-prelude:Err
                          (web3/types:CryptoError
                           (cl:format cl:nil "Failed to load trusted setup: ~A" ret))))))))))))

  (declare kzg-context-loaded? (KZGContext -> Boolean))
  (define (kzg-context-loaded? ctx)
    "Check if a KZG context is valid and loaded"
    (lisp Boolean (ctx)
      (cl:not (cffi:null-pointer-p ctx))))

  ;;; =========================================================================
  ;;; Commitment Functions
  ;;; =========================================================================

  (declare blob-to-commitment (types:Bytes -> KZGContext -> (types:Web3Result KZGCommitment)))
  (define (blob-to-commitment blob-data ctx)
    "Compute a KZG commitment for a blob.
     Returns a 48-byte compressed G1 point."
    (lisp (types:Web3Result KZGCommitment) (blob-data ctx)
      (cl:block blob-to-commitment-block
        ;; Validate blob size
        (cl:unless (cl:= (cl:length blob-data) blob:+bytes-per-blob+)
          (cl:return-from blob-to-commitment-block
            (coalton-prelude:Err
             (web3/types:CryptoError "Blob must be exactly 131072 bytes"))))
        ;; Allocate output commitment
        (cffi:with-foreign-objects ((commitment-ptr :uint8 48)
                                    (blob-ptr :uint8 blob:+bytes-per-blob+))
          ;; Copy blob data to foreign memory
          (cl:loop :for i :from 0 :below blob:+bytes-per-blob+
                   :do (cl:setf (cffi:mem-aref blob-ptr :uint8 i) (cl:aref blob-data i)))
          ;; Compute commitment
          (cl:let ((ret (%blob-to-kzg-commitment commitment-ptr blob-ptr ctx)))
            (cl:if (cl:eq ret :ok)
                   ;; Copy result back to Lisp
                   (cl:let ((result (cl:make-array 48 :fill-pointer 48 :adjustable cl:t)))
                     (cl:loop :for i :from 0 :below 48
                              :do (cl:setf (cl:aref result i)
                                           (cffi:mem-aref commitment-ptr :uint8 i)))
                     (coalton-prelude:Ok result))
                   (coalton-prelude:Err
                    (web3/types:CryptoError
                     (cl:format cl:nil "Failed to compute commitment: ~A" ret)))))))))

  (declare blobs-to-commitments ((List types:Bytes) -> KZGContext -> (types:Web3Result (List KZGCommitment))))
  (define (blobs-to-commitments blobs ctx)
    "Compute KZG commitments for a list of blobs"
    (let ((results
            (map (fn (b) (blob-to-commitment b ctx)) blobs)))
      ;; Check if any failed
      (lisp (types:Web3Result (List KZGCommitment)) (results)
        (cl:let ((commitments cl:nil)
                 (err-val cl:nil))
          (cl:dolist (r results)
            (cl:typecase r
              (coalton-library/classes::result/ok
               (cl:push (cl:slot-value r 'coalton-library/classes::_0) commitments))
              (coalton-library/classes::result/err
               (cl:setf err-val (cl:slot-value r 'coalton-library/classes::_0)))))
          (cl:if err-val
                 (coalton-prelude:Err err-val)
                 (coalton-prelude:Ok (cl:nreverse commitments)))))))

  ;;; =========================================================================
  ;;; Proof Functions
  ;;; =========================================================================

  (declare compute-blob-proof (types:Bytes -> KZGCommitment -> KZGContext -> (types:Web3Result KZGProof)))
  (define (compute-blob-proof blob-data commitment ctx)
    "Compute a KZG proof for a blob given its commitment.
     Returns a 48-byte compressed G1 point."
    (lisp (types:Web3Result KZGProof) (blob-data commitment ctx)
      (cl:block compute-blob-proof-block
        ;; Validate inputs
        (cl:unless (cl:= (cl:length blob-data) blob:+bytes-per-blob+)
          (cl:return-from compute-blob-proof-block
            (coalton-prelude:Err
             (web3/types:CryptoError "Blob must be exactly 131072 bytes"))))
        (cl:unless (cl:= (cl:length commitment) 48)
          (cl:return-from compute-blob-proof-block
            (coalton-prelude:Err
             (web3/types:CryptoError "Commitment must be exactly 48 bytes"))))
        ;; Allocate foreign memory
        (cffi:with-foreign-objects ((proof-ptr :uint8 48)
                                    (y-ptr :uint8 32)
                                    (blob-ptr :uint8 blob:+bytes-per-blob+)
                                    (commitment-ptr :uint8 48))
          ;; Copy data to foreign memory
          (cl:loop :for i :from 0 :below blob:+bytes-per-blob+
                   :do (cl:setf (cffi:mem-aref blob-ptr :uint8 i) (cl:aref blob-data i)))
          (cl:loop :for i :from 0 :below 48
                   :do (cl:setf (cffi:mem-aref commitment-ptr :uint8 i) (cl:aref commitment i)))
          ;; Compute proof
          (cl:let ((ret (%compute-blob-kzg-proof proof-ptr y-ptr blob-ptr commitment-ptr ctx)))
            (cl:if (cl:eq ret :ok)
                   ;; Copy result back to Lisp
                   (cl:let ((result (cl:make-array 48 :fill-pointer 48 :adjustable cl:t)))
                     (cl:loop :for i :from 0 :below 48
                              :do (cl:setf (cl:aref result i)
                                           (cffi:mem-aref proof-ptr :uint8 i)))
                     (coalton-prelude:Ok result))
                   (coalton-prelude:Err
                    (web3/types:CryptoError
                     (cl:format cl:nil "Failed to compute proof: ~A" ret)))))))))

  (declare compute-kzg-proof (types:Bytes -> types:Bytes -> KZGContext -> (types:Web3Result (Tuple KZGProof types:Bytes))))
  (define (compute-kzg-proof blob-data z ctx)
    "Compute a KZG proof for a blob at evaluation point z.
     Returns (proof, y) where y = p(z) for the polynomial p represented by the blob."
    (lisp (types:Web3Result (Tuple KZGProof types:Bytes)) (blob-data z ctx)
      (cl:block compute-kzg-proof-block
        ;; Validate inputs
        (cl:unless (cl:= (cl:length blob-data) blob:+bytes-per-blob+)
          (cl:return-from compute-kzg-proof-block
            (coalton-prelude:Err
             (web3/types:CryptoError "Blob must be exactly 131072 bytes"))))
        (cl:unless (cl:= (cl:length z) 32)
          (cl:return-from compute-kzg-proof-block
            (coalton-prelude:Err
             (web3/types:CryptoError "Evaluation point z must be exactly 32 bytes"))))
        ;; Allocate foreign memory
        (cffi:with-foreign-objects ((proof-ptr :uint8 48)
                                    (y-ptr :uint8 32)
                                    (blob-ptr :uint8 blob:+bytes-per-blob+)
                                    (z-ptr :uint8 32))
          ;; Copy data to foreign memory
          (cl:loop :for i :from 0 :below blob:+bytes-per-blob+
                   :do (cl:setf (cffi:mem-aref blob-ptr :uint8 i) (cl:aref blob-data i)))
          (cl:loop :for i :from 0 :below 32
                   :do (cl:setf (cffi:mem-aref z-ptr :uint8 i) (cl:aref z i)))
          ;; Compute proof
          (cl:let ((ret (%compute-kzg-proof proof-ptr y-ptr blob-ptr z-ptr ctx)))
            (cl:if (cl:eq ret :ok)
                   ;; Copy results back to Lisp
                   (cl:let ((proof-result (cl:make-array 48 :fill-pointer 48 :adjustable cl:t))
                            (y-result (cl:make-array 32 :fill-pointer 32 :adjustable cl:t)))
                     (cl:loop :for i :from 0 :below 48
                              :do (cl:setf (cl:aref proof-result i)
                                           (cffi:mem-aref proof-ptr :uint8 i)))
                     (cl:loop :for i :from 0 :below 32
                              :do (cl:setf (cl:aref y-result i)
                                           (cffi:mem-aref y-ptr :uint8 i)))
                     (coalton-prelude:Ok (coalton-prelude:Tuple proof-result y-result)))
                   (coalton-prelude:Err
                    (web3/types:CryptoError
                     (cl:format cl:nil "Failed to compute KZG proof: ~A" ret)))))))))

  ;;; =========================================================================
  ;;; Verification Functions
  ;;; =========================================================================

  (declare verify-blob-proof (types:Bytes -> KZGCommitment -> KZGProof -> KZGContext -> Boolean))
  (define (verify-blob-proof blob-data commitment proof ctx)
    "Verify a KZG proof for a blob against its commitment.
     Returns True if the proof is valid."
    (lisp Boolean (blob-data commitment proof ctx)
      (cl:block verify-blob-proof-block
        ;; Validate inputs
        (cl:unless (cl:and (cl:= (cl:length blob-data) blob:+bytes-per-blob+)
                           (cl:= (cl:length commitment) 48)
                           (cl:= (cl:length proof) 48))
          (cl:return-from verify-blob-proof-block coalton:False))
        ;; Allocate foreign memory
        (cffi:with-foreign-objects ((result-ptr :bool)
                                    (blob-ptr :uint8 blob:+bytes-per-blob+)
                                    (commitment-ptr :uint8 48)
                                    (proof-ptr :uint8 48))
          ;; Copy data to foreign memory
          (cl:loop :for i :from 0 :below blob:+bytes-per-blob+
                   :do (cl:setf (cffi:mem-aref blob-ptr :uint8 i) (cl:aref blob-data i)))
          (cl:loop :for i :from 0 :below 48
                   :do (cl:setf (cffi:mem-aref commitment-ptr :uint8 i) (cl:aref commitment i)))
          (cl:loop :for i :from 0 :below 48
                   :do (cl:setf (cffi:mem-aref proof-ptr :uint8 i) (cl:aref proof i)))
          ;; Verify
          (cl:let ((ret (%verify-blob-kzg-proof result-ptr blob-ptr commitment-ptr proof-ptr ctx)))
            (cl:if (cl:eq ret :ok)
                   (cl:if (cffi:mem-ref result-ptr :bool)
                          coalton:True
                          coalton:False)
                   coalton:False))))))

  (declare verify-blob-proofs ((List types:Bytes) -> (List KZGCommitment) -> (List KZGProof) -> KZGContext -> Boolean))
  (define (verify-blob-proofs blobs commitments proofs ctx)
    "Batch verify multiple blob proofs. More efficient than individual verification."
    (lisp Boolean (blobs commitments proofs ctx)
      (cl:block verify-blob-proofs-block
        (cl:let ((n (cl:length blobs)))
          ;; Validate list lengths match
          (cl:unless (cl:and (cl:= (cl:length commitments) n)
                             (cl:= (cl:length proofs) n))
            (cl:return-from verify-blob-proofs-block coalton:False))
          ;; Handle empty case
          (cl:when (cl:zerop n)
            (cl:return-from verify-blob-proofs-block coalton:True))
          ;; Allocate contiguous arrays
          (cl:let ((blob-size blob:+bytes-per-blob+))
            (cffi:with-foreign-objects ((result-ptr :bool)
                                        (blobs-ptr :uint8 (cl:* n blob-size))
                                        (commitments-ptr :uint8 (cl:* n 48))
                                        (proofs-ptr :uint8 (cl:* n 48)))
              ;; Copy all data
              (cl:let ((blob-idx 0))
                (cl:dolist (blob blobs)
                  (cl:loop :for i :from 0 :below blob-size
                           :do (cl:setf (cffi:mem-aref blobs-ptr :uint8
                                                       (cl:+ (cl:* blob-idx blob-size) i))
                                        (cl:aref blob i)))
                  (cl:incf blob-idx)))
              (cl:let ((comm-idx 0))
                (cl:dolist (comm commitments)
                  (cl:loop :for i :from 0 :below 48
                           :do (cl:setf (cffi:mem-aref commitments-ptr :uint8
                                                       (cl:+ (cl:* comm-idx 48) i))
                                        (cl:aref comm i)))
                  (cl:incf comm-idx)))
              (cl:let ((proof-idx 0))
                (cl:dolist (proof proofs)
                  (cl:loop :for i :from 0 :below 48
                           :do (cl:setf (cffi:mem-aref proofs-ptr :uint8
                                                       (cl:+ (cl:* proof-idx 48) i))
                                        (cl:aref proof i)))
                  (cl:incf proof-idx)))
              ;; Batch verify
              (cl:let ((ret (%verify-blob-kzg-proof-batch
                             result-ptr blobs-ptr commitments-ptr proofs-ptr n ctx)))
                (cl:if (cl:eq ret :ok)
                       (cl:if (cffi:mem-ref result-ptr :bool)
                              coalton:True
                              coalton:False)
                       coalton:False)))))))))


