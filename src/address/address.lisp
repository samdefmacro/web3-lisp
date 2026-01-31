(in-package #:web3/address)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; Address type - exactly 20 bytes

  (define-type Address
    "Ethereum address (20 bytes)"
    (%Address types:Bytes))

  (declare address-bytes (Address -> types:Bytes))
  (define (address-bytes addr)
    "Get the raw 20 bytes of an address"
    (match addr ((%Address b) b)))

  (declare address-zero (Unit -> Address))
  (define (address-zero)
    "The zero address (0x0000...0000)"
    (%Address (types:make-bytes 20)))

  (declare address-from-bytes (types:Bytes -> (types:Web3Result Address)))
  (define (address-from-bytes bytes)
    "Create an address from exactly 20 bytes"
    (if (/= (types:bytes-length bytes) 20)
        (Err (types:AddressError "Address must be exactly 20 bytes"))
        (Ok (%Address (types:bytes-copy bytes)))))

  (declare address-from-hex (String -> (types:Web3Result Address)))
  (define (address-from-hex hex-str)
    "Parse an address from a hex string (with or without 0x prefix, with or without checksum)"
    (match (types:hex-decode hex-str)
      ((Err e) (Err e))
      ((Ok bytes)
       (address-from-bytes bytes))))

  (declare address-to-hex (Address -> String))
  (define (address-to-hex addr)
    "Convert address to 0x-prefixed lowercase hex string"
    (types:hex-encode-prefixed (address-bytes addr)))

  (declare address-to-checksum-hex (Address -> String))
  (define (address-to-checksum-hex addr)
    "Convert address to EIP-55 checksummed hex string"
    (let ((hex-lower (types:hex-encode (address-bytes addr))))
      ;; Hash the lowercase hex string (no prefix)
      (let ((addr-hash (crypto:keccak256
                   (lisp types:Bytes (hex-lower)
                     (cl:let* ((octets (cl:map 'cl:vector #'cl:char-code hex-lower))
                               (result (cl:make-array (cl:length octets)
                                                      :fill-pointer (cl:length octets)
                                                      :adjustable cl:t
                                                      :initial-contents octets)))
                       result)))))
        ;; Apply checksum: uppercase if corresponding hash nibble >= 8
        (lisp String (hex-lower addr-hash)
          (cl:let ((result (cl:make-string (cl:+ 2 (cl:length hex-lower)))))
            (cl:setf (cl:char result 0) #\0)
            (cl:setf (cl:char result 1) #\x)
            (cl:loop :for i :from 0 :below (cl:length hex-lower)
                     :for ch := (cl:char hex-lower i)
                     :for hash-byte := (cl:aref addr-hash (cl:floor i 2))
                     :for hash-nibble := (cl:if (cl:evenp i)
                                                (cl:ash hash-byte -4)
                                                (cl:logand hash-byte #xf))
                     :do (cl:setf (cl:char result (cl:+ 2 i))
                                  (cl:if (cl:and (cl:alpha-char-p ch)
                                                 (cl:>= hash-nibble 8))
                                         (cl:char-upcase ch)
                                         ch)))
            result)))))

  (declare address-from-public-key (types:Bytes -> (types:Web3Result Address)))
  (define (address-from-public-key pub-key)
    "Derive an Ethereum address from a public key.
     Accepts uncompressed (65 bytes), compressed (33 bytes), or raw (64 bytes)."
    (match (crypto:public-key-to-uncompressed pub-key)
      ((Err e) (Err e))
      ((Ok uncompressed)
       ;; Hash the 64-byte public key (skip the 04 prefix)
       (let ((key-data (types:bytes-drop 1 uncompressed)))
         (let ((addr-hash (crypto:keccak256 key-data)))
           ;; Take last 20 bytes of hash
           (address-from-bytes (types:bytes-drop 12 addr-hash)))))))

  ;;; Contract address computation

  (declare compute-contract-address (Address -> U64 -> Address))
  (define (compute-contract-address deployer nonce)
    "Compute contract address using CREATE: keccak256(rlp([sender, nonce]))[12:]"
    ;; RLP encode [sender_address, nonce]
    ;; For simplicity, we build the RLP manually
    (let ((sender-bytes (address-bytes deployer))
          (nonce-bytes (lisp types:Bytes (nonce)
                        (cl:if (cl:zerop nonce)
                               ;; RLP: 0 encodes as empty byte string
                               (cl:make-array 0 :fill-pointer 0 :adjustable cl:t)
                               (cl:let* ((byte-count (cl:ceiling (cl:integer-length nonce) 8))
                                         (result (cl:make-array byte-count
                                                                :fill-pointer byte-count
                                                                :adjustable cl:t)))
                                 (cl:loop :for i :from 0 :below byte-count
                                          :do (cl:setf (cl:aref result (cl:- byte-count 1 i))
                                                       (cl:ldb (cl:byte 8 (cl:* i 8)) nonce)))
                                 result)))))
      ;; Build RLP list: [address (20 bytes), nonce]
      (let ((rlp-encoded
              (lisp types:Bytes (sender-bytes nonce-bytes)
                ;; Build RLP encoding of [sender, nonce]
                (cl:let* (;; RLP encode sender address (20 bytes -> 0x94 prefix)
                          (rlp-sender (cl:make-array 21 :fill-pointer 21 :adjustable cl:t))
                          ;; RLP encode nonce
                          (rlp-nonce
                            (cl:cond
                              ;; Empty nonce (0) -> 0x80
                              ((cl:zerop (cl:length nonce-bytes))
                               (cl:let ((r (cl:make-array 1 :fill-pointer 1 :adjustable cl:t)))
                                 (cl:setf (cl:aref r 0) #x80)
                                 r))
                              ;; Single byte <= 0x7f
                              ((cl:and (cl:= (cl:length nonce-bytes) 1)
                                       (cl:<= (cl:aref nonce-bytes 0) #x7f))
                               nonce-bytes)
                              ;; Length-prefixed
                              (cl:t
                               (cl:let* ((len (cl:length nonce-bytes))
                                         (r (cl:make-array (cl:1+ len)
                                                           :fill-pointer (cl:1+ len)
                                                           :adjustable cl:t)))
                                 (cl:setf (cl:aref r 0) (cl:+ #x80 len))
                                 (cl:loop :for i :from 0 :below len
                                          :do (cl:setf (cl:aref r (cl:1+ i))
                                                       (cl:aref nonce-bytes i)))
                                 r))))
                          ;; Total payload length
                          (payload-len (cl:+ 21 (cl:length rlp-nonce)))
                          ;; List prefix
                          (list-prefix
                            (cl:if (cl:< payload-len 56)
                                   (cl:let ((r (cl:make-array 1 :fill-pointer 1 :adjustable cl:t)))
                                     (cl:setf (cl:aref r 0) (cl:+ #xc0 payload-len))
                                     r)
                                   ;; Long list (unlikely for address+nonce)
                                   (cl:let* ((len-bytes-count (cl:ceiling (cl:integer-length payload-len) 8))
                                             (r (cl:make-array (cl:1+ len-bytes-count)
                                                               :fill-pointer (cl:1+ len-bytes-count)
                                                               :adjustable cl:t)))
                                     (cl:setf (cl:aref r 0) (cl:+ #xf7 len-bytes-count))
                                     (cl:loop :for i :from 0 :below len-bytes-count
                                              :do (cl:setf (cl:aref r (cl:- len-bytes-count i))
                                                           (cl:ldb (cl:byte 8 (cl:* i 8)) payload-len)))
                                     r))))
                  ;; Build sender RLP (0x94 || address)
                  (cl:setf (cl:aref rlp-sender 0) #x94)
                  (cl:loop :for i :from 0 :below 20
                           :do (cl:setf (cl:aref rlp-sender (cl:1+ i))
                                        (cl:aref sender-bytes i)))
                  ;; Concatenate: list-prefix || rlp-sender || rlp-nonce
                  (cl:let* ((total-len (cl:+ (cl:length list-prefix)
                                             (cl:length rlp-sender)
                                             (cl:length rlp-nonce)))
                            (result (cl:make-array total-len
                                                   :fill-pointer total-len
                                                   :adjustable cl:t))
                            (pos 0))
                    (cl:loop :for b :across list-prefix
                             :do (cl:setf (cl:aref result pos) b)
                                 (cl:incf pos))
                    (cl:loop :for b :across rlp-sender
                             :do (cl:setf (cl:aref result pos) b)
                                 (cl:incf pos))
                    (cl:loop :for b :across rlp-nonce
                             :do (cl:setf (cl:aref result pos) b)
                                 (cl:incf pos))
                    result)))))
        (let ((addr-hash (crypto:keccak256 rlp-encoded)))
          (match (address-from-bytes (types:bytes-drop 12 addr-hash))
            ((Ok addr) addr)
            ((Err _) (address-zero)))))))

  (declare compute-create2-address (Address -> types:Bytes -> types:Bytes -> Address))
  (define (compute-create2-address deployer salt init-code-hash)
    "Compute contract address using CREATE2: keccak256(0xff ++ sender ++ salt ++ keccak256(init_code))[12:]"
    (let ((data (types:bytes-concat-many
                 (Cons (types:bytes-from-list (Cons #xff Nil))
                       (Cons (address-bytes deployer)
                             (Cons (types:bytes-pad-left 32 salt)
                                   (Cons init-code-hash Nil)))))))
      (let ((addr-hash (crypto:keccak256 data)))
        (match (address-from-bytes (types:bytes-drop 12 addr-hash))
          ((Ok addr) addr)
          ((Err _) (address-zero)))))))
