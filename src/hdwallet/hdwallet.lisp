;;;; HD Wallet - BIP-39 Mnemonics and BIP-32 Key Derivation
;;;;
;;;; Implements:
;;;; - BIP-39: Mnemonic code for generating deterministic keys
;;;; - BIP-32: Hierarchical Deterministic Wallets
;;;; - BIP-44: Multi-account hierarchy (Ethereum path: m/44'/60'/0'/0/x)

(in-package #:web3/hdwallet)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; BIP-39 Word List (English)
  ;;; =========================================================================

  ;; The word list is stored in Common Lisp and accessed via FFI
  ;; for efficiency (2048 words is large for inline Coalton data)

  (declare get-word (UFix -> String))
  (define (get-word index)
    "Get word at index from BIP-39 English word list."
    (lisp String (index)
      (cl:aref web3/hdwallet::*bip39-wordlist* index)))

  (declare find-word-index (String -> (Optional UFix)))
  (define (find-word-index word)
    "Find the index of a word in the BIP-39 word list."
    (lisp (Optional UFix) (word)
      (cl:let ((pos (cl:position word web3/hdwallet::*bip39-wordlist* :test #'cl:string=)))
        (cl:if pos
               (Some pos)
               None))))

  ;;; =========================================================================
  ;;; Cryptographic Helpers
  ;;; =========================================================================

  (declare hmac-sha512 (types:Bytes -> types:Bytes -> types:Bytes))
  (define (hmac-sha512 key data)
    "Compute HMAC-SHA512."
    (lisp types:Bytes (key data)
      ;; Ironclad requires simple arrays of (unsigned-byte 8)
      (cl:let* ((key-simple (cl:make-array (cl:length key)
                                           :element-type '(cl:unsigned-byte 8)
                                           :initial-contents (cl:coerce key 'cl:list)))
                (data-simple (cl:make-array (cl:length data)
                                            :element-type '(cl:unsigned-byte 8)
                                            :initial-contents (cl:coerce data 'cl:list)))
                (hmac (ironclad:make-hmac key-simple :sha512))
                (result (cl:make-array 64 :fill-pointer 64 :adjustable cl:t)))
        (ironclad:update-hmac hmac data-simple)
        (cl:replace result (ironclad:hmac-digest hmac))
        result)))

  (declare pbkdf2-sha512 (types:Bytes -> types:Bytes -> UFix -> types:Bytes))
  (define (pbkdf2-sha512 password salt iterations)
    "Derive key using PBKDF2-HMAC-SHA512."
    (lisp types:Bytes (password salt iterations)
      ;; Ironclad requires simple arrays of (unsigned-byte 8)
      (cl:let* ((pass-simple (cl:make-array (cl:length password)
                                            :element-type '(cl:unsigned-byte 8)
                                            :initial-contents (cl:coerce password 'cl:list)))
                (salt-simple (cl:make-array (cl:length salt)
                                            :element-type '(cl:unsigned-byte 8)
                                            :initial-contents (cl:coerce salt 'cl:list)))
                (derived (ironclad:derive-key
                          (ironclad:make-kdf 'ironclad:pbkdf2 :digest :sha512)
                          pass-simple salt-simple iterations 64))
                (result (cl:make-array 64 :fill-pointer 64 :adjustable cl:t)))
        (cl:replace result derived)
        result)))

  (declare sha256 (types:Bytes -> types:Bytes))
  (define (sha256 data)
    "Compute SHA256 hash."
    (lisp types:Bytes (data)
      (cl:let* ((digest (ironclad:digest-sequence :sha256 data))
                (len (cl:length digest))
                (result (cl:make-array len :fill-pointer len :adjustable cl:t)))
        (cl:replace result digest)
        result)))

  ;;; =========================================================================
  ;;; BIP-39 Mnemonic Functions
  ;;; =========================================================================

  (declare entropy-to-mnemonic (types:Bytes -> (types:Web3Result String)))
  (define (entropy-to-mnemonic entropy)
    "Convert entropy bytes to mnemonic phrase.
     Entropy must be 16, 20, 24, 28, or 32 bytes (128-256 bits)."
    (lisp (types:Web3Result String) (entropy)
      ;; Ensure wordlist is loaded
      (cl:if (cl:null web3/hdwallet::*bip39-wordlist*)
             (Err (web3/types:WalletError "BIP-39 wordlist not loaded. Call ensure-wordlist."))
             (cl:let ((ent-len (cl:length entropy)))
               (cl:if (cl:not (cl:member ent-len '(16 20 24 28 32)))
                      (Err (web3/types:WalletError "Invalid entropy length (must be 16-32 bytes)"))
               ;; Convert to simple array for ironclad
               (cl:let* ((entropy-simple (cl:make-array ent-len
                                                        :element-type '(cl:unsigned-byte 8)
                                                        :initial-contents (cl:coerce entropy 'cl:list)))
                         ;; Calculate checksum
                         (sha-hash (ironclad:digest-sequence :sha256 entropy-simple))
                         (checksum-bits (cl:floor ent-len 4))
                         ;; Convert entropy + checksum to bits
                         (total-bits (cl:+ (cl:* ent-len 8) checksum-bits))
                         (word-count (cl:floor total-bits 11))
                         ;; Build bit string
                         (bits (cl:make-array total-bits :element-type 'cl:bit))
                         (bit-idx 0))
                 ;; Copy entropy bits
                 (cl:loop :for byte :across entropy :do
                   (cl:loop :for i :from 7 :downto 0 :do
                     (cl:setf (cl:aref bits bit-idx) (cl:ldb (cl:byte 1 i) byte))
                     (cl:incf bit-idx)))
                 ;; Copy checksum bits
                 (cl:loop :for i :from (cl:- 8 1) :downto (cl:- 8 checksum-bits) :do
                   (cl:setf (cl:aref bits bit-idx) (cl:ldb (cl:byte 1 i) (cl:aref sha-hash 0)))
                   (cl:incf bit-idx))
                 ;; Convert to words
                 (cl:let ((words (cl:make-array word-count)))
                   (cl:loop :for w :below word-count :do
                     (cl:let ((idx 0))
                       (cl:loop :for b :from 0 :below 11 :do
                         (cl:setf idx (cl:+ (cl:ash idx 1) (cl:aref bits (cl:+ (cl:* w 11) b)))))
                       (cl:setf (cl:aref words w) (cl:aref web3/hdwallet::*bip39-wordlist* idx))))
                   (Ok (cl:format cl:nil "~{~A~^ ~}" (cl:coerce words 'cl:list))))))))))

  (declare generate-mnemonic (UFix -> (types:Web3Result String)))
  (define (generate-mnemonic word-count)
    "Generate a random mnemonic with specified word count.
     Valid counts: 12, 15, 18, 21, 24 words."
    (lisp (types:Web3Result String) (word-count)
      (cl:let ((entropy-bytes (cl:case word-count
                                (12 16)
                                (15 20)
                                (18 24)
                                (21 28)
                                (24 32)
                                (cl:otherwise cl:nil))))
        (cl:if (cl:null entropy-bytes)
               (Err (web3/types:WalletError "Invalid word count (must be 12, 15, 18, 21, or 24)"))
               (cl:let* ((entropy (cl:make-array entropy-bytes
                                                 :fill-pointer entropy-bytes
                                                 :adjustable cl:t)))
                 ;; Generate cryptographically secure random entropy
                 (cl:let ((secure-bytes (ironclad:random-data entropy-bytes)))
                   (cl:loop :for i :below entropy-bytes :do
                     (cl:setf (cl:aref entropy i) (cl:aref secure-bytes i))))
                 (web3/hdwallet::entropy-to-mnemonic entropy))))))

  (declare validate-mnemonic (String -> Boolean))
  (define (validate-mnemonic mnemonic)
    "Validate a mnemonic phrase (check words and checksum)."
    (lisp Boolean (mnemonic)
      (cl:let* ((words (uiop:split-string mnemonic :separator " "))
                (words (cl:remove-if (cl:lambda (s) (cl:zerop (cl:length s))) words))
                (word-count (cl:length words)))
        (cl:if (cl:not (cl:member word-count '(12 15 18 21 24)))
               coalton:False
               ;; Check all words exist and get indices
               (cl:let ((indices (cl:make-array word-count)))
                 (cl:if (cl:loop :for i :below word-count :do
                          (cl:let ((pos (cl:position (cl:nth i words)
                                                     web3/hdwallet::*bip39-wordlist*
                                                     :test #'cl:string=)))
                            (cl:if (cl:null pos)
                                   (cl:return cl:nil)
                                   (cl:setf (cl:aref indices i) pos)))
                          :finally (cl:return cl:t))
                        ;; Verify checksum
                        (cl:let* ((total-bits (cl:* word-count 11))
                                  (checksum-bits (cl:floor word-count 3))
                                  (entropy-bits (cl:- total-bits checksum-bits))
                                  (bits (cl:make-array total-bits :element-type 'cl:bit)))
                          ;; Convert indices to bits
                          (cl:loop :for w :below word-count :do
                            (cl:let ((idx (cl:aref indices w)))
                              (cl:loop :for b :from 10 :downto 0 :do
                                (cl:setf (cl:aref bits (cl:+ (cl:* w 11) (cl:- 10 b)))
                                         (cl:ldb (cl:byte 1 b) idx)))))
                          ;; Extract entropy
                          (cl:let* ((entropy-bytes (cl:floor entropy-bits 8))
                                    (entropy (cl:make-array entropy-bytes
                                                            :element-type '(cl:unsigned-byte 8))))
                            (cl:loop :for byte-idx :below entropy-bytes :do
                              (cl:let ((byte-val 0))
                                (cl:loop :for bit-idx :from 0 :below 8 :do
                                  (cl:setf byte-val
                                           (cl:+ (cl:ash byte-val 1)
                                                 (cl:aref bits (cl:+ (cl:* byte-idx 8) bit-idx)))))
                                (cl:setf (cl:aref entropy byte-idx) byte-val)))
                            ;; Compute expected checksum
                            (cl:let* ((sha-hash (ironclad:digest-sequence :sha256 entropy))
                                      (expected-cs (cl:ldb (cl:byte checksum-bits
                                                                    (cl:- 8 checksum-bits))
                                                          (cl:aref sha-hash 0)))
                                      (actual-cs 0))
                              ;; Extract actual checksum from bits
                              (cl:loop :for i :from entropy-bits :below total-bits :do
                                (cl:setf actual-cs (cl:+ (cl:ash actual-cs 1) (cl:aref bits i))))
                              (cl:if (cl:= expected-cs actual-cs)
                                     coalton:True
                                     coalton:False))))
                        coalton:False))))))

  (declare mnemonic-to-seed (String -> String -> types:Bytes))
  (define (mnemonic-to-seed mnemonic passphrase)
    "Convert mnemonic to 64-byte seed using PBKDF2.
     Passphrase can be empty string for no passphrase."
    (lisp types:Bytes (mnemonic passphrase)
      (cl:let* ((mnemonic-bytes (sb-ext:string-to-octets mnemonic :external-format :utf-8))
                (salt-str (cl:concatenate 'cl:string "mnemonic" passphrase))
                (salt-bytes (sb-ext:string-to-octets salt-str :external-format :utf-8))
                (mnemonic-arr (cl:make-array (cl:length mnemonic-bytes)
                                             :fill-pointer (cl:length mnemonic-bytes)
                                             :adjustable cl:t
                                             :initial-contents (cl:coerce mnemonic-bytes 'cl:list)))
                (salt-arr (cl:make-array (cl:length salt-bytes)
                                         :fill-pointer (cl:length salt-bytes)
                                         :adjustable cl:t
                                         :initial-contents (cl:coerce salt-bytes 'cl:list))))
        (web3/hdwallet::pbkdf2-sha512 mnemonic-arr salt-arr 2048))))

  ;;; =========================================================================
  ;;; BIP-32 HD Key Types
  ;;; =========================================================================

  (define-struct HDKey
    "Hierarchical Deterministic key."
    (hd-private-key types:Bytes)    ; 32 bytes
    (hd-public-key types:Bytes)     ; 33 bytes compressed
    (hd-chain-code types:Bytes))    ; 32 bytes

  ;;; =========================================================================
  ;;; BIP-32 Key Derivation
  ;;; =========================================================================

  (declare master-key-from-seed (types:Bytes -> (types:Web3Result HDKey)))
  (define (master-key-from-seed seed)
    "Derive master key from seed using HMAC-SHA512."
    (if (< (types:bytes-length seed) 16)
        (Err (types:WalletError "Seed too short"))
        (let ((key-str (lisp types:Bytes ()
                         (cl:let* ((key "Bitcoin seed")
                                   (bytes (sb-ext:string-to-octets key :external-format :utf-8))
                                   (arr (cl:make-array (cl:length bytes)
                                                       :fill-pointer (cl:length bytes)
                                                       :adjustable cl:t
                                                       :initial-contents (cl:coerce bytes 'cl:list))))
                           arr))))
          (let ((i (hmac-sha512 key-str seed)))
            (let ((priv-key (types:bytes-slice 0 32 i))
                  (chain-code (types:bytes-slice 32 32 i)))
              (match (crypto:private-key-to-public-key priv-key)
                ((Ok pub-key)
                 ;; Compress public key
                 (let ((compressed (compress-public-key pub-key)))
                   (Ok (HDKey priv-key compressed chain-code))))
                ((Err e) (Err e))))))))

  (declare compress-public-key (types:Bytes -> types:Bytes))
  (define (compress-public-key pub-key)
    "Compress a 65-byte uncompressed public key to 33 bytes."
    (lisp types:Bytes (pub-key)
      (cl:if (cl:= (cl:length pub-key) 33)
             pub-key  ; Already compressed
             (cl:let* ((result (cl:make-array 33 :fill-pointer 33 :adjustable cl:t))
                       (y-last-byte (cl:aref pub-key 64))
                       (prefix (cl:if (cl:evenp y-last-byte) #x02 #x03)))
               (cl:setf (cl:aref result 0) prefix)
               (cl:replace result pub-key :start1 1 :start2 1 :end2 33)
               result))))

  ;; Helper to derive child key bytes (returns child-priv, child-chain as tuple or error)
  (declare derive-child-bytes (HDKey -> UFix -> (types:Web3Result (Tuple types:Bytes types:Bytes))))
  (define (derive-child-bytes parent actual-index)
    "Derive child key bytes. Returns (private-key, chain-code) tuple."
    ;; Extract struct fields in Coalton, then pass to lisp
    (let ((priv-key (.hd-private-key parent))
          (pub-key (.hd-public-key parent))
          (chain-code (.hd-chain-code parent)))
      (lisp (types:Web3Result (Tuple types:Bytes types:Bytes)) (priv-key pub-key chain-code actual-index)
        (cl:let* (
                (hardened-p (cl:>= actual-index #x80000000))
                ;; Build data for HMAC
                (data (cl:if hardened-p
                             ;; Hardened: 0x00 || private_key || index
                             (cl:let ((d (cl:make-array 37 :element-type '(cl:unsigned-byte 8))))
                               (cl:setf (cl:aref d 0) 0)
                               (cl:replace d priv-key :start1 1)
                               (cl:setf (cl:aref d 33) (cl:ldb (cl:byte 8 24) actual-index))
                               (cl:setf (cl:aref d 34) (cl:ldb (cl:byte 8 16) actual-index))
                               (cl:setf (cl:aref d 35) (cl:ldb (cl:byte 8 8) actual-index))
                               (cl:setf (cl:aref d 36) (cl:ldb (cl:byte 8 0) actual-index))
                               d)
                             ;; Normal: public_key || index
                             (cl:let ((d (cl:make-array 37 :element-type '(cl:unsigned-byte 8))))
                               (cl:replace d pub-key)
                               (cl:setf (cl:aref d 33) (cl:ldb (cl:byte 8 24) actual-index))
                               (cl:setf (cl:aref d 34) (cl:ldb (cl:byte 8 16) actual-index))
                               (cl:setf (cl:aref d 35) (cl:ldb (cl:byte 8 8) actual-index))
                               (cl:setf (cl:aref d 36) (cl:ldb (cl:byte 8 0) actual-index))
                               d)))
                (data-arr (cl:make-array (cl:length data)
                                         :fill-pointer (cl:length data)
                                         :adjustable cl:t
                                         :initial-contents (cl:coerce data 'cl:list)))
                (chain-arr (cl:make-array 32 :fill-pointer 32 :adjustable cl:t
                                          :initial-contents (cl:coerce chain-code 'cl:list)))
                (i (web3/hdwallet::hmac-sha512 chain-arr data-arr))
                (il (cl:subseq i 0 32))
                (ir (cl:subseq i 32 64))
                ;; Add il to parent private key (mod n)
                (secp256k1-n (cl:parse-integer
                              "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141"
                              :radix 16))
                (il-int (cl:reduce (cl:lambda (acc b) (cl:+ (cl:ash acc 8) b))
                                   il :initial-value 0))
                (priv-int (cl:reduce (cl:lambda (acc b) (cl:+ (cl:ash acc 8) b))
                                     priv-key :initial-value 0))
                (child-int (cl:mod (cl:+ il-int priv-int) secp256k1-n)))
        ;; Convert back to bytes
        (cl:if (cl:or (cl:zerop il-int) (cl:>= il-int secp256k1-n))
               (Err (web3/types:WalletError "Invalid child key"))
               (cl:let* ((child-priv (cl:make-array 32 :fill-pointer 32 :adjustable cl:t))
                         (child-chain (cl:make-array 32 :fill-pointer 32 :adjustable cl:t)))
                 ;; Convert integer to bytes
                 (cl:loop :for i :from 31 :downto 0 :do
                   (cl:setf (cl:aref child-priv i) (cl:ldb (cl:byte 8 (cl:* (cl:- 31 i) 8)) child-int)))
                 (cl:replace child-chain ir)
                 (Ok (Tuple child-priv child-chain))))))))

  (declare derive-child (HDKey -> UFix -> Boolean -> (types:Web3Result HDKey)))
  (define (derive-child parent index hardened)
    "Derive child key at index. If hardened is true, uses hardened derivation."
    (let ((actual-index (if hardened
                            (+ index #x80000000)
                            index)))
      (match (derive-child-bytes parent actual-index)
        ((Err e) (Err e))
        ((Ok (Tuple child-priv child-chain))
         ;; Get public key using Coalton pattern matching
         (match (crypto:private-key-to-public-key child-priv)
           ((Err e) (Err e))
           ((Ok pub-key)
            (Ok (HDKey child-priv (compress-public-key pub-key) child-chain))))))))
  (declare parse-path (String -> (types:Web3Result (List (Tuple UFix Boolean)))))
  (define (parse-path path)
    "Parse a derivation path like 'm/44'/60'/0'/0/0' into list of (index, hardened) pairs."
    (lisp (types:Web3Result (List (Tuple UFix Boolean))) (path)
      (cl:handler-case
          (cl:let* ((parts (uiop:split-string path :separator "/"))
                    (parts (cl:remove-if (cl:lambda (s) (cl:zerop (cl:length s))) parts)))
            ;; Skip 'm' if present
            (cl:when (cl:and parts (cl:string-equal (cl:first parts) "m"))
              (cl:setf parts (cl:rest parts)))
            ;; Parse each component
            (cl:let ((result coalton:Nil))
              (cl:dolist (part (cl:reverse parts))
                (cl:let* ((hardened (cl:or (cl:char= (cl:char part (cl:1- (cl:length part))) #\')
                                           (cl:char= (cl:char part (cl:1- (cl:length part))) #\h)))
                          (num-str (cl:if hardened
                                         (cl:subseq part 0 (cl:1- (cl:length part)))
                                         part))
                          (num (cl:parse-integer num-str)))
                  (cl:setf result (coalton:Cons (coalton-prelude:Tuple num hardened) result))))
              (Ok result)))
        (cl:error (e)
          (cl:declare (cl:ignore e))
          (Err (web3/types:WalletError "Invalid derivation path"))))))

  ;; Helper to recursively derive through path components
  (declare derive-path-rec (HDKey -> (List (Tuple UFix Boolean)) -> (types:Web3Result HDKey)))
  (define (derive-path-rec key components)
    "Recursively derive through path components."
    (match components
      ((Nil) (Ok key))
      ((Cons comp rest)
       (let ((idx (fst comp))
             (hard (snd comp)))
         (match (derive-child key idx hard)
           ((Err e) (Err e))
           ((Ok child-key) (derive-path-rec child-key rest)))))))

  (declare derive-path (HDKey -> String -> (types:Web3Result HDKey)))
  (define (derive-path master-key path)
    "Derive key at path from master key."
    (match (parse-path path)
      ((Err e) (Err e))
      ((Ok components) (derive-path-rec master-key components))))

  ;;; =========================================================================
  ;;; Ethereum Helpers
  ;;; =========================================================================

  (declare ethereum-path (UFix -> String))
  (define (ethereum-path account-index)
    "Standard Ethereum derivation path: m/44'/60'/0'/0/index"
    (lisp String (account-index)
      (cl:format cl:nil "m/44'/60'/0'/0/~D" account-index)))

  (declare ethereum-ledger-path (UFix -> String))
  (define (ethereum-ledger-path account-index)
    "Ledger Ethereum derivation path: m/44'/60'/index'/0/0"
    (lisp String (account-index)
      (cl:format cl:nil "m/44'/60'/~D'/0/0" account-index)))

  (declare derive-ethereum-key (types:Bytes -> UFix -> (types:Web3Result HDKey)))
  (define (derive-ethereum-key seed account-index)
    "Derive Ethereum key at standard path m/44'/60'/0'/0/index."
    (match (master-key-from-seed seed)
      ((Err e) (Err e))
      ((Ok master) (derive-path master (ethereum-path account-index)))))

  (declare derive-ethereum-address (types:Bytes -> UFix -> (types:Web3Result addr:Address)))
  (define (derive-ethereum-address seed account-index)
    "Derive Ethereum address at standard path."
    (match (derive-ethereum-key seed account-index)
      ((Err e) (Err e))
      ((Ok hd-key)
       ;; Need to get uncompressed public key for address derivation
       (match (crypto:private-key-to-public-key (.hd-private-key hd-key))
         ((Err e) (Err e))
         ((Ok pub-key) (addr:address-from-public-key pub-key))))))

  (declare mnemonic-to-private-key (String -> UFix -> (types:Web3Result types:Bytes)))
  (define (mnemonic-to-private-key mnemonic account-index)
    "Derive private key from mnemonic at standard Ethereum path."
    (let ((seed (mnemonic-to-seed mnemonic "")))
      (match (derive-ethereum-key seed account-index)
        ((Err e) (Err e))
        ((Ok hd-key) (Ok (.hd-private-key hd-key))))))

  (declare mnemonic-to-address (String -> UFix -> (types:Web3Result addr:Address)))
  (define (mnemonic-to-address mnemonic account-index)
    "Derive Ethereum address from mnemonic at standard path."
    (let ((seed (mnemonic-to-seed mnemonic "")))
      (derive-ethereum-address seed account-index))))
