;;;; ENS (Ethereum Name Service) module
;;;;
;;;; Provides utilities for:
;;;; - Namehash computation (EIP-137)
;;;; - ENS registry interaction
;;;; - Resolver interaction
;;;; - Reverse resolution

(in-package #:web3/ens)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Internal Helpers
  ;;; =========================================================================

  (declare bytes-concat (types:Bytes -> types:Bytes -> types:Bytes))
  (define (bytes-concat a b)
    "Concatenate two byte arrays."
    (types:bytes-concat-many (Cons a (Cons b Nil))))

  (declare string-to-bytes (String -> types:Bytes))
  (define (string-to-bytes s)
    "Convert a string to UTF-8 bytes."
    (lisp types:Bytes (s)
      (cl:let* ((str (cl:the cl:string s))
                (len (cl:length str))
                (arr (cl:make-array len :fill-pointer len :adjustable cl:t)))
        (cl:dotimes (i len arr)
          (cl:setf (cl:aref arr i) (cl:char-code (cl:char str i)))))))

  ;;; =========================================================================
  ;;; Namehash (EIP-137)
  ;;; =========================================================================
  ;;;
  ;;; The namehash algorithm converts a domain name like "vitalik.eth" into
  ;;; a 32-byte node identifier used by ENS contracts.
  ;;;
  ;;; Algorithm:
  ;;;   namehash('') = 0x0000...0000 (32 zero bytes)
  ;;;   namehash(name) = keccak256(namehash(parent) ++ labelhash(label))
  ;;;
  ;;; where labelhash(label) = keccak256(label)

  (declare labelhash (String -> types:Bytes))
  (define (labelhash label)
    "Compute the keccak256 hash of a single label.
     Example: (labelhash \"eth\") => hash of 'eth'"
    (crypto:keccak256 (string-to-bytes label)))

  (declare namehash (String -> types:Bytes))
  (define (namehash name)
    "Compute the namehash of an ENS name.

     Examples:
       (namehash \"\")           => 0x00...00 (32 zero bytes)
       (namehash \"eth\")        => keccak256(0x00...00 ++ keccak256(\"eth\"))
       (namehash \"vitalik.eth\") => keccak256(namehash(\"eth\") ++ keccak256(\"vitalik\"))

     The name should be normalized (lowercase, no trailing dot)."
    (if (== name "")
        ;; Empty name returns 32 zero bytes
        (types:bytes-from-list
         (replicate 32 0))
        ;; Split into labels and compute recursively
        (let ((labels (split-name name)))
          (namehash-labels labels))))

  (declare namehash-hex (String -> String))
  (define (namehash-hex name)
    "Compute the namehash and return as hex string with 0x prefix."
    (let ((hash (namehash name)))
      (<> "0x" (types:hex-encode hash))))

  (declare namehash-labels ((List String) -> types:Bytes))
  (define (namehash-labels labels)
    "Compute namehash from a list of labels (in reverse order of the domain).
     E.g., for 'vitalik.eth', labels would be (Cons \"vitalik\" (Cons \"eth\" Nil))"
    (match labels
      ((Nil)
       ;; Base case: empty produces 32 zero bytes
       (types:bytes-from-list (replicate 32 0)))
      ((Cons label rest)
       ;; Recursive case: hash(namehash(rest) ++ labelhash(label))
       (let ((parent-hash (namehash-labels rest))
             (label-hash (labelhash label)))
         (crypto:keccak256 (bytes-concat parent-hash label-hash))))))

  (declare split-name (String -> (List String)))
  (define (split-name name)
    "Split a domain name into labels.
     E.g., 'vitalik.eth' => (Cons \"vitalik\" (Cons \"eth\" Nil))"
    (lisp (List String) (name)
      (cl:labels ((build-list (parts)
                    (cl:if (cl:null parts)
                           coalton:Nil
                           (coalton:Cons (cl:car parts)
                                         (build-list (cl:cdr parts))))))
        (cl:let ((parts (uiop:split-string name :separator ".")))
          (build-list (cl:remove-if (cl:lambda (s) (cl:zerop (cl:length s))) parts))))))

  (declare replicate (UFix -> :a -> (List :a)))
  (define (replicate n val)
    "Create a list of n copies of val."
    (if (== n 0)
        Nil
        (Cons val (replicate (- n 1) val))))

  ;;; =========================================================================
  ;;; ENS Contract Addresses (Ethereum Mainnet)
  ;;; =========================================================================

  (declare ens-registry-address (Unit -> String))
  (define (ens-registry-address _)
    "ENS Registry contract address on Ethereum mainnet."
    "0x00000000000C2E074eC69A0dFb2997BA6C7d2e1e")

  (declare ens-public-resolver-address (Unit -> String))
  (define (ens-public-resolver-address _)
    "ENS Public Resolver contract address on Ethereum mainnet."
    "0x231b0Ee14048e9dCcD1d247744d114a4EB5E8E63")

  ;;; =========================================================================
  ;;; ENS Registry Interface
  ;;; =========================================================================
  ;;;
  ;;; The ENS Registry maps namehashes to owner/resolver addresses.
  ;;; Key functions:
  ;;;   resolver(bytes32 node) -> address
  ;;;   owner(bytes32 node) -> address

  (declare ens-resolver-selector (Unit -> types:Bytes))
  (define (ens-resolver-selector _)
    "Function selector for resolver(bytes32) = 0x0178b8bf"
    (abi:function-selector "resolver(bytes32)"))

  (declare ens-owner-selector (Unit -> types:Bytes))
  (define (ens-owner-selector _)
    "Function selector for owner(bytes32) = 0x02571be3"
    (abi:function-selector "owner(bytes32)"))

  (declare ens-resolver-calldata (types:Bytes -> types:Bytes))
  (define (ens-resolver-calldata node)
    "Build calldata for resolver(bytes32 node).
     Use this to find which resolver handles a name."
    (bytes-concat
     (ens-resolver-selector Unit)
     (abi:abi-encode (Cons (abi:AbiBytesFixedVal node) Nil))))

  (declare ens-owner-calldata (types:Bytes -> types:Bytes))
  (define (ens-owner-calldata node)
    "Build calldata for owner(bytes32 node).
     Use this to find who owns a name."
    (bytes-concat
     (ens-owner-selector Unit)
     (abi:abi-encode (Cons (abi:AbiBytesFixedVal node) Nil))))

  ;;; =========================================================================
  ;;; Resolver Interface
  ;;; =========================================================================
  ;;;
  ;;; Resolvers implement various resolution methods:
  ;;;   addr(bytes32 node) -> address                    (EIP-137)
  ;;;   name(bytes32 node) -> string                     (reverse resolution)
  ;;;   text(bytes32 node, string key) -> string         (text records)
  ;;;   contenthash(bytes32 node) -> bytes               (IPFS/Swarm hashes)

  (declare resolver-addr-selector (Unit -> types:Bytes))
  (define (resolver-addr-selector _)
    "Function selector for addr(bytes32) = 0x3b3b57de"
    (abi:function-selector "addr(bytes32)"))

  (declare resolver-name-selector (Unit -> types:Bytes))
  (define (resolver-name-selector _)
    "Function selector for name(bytes32) = 0x691f3431"
    (abi:function-selector "name(bytes32)"))

  (declare resolver-text-selector (Unit -> types:Bytes))
  (define (resolver-text-selector _)
    "Function selector for text(bytes32,string) = 0x59d1d43c"
    (abi:function-selector "text(bytes32,string)"))

  (declare resolver-contenthash-selector (Unit -> types:Bytes))
  (define (resolver-contenthash-selector _)
    "Function selector for contenthash(bytes32) = 0xbc1c58d1"
    (abi:function-selector "contenthash(bytes32)"))

  (declare resolver-addr-calldata (types:Bytes -> types:Bytes))
  (define (resolver-addr-calldata node)
    "Build calldata for addr(bytes32 node).
     Use this to resolve a name to an Ethereum address."
    (bytes-concat
     (resolver-addr-selector Unit)
     (abi:abi-encode (Cons (abi:AbiBytesFixedVal node) Nil))))

  (declare resolver-name-calldata (types:Bytes -> types:Bytes))
  (define (resolver-name-calldata node)
    "Build calldata for name(bytes32 node).
     Use this for reverse resolution (address -> name)."
    (bytes-concat
     (resolver-name-selector Unit)
     (abi:abi-encode (Cons (abi:AbiBytesFixedVal node) Nil))))

  (declare resolver-text-calldata (types:Bytes -> String -> types:Bytes))
  (define (resolver-text-calldata node key)
    "Build calldata for text(bytes32 node, string key).
     Common keys: 'email', 'url', 'avatar', 'description', 'com.twitter', etc."
    (bytes-concat
     (resolver-text-selector Unit)
     (abi:abi-encode (Cons (abi:AbiBytesFixedVal node)
                           (Cons (abi:AbiStringVal key) Nil)))))

  ;;; =========================================================================
  ;;; Reverse Resolution
  ;;; =========================================================================
  ;;;
  ;;; ENS supports reverse resolution: given an address, find its primary name.
  ;;; The reverse name is stored at: <address>.addr.reverse
  ;;; where <address> is the lowercase hex address without 0x prefix.

  (declare address-to-reverse-name (String -> String))
  (define (address-to-reverse-name addr-hex)
    "Convert an address to its reverse resolution name.
     E.g., '0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045'
           => 'd8da6bf26964af9d7eed9e03e53415d37aa96045.addr.reverse'"
    (let ((addr-lower (string-downcase (strip-0x addr-hex))))
      (<> addr-lower ".addr.reverse")))

  (declare reverse-node (String -> types:Bytes))
  (define (reverse-node addr-hex)
    "Compute the namehash for reverse resolution of an address.
     This is namehash('<address>.addr.reverse')."
    (namehash (address-to-reverse-name addr-hex)))

  (declare strip-0x (String -> String))
  (define (strip-0x s)
    "Remove 0x prefix from a hex string if present."
    (lisp String (s)
      (cl:if (cl:and (cl:>= (cl:length s) 2)
                     (cl:string= (cl:subseq s 0 2) "0x"))
             (cl:subseq s 2)
             s)))

  (declare string-downcase (String -> String))
  (define (string-downcase s)
    "Convert string to lowercase."
    (lisp String (s)
      (cl:string-downcase s)))

  ;;; =========================================================================
  ;;; Result Decoding
  ;;; =========================================================================

  (declare decode-address-result (types:Bytes -> (types:Web3Result addr:Address)))
  (define (decode-address-result data)
    "Decode an address from resolver call result.
     The result is a 32-byte ABI-encoded address."
    (if (< (types:bytes-length data) 32)
        (Err (types:AbiError "Invalid address result: too short"))
        ;; Address is in the last 20 bytes of the 32-byte slot
        (addr:address-from-bytes (types:bytes-slice 12 32 data))))

  (declare decode-name-result (types:Bytes -> (types:Web3Result String)))
  (define (decode-name-result data)
    "Decode a string from resolver call result.
     The result is an ABI-encoded dynamic string:
     - bytes 0-31: offset to string data (always 32 for single return)
     - bytes 32-63: string length
     - bytes 64+: string data"
    (if (< (types:bytes-length data) 64)
        (Err (types:AbiError "Invalid string result: too short"))
        ;; For single string return, offset is always 32
        ;; Read length at bytes 32-63 (value is in last byte for small strings)
        (let ((len (into (types:bytes-ref-unsafe 63 data))))
          (if (< (types:bytes-length data) (+ 64 len))
              (Err (types:AbiError "Invalid string result: truncated"))
              ;; Extract string bytes and convert to string
              (let ((str-bytes (types:bytes-slice 64 len data)))
                (Ok (bytes-to-string str-bytes)))))))

  (declare bytes-to-string (types:Bytes -> String))
  (define (bytes-to-string bytes)
    "Convert UTF-8 bytes to a string."
    (lisp String (bytes)
      (cl:let ((len (cl:length bytes)))
        (cl:with-output-to-string (out)
          (cl:dotimes (i len)
            (cl:write-char (cl:code-char (cl:aref bytes i)) out))))))

  (declare decode-text-result (types:Bytes -> (types:Web3Result String)))
  (define (decode-text-result data)
    "Decode a text record from resolver call result."
    (decode-name-result data)))
