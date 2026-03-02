;;; ENS (Ethereum Name Service) tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Namehash Tests
;;; =========================================================================

(defun run-ens-tests ()
  (format t "~%=== ENS Tests ===~%")

  ;; Test vectors from EIP-137: https://eips.ethereum.org/EIPS/eip-137
  ;; namehash('') = 0x0000000000000000000000000000000000000000000000000000000000000000
  (test-case "namehash empty string = 0x00...00"
    (let ((hash (coalton:coalton (web3/ens:namehash ""))))
      (assert (= (length hash) 32))
      (assert (every #'zerop (coerce hash 'list)))))

  ;; namehash('eth') = 0x93cdeb708b7545dc668eb9280176169d1c33cfd8ed6f04690a0bcc88a93fc4ae
  (test-case "namehash 'eth'"
    (let ((hash (coalton:coalton (web3/ens:namehash "eth"))))
      (assert (= (length hash) 32))
      (assert (= (aref hash 0) #x93))
      (assert (= (aref hash 1) #xcd))
      (assert (= (aref hash 2) #xeb))
      (assert (= (aref hash 31) #xae))))

  ;; namehash('foo.eth') = 0xde9b09fd7c5f901e23a3f19fecc54828e9c848539801e86591bd9801b019f84f
  (test-case "namehash 'foo.eth'"
    (let ((hash (coalton:coalton (web3/ens:namehash "foo.eth"))))
      (assert (= (length hash) 32))
      (assert (= (aref hash 0) #xde))
      (assert (= (aref hash 1) #x9b))
      (assert (= (aref hash 2) #x09))
      (assert (= (aref hash 31) #x4f))))

  ;; Test a real ENS name: vitalik.eth
  (test-case "namehash 'vitalik.eth'"
    (let ((hash (coalton:coalton (web3/ens:namehash "vitalik.eth"))))
      (assert (= (length hash) 32))
      ;; 0xee6c4522aab0003e8d14cd40a6af439055fd2577951148c14b6cea9a53475835
      (assert (= (aref hash 0) #xee))
      (assert (= (aref hash 1) #x6c))
      (assert (= (aref hash 31) #x35))))

  ;; Test subdomain
  (test-case "namehash 'alice.vitalik.eth'"
    (let ((hash (coalton:coalton (web3/ens:namehash "alice.vitalik.eth"))))
      (assert (= (length hash) 32))))

  ;; Test namehash-hex returns correct format
  (test-case "namehash-hex returns 0x-prefixed hex"
    (let ((hex (coalton:coalton (web3/ens:namehash-hex "eth"))))
      (assert (= (length hex) 66))  ;; 0x + 64 hex chars
      (assert (string= (subseq hex 0 2) "0x"))
      (assert (string= (subseq hex 2 6) "93cd"))))

  ;;; =========================================================================
  ;;; Labelhash Tests
  ;;; =========================================================================

  (test-case "labelhash 'eth' = keccak256('eth')"
    (let ((label-hash (coalton:coalton (web3/ens:labelhash "eth")))
          (keccak-hash (coalton:coalton
                        (web3/crypto:keccak256
                         (web3/types:bytes-from-list
                          (coalton:Cons #x65 (coalton:Cons #x74 (coalton:Cons #x68 coalton:Nil))))))))
      (assert (= (length label-hash) 32))
      ;; labelhash should equal keccak256 of the ASCII bytes
      (assert (equalp (coerce label-hash 'list) (coerce keccak-hash 'list)))))

  ;;; =========================================================================
  ;;; Registry Selector Tests
  ;;; =========================================================================

  (test-case "resolver(bytes32) selector = 0x0178b8bf"
    (let ((selector (coalton:coalton web3/ens:ens-resolver-selector)))
      (assert (= (length selector) 4))
      (assert (= (aref selector 0) #x01))
      (assert (= (aref selector 1) #x78))
      (assert (= (aref selector 2) #xb8))
      (assert (= (aref selector 3) #xbf))))

  (test-case "owner(bytes32) selector = 0x02571be3"
    (let ((selector (coalton:coalton web3/ens:ens-owner-selector)))
      (assert (= (length selector) 4))
      (assert (= (aref selector 0) #x02))
      (assert (= (aref selector 1) #x57))
      (assert (= (aref selector 2) #x1b))
      (assert (= (aref selector 3) #xe3))))

  ;;; =========================================================================
  ;;; Resolver Selector Tests
  ;;; =========================================================================

  (test-case "addr(bytes32) selector = 0x3b3b57de"
    (let ((selector (coalton:coalton web3/ens:resolver-addr-selector)))
      (assert (= (length selector) 4))
      (assert (= (aref selector 0) #x3b))
      (assert (= (aref selector 1) #x3b))
      (assert (= (aref selector 2) #x57))
      (assert (= (aref selector 3) #xde))))

  (test-case "name(bytes32) selector = 0x691f3431"
    (let ((selector (coalton:coalton web3/ens:resolver-name-selector)))
      (assert (= (length selector) 4))
      (assert (= (aref selector 0) #x69))
      (assert (= (aref selector 1) #x1f))
      (assert (= (aref selector 2) #x34))
      (assert (= (aref selector 3) #x31))))

  (test-case "text(bytes32,string) selector = 0x59d1d43c"
    (let ((selector (coalton:coalton web3/ens:resolver-text-selector)))
      (assert (= (length selector) 4))
      (assert (= (aref selector 0) #x59))
      (assert (= (aref selector 1) #xd1))
      (assert (= (aref selector 2) #xd4))
      (assert (= (aref selector 3) #x3c))))

  ;;; =========================================================================
  ;;; Calldata Builder Tests
  ;;; =========================================================================

  (test-case "resolver-addr-calldata builds valid calldata"
    (let* ((node (coalton:coalton (web3/ens:namehash "vitalik.eth")))
           (calldata (coalton:coalton
                      (web3/ens:resolver-addr-calldata
                       (coalton:lisp web3/types:Bytes () node)))))
      ;; Should be 4 bytes selector + 32 bytes node = 36 bytes
      (assert (= (length calldata) 36))
      ;; First 4 bytes should be addr selector
      (assert (= (aref calldata 0) #x3b))
      (assert (= (aref calldata 1) #x3b))
      (assert (= (aref calldata 2) #x57))
      (assert (= (aref calldata 3) #xde))))

  (test-case "resolver-text-calldata builds valid calldata"
    (let* ((node (coalton:coalton (web3/ens:namehash "vitalik.eth")))
           (calldata (coalton:coalton
                      (web3/ens:resolver-text-calldata
                       (coalton:lisp web3/types:Bytes () node)
                       "avatar"))))
      ;; Should have selector + encoded bytes32 + encoded string
      (assert (> (length calldata) 36))
      ;; First 4 bytes should be text selector
      (assert (= (aref calldata 0) #x59))
      (assert (= (aref calldata 1) #xd1))))

  ;;; =========================================================================
  ;;; Reverse Resolution Tests
  ;;; =========================================================================

  (test-case "address-to-reverse-name formats correctly"
    (let ((reverse-name (coalton:coalton
                         (web3/ens:address-to-reverse-name
                          "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045"))))
      (assert (string= reverse-name
                       "d8da6bf26964af9d7eed9e03e53415d37aa96045.addr.reverse"))))

  (test-case "address-to-reverse-name handles no 0x prefix"
    (let ((reverse-name (coalton:coalton
                         (web3/ens:address-to-reverse-name
                          "d8dA6BF26964aF9D7eEd9e03E53415D37aA96045"))))
      (assert (string= reverse-name
                       "d8da6bf26964af9d7eed9e03e53415d37aa96045.addr.reverse"))))

  (test-case "reverse-node computes namehash of reverse name"
    (let ((node (coalton:coalton
                 (web3/ens:reverse-node
                  "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045"))))
      (assert (= (length node) 32))))

  ;;; =========================================================================
  ;;; Decode Result Tests
  ;;; =========================================================================

  (test-case "decode-address-result decodes valid address"
    ;; ABI-encoded address (32 bytes, address in last 20)
    (let* ((encoded (result-value
                     (coalton:coalton
                      (web3/types:hex-decode
                       "000000000000000000000000d8da6bf26964af9d7eed9e03e53415d37aa96045"))))
           (result (coalton:coalton
                    (web3/ens:decode-address-result
                     (coalton:lisp web3/types:Bytes () encoded)))))
      (assert (is-ok result))
      (let ((addr-hex (coalton:coalton
                       (web3/address:address-to-hex
                        (coalton:lisp web3/address:Address () (result-value result))))))
        (assert (string-equal (string-downcase addr-hex)
                              "0xd8da6bf26964af9d7eed9e03e53415d37aa96045")))))

  (test-case "decode-address-result fails on short data"
    (let* ((encoded (result-value (coalton:coalton (web3/types:hex-decode "d8da6bf2"))))
           (result (coalton:coalton
                    (web3/ens:decode-address-result
                     (coalton:lisp web3/types:Bytes () encoded)))))
      (assert (not (is-ok result)))))

  ;;; =========================================================================
  ;;; Contract Address Tests
  ;;; =========================================================================

  (test-case "ENS registry address is correct"
    (let ((addr (coalton:coalton web3/ens:ens-registry-address)))
      (assert (string= addr "0x00000000000C2E074eC69A0dFb2997BA6C7d2e1e"))))

  (test-case "ENS public resolver address is correct"
    (let ((addr (coalton:coalton web3/ens:ens-public-resolver-address)))
      (assert (string= addr "0x231b0Ee14048e9dCcD1d247744d114a4EB5E8E63")))))
