;;; ERC-165 Standard Interface Detection tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; ERC-165 Tests
;;; =========================================================================

(defun run-erc165-tests ()
  (format t "~%=== ERC-165 Tests ===~%")

  ;;; =========================================================================
  ;;; Selector Tests
  ;;; =========================================================================

  (test-case "selector-supports-interface is 4 bytes"
    (let ((sel (coalton:coalton web3/erc165:selector-supports-interface)))
      (assert (= (length sel) 4))))

  (test-case "selector-supports-interface matches 0x01ffc9a7"
    (let ((sel (coalton:coalton web3/erc165:selector-supports-interface)))
      (assert (= (aref sel 0) #x01))
      (assert (= (aref sel 1) #xff))
      (assert (= (aref sel 2) #xc9))
      (assert (= (aref sel 3) #xa7))))

  ;;; =========================================================================
  ;;; Interface ID Tests
  ;;; =========================================================================

  (test-case "interface-erc165 is 0x01ffc9a7"
    (let ((id (coalton:coalton web3/erc165:interface-erc165)))
      (assert (= (length id) 4))
      (assert (= (aref id 0) #x01))
      (assert (= (aref id 1) #xff))
      (assert (= (aref id 2) #xc9))
      (assert (= (aref id 3) #xa7))))

  (test-case "interface-erc721 is 0x80ac58cd"
    (let ((id (coalton:coalton web3/erc165:interface-erc721)))
      (assert (= (length id) 4))
      (assert (= (aref id 0) #x80))
      (assert (= (aref id 1) #xac))
      (assert (= (aref id 2) #x58))
      (assert (= (aref id 3) #xcd))))

  (test-case "interface-erc721-metadata is 0x5b5e139f"
    (let ((id (coalton:coalton web3/erc165:interface-erc721-metadata)))
      (assert (= (length id) 4))
      (assert (= (aref id 0) #x5b))
      (assert (= (aref id 1) #x5e))
      (assert (= (aref id 2) #x13))
      (assert (= (aref id 3) #x9f))))

  (test-case "interface-erc721-enumerable is 0x780e9d63"
    (let ((id (coalton:coalton web3/erc165:interface-erc721-enumerable)))
      (assert (= (length id) 4))
      (assert (= (aref id 0) #x78))
      (assert (= (aref id 1) #x0e))
      (assert (= (aref id 2) #x9d))
      (assert (= (aref id 3) #x63))))

  (test-case "interface-erc1155 is 0xd9b67a26"
    (let ((id (coalton:coalton web3/erc165:interface-erc1155)))
      (assert (= (length id) 4))
      (assert (= (aref id 0) #xd9))
      (assert (= (aref id 1) #xb6))
      (assert (= (aref id 2) #x7a))
      (assert (= (aref id 3) #x26))))

  (test-case "interface-erc1155-metadata-uri is 0x0e89341c"
    (let ((id (coalton:coalton web3/erc165:interface-erc1155-metadata-uri)))
      (assert (= (length id) 4))
      (assert (= (aref id 0) #x0e))
      (assert (= (aref id 1) #x89))
      (assert (= (aref id 2) #x34))
      (assert (= (aref id 3) #x1c))))

  (test-case "interface-erc20 is 0x36372b07"
    (let ((id (coalton:coalton web3/erc165:interface-erc20)))
      (assert (= (length id) 4))
      (assert (= (aref id 0) #x36))
      (assert (= (aref id 1) #x37))
      (assert (= (aref id 2) #x2b))
      (assert (= (aref id 3) #x07))))

  (test-case "all interface IDs are distinct"
    (let ((ids (list (coalton:coalton web3/erc165:interface-erc165)
                     (coalton:coalton web3/erc165:interface-erc721)
                     (coalton:coalton web3/erc165:interface-erc721-metadata)
                     (coalton:coalton web3/erc165:interface-erc721-enumerable)
                     (coalton:coalton web3/erc165:interface-erc1155)
                     (coalton:coalton web3/erc165:interface-erc1155-metadata-uri)
                     (coalton:coalton web3/erc165:interface-erc20))))
      (assert (= (length (remove-duplicates ids :test #'equalp)) 7))))

  ;;; =========================================================================
  ;;; Integration Tests (require WEB3_TEST_RPC_URL)
  ;;; =========================================================================

  (let ((rpc-url (uiop:getenv "WEB3_TEST_RPC_URL")))
    (if rpc-url
        (progn
          (test-case "USDC supports ERC-165"
            (let* ((provider (coalton:coalton
                              (web3/provider:make-http-provider
                               (coalton:lisp coalton:String () rpc-url))))
                   (usdc (result-value
                          (coalton:coalton
                           (web3/address:address-from-hex
                            "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"))))
                   (result (coalton:coalton
                            (web3/erc165:supports-interface
                             (coalton:lisp web3/provider:HttpProvider () provider)
                             (coalton:lisp web3/address:Address () usdc)
                             (coalton:lisp web3/types:Bytes ()
                               (coalton:coalton web3/erc165:interface-erc165))))))
              (assert (result-ok-p result))))

          (test-case "Bored Ape (ERC-721) supports ERC-721 interface"
            (let* ((provider (coalton:coalton
                              (web3/provider:make-http-provider
                               (coalton:lisp coalton:String () rpc-url))))
                   (bayc (result-value
                          (coalton:coalton
                           (web3/address:address-from-hex
                            "0xBC4CA0EdA7647A8aB7C2061c2E118A18a936f13D"))))
                   (result (coalton:coalton
                            (web3/erc165:supports-interface
                             (coalton:lisp web3/provider:HttpProvider () provider)
                             (coalton:lisp web3/address:Address () bayc)
                             (coalton:lisp web3/types:Bytes ()
                               (coalton:coalton web3/erc165:interface-erc721))))))
              (assert (result-ok-p result))
              (assert (eq (result-value result) coalton:True)))))
        (format t "~%  Note: Set WEB3_TEST_RPC_URL for ERC-165 integration tests~%"))))
