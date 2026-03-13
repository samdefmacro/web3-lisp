;;; ERC-721 Metadata tests - Pure Common Lisp

(in-package #:web3-tests/runner)

;;; =========================================================================
;;; Enumerable Selector Tests
;;; =========================================================================

(defun run-erc721-metadata-tests ()
  (format t "~%=== ERC-721 Metadata & Enumerable Tests ===~%")

  ;; Enumerable selectors
  (test-case "selector totalSupply() = 0x18160ddd"
    (let ((selector (coalton:coalton web3/erc721:selector-total-supply)))
      (assert (= (length selector) 4))
      (assert (= (aref selector 0) #x18))
      (assert (= (aref selector 1) #x16))
      (assert (= (aref selector 2) #x0d))
      (assert (= (aref selector 3) #xdd))))

  (test-case "selector tokenByIndex(uint256) = 0x4f6ccce7"
    (let ((selector (coalton:coalton web3/erc721:selector-token-by-index)))
      (assert (= (length selector) 4))
      (assert (= (aref selector 0) #x4f))
      (assert (= (aref selector 1) #x6c))
      (assert (= (aref selector 2) #xcc))
      (assert (= (aref selector 3) #xe7))))

  (test-case "selector tokenOfOwnerByIndex(address,uint256) = 0x2f745c59"
    (let ((selector (coalton:coalton web3/erc721:selector-token-of-owner-by-index)))
      (assert (= (length selector) 4))
      (assert (= (aref selector 0) #x2f))
      (assert (= (aref selector 1) #x74))
      (assert (= (aref selector 2) #x5c))
      (assert (= (aref selector 3) #x59))))

  ;;; =========================================================================
  ;;; NftAttribute Type Tests
  ;;; =========================================================================

  (test-case "NftAttribute accessors"
    (let ((attr (coalton:coalton
                 (web3/erc721-metadata:NftAttribute "Background" "Blue"))))
      (assert (string= (coalton:coalton
                         (web3/erc721-metadata:nft-attribute-trait-type
                          (coalton:lisp web3/erc721-metadata:NftAttribute () attr)))
                        "Background"))
      (assert (string= (coalton:coalton
                         (web3/erc721-metadata:nft-attribute-value
                          (coalton:lisp web3/erc721-metadata:NftAttribute () attr)))
                        "Blue"))))

  ;;; =========================================================================
  ;;; NftMetadata Type Tests
  ;;; =========================================================================

  (test-case "NftMetadata accessors"
    (let ((meta (coalton:coalton
                 (web3/erc721-metadata:NftMetadata
                  "Cool NFT"
                  "A cool description"
                  "https://example.com/image.png"
                  "https://example.com"
                  ""
                  coalton:Nil))))
      (assert (string= (coalton:coalton
                         (web3/erc721-metadata:nft-metadata-name
                          (coalton:lisp web3/erc721-metadata:NftMetadata () meta)))
                        "Cool NFT"))
      (assert (string= (coalton:coalton
                         (web3/erc721-metadata:nft-metadata-description
                          (coalton:lisp web3/erc721-metadata:NftMetadata () meta)))
                        "A cool description"))
      (assert (string= (coalton:coalton
                         (web3/erc721-metadata:nft-metadata-image
                          (coalton:lisp web3/erc721-metadata:NftMetadata () meta)))
                        "https://example.com/image.png"))
      (assert (string= (coalton:coalton
                         (web3/erc721-metadata:nft-metadata-external-url
                          (coalton:lisp web3/erc721-metadata:NftMetadata () meta)))
                        "https://example.com"))
      (assert (string= (coalton:coalton
                         (web3/erc721-metadata:nft-metadata-animation-url
                          (coalton:lisp web3/erc721-metadata:NftMetadata () meta)))
                        ""))))

  ;;; =========================================================================
  ;;; IPFS Gateway URL Tests
  ;;; =========================================================================

  (test-case "ipfs-to-gateway-url converts ipfs:// URI"
    (let ((url (coalton:coalton
                (web3/erc721-metadata:ipfs-to-gateway-url
                 "ipfs://QmXoypizjW3WknFiJnKLwHCnL72vedxjQkDDP1mXWo6uco"))))
      (assert (string= url "https://ipfs.io/ipfs/QmXoypizjW3WknFiJnKLwHCnL72vedxjQkDDP1mXWo6uco"))))

  (test-case "ipfs-to-gateway-url passes through http:// URI"
    (let ((url (coalton:coalton
                (web3/erc721-metadata:ipfs-to-gateway-url
                 "https://example.com/metadata.json"))))
      (assert (string= url "https://example.com/metadata.json"))))

  ;;; =========================================================================
  ;;; Data URI Parsing Tests
  ;;; =========================================================================

  (test-case "parse-data-uri decodes base64 JSON"
    (let* ((json "{\"name\":\"Test\"}")
           (b64-str (cl-base64:string-to-base64-string json))
           (data-uri (concatenate 'string "data:application/json;base64," b64-str))
           (result (coalton:coalton
                    (web3/erc721-metadata:parse-data-uri
                     (coalton:lisp coalton:String () data-uri)))))
      (assert (is-ok result))
      (let ((decoded (result-value result)))
        (assert (string= decoded json)))))

  (test-case "parse-data-uri decodes plain text"
    (let* ((json "{\"name\":\"Test\"}")
           (data-uri (concatenate 'string "data:application/json," json))
           (result (coalton:coalton
                    (web3/erc721-metadata:parse-data-uri
                     (coalton:lisp coalton:String () data-uri)))))
      (assert (is-ok result))
      (let ((decoded (result-value result)))
        (assert (string= decoded json)))))

  ;;; =========================================================================
  ;;; Metadata JSON Parsing Tests (via data: URIs)
  ;;; =========================================================================

  (test-case "fetch-metadata-json parses basic metadata from data URI"
    (let* ((json "{\"name\":\"Bored Ape\",\"description\":\"A bored ape\",\"image\":\"ipfs://QmAbc\"}")
           (b64 (cl-base64:string-to-base64-string json))
           (data-uri (concatenate 'string "data:application/json;base64," b64))
           (result (coalton:coalton
                    (web3/erc721-metadata:fetch-metadata-json
                     (coalton:lisp coalton:String () data-uri)))))
      (assert (is-ok result))
      (let ((meta (result-value result)))
        (assert (string= (coalton:coalton
                           (web3/erc721-metadata:nft-metadata-name
                            (coalton:lisp web3/erc721-metadata:NftMetadata () meta)))
                          "Bored Ape"))
        (assert (string= (coalton:coalton
                           (web3/erc721-metadata:nft-metadata-description
                            (coalton:lisp web3/erc721-metadata:NftMetadata () meta)))
                          "A bored ape"))
        (assert (string= (coalton:coalton
                           (web3/erc721-metadata:nft-metadata-image
                            (coalton:lisp web3/erc721-metadata:NftMetadata () meta)))
                          "ipfs://QmAbc")))))

  (test-case "fetch-metadata-json parses attributes"
    (let* ((json "{\"name\":\"NFT\",\"description\":\"\",\"image\":\"\",\"attributes\":[{\"trait_type\":\"Background\",\"value\":\"Blue\"},{\"trait_type\":\"Rarity\",\"value\":\"Legendary\"}]}")
           (data-uri (concatenate 'string "data:application/json," json))
           (result (coalton:coalton
                    (web3/erc721-metadata:fetch-metadata-json
                     (coalton:lisp coalton:String () data-uri)))))
      (assert (is-ok result))
      (let* ((meta (result-value result))
             (attrs (coalton:coalton
                     (web3/erc721-metadata:nft-metadata-attributes
                      (coalton:lisp web3/erc721-metadata:NftMetadata () meta)))))
        ;; Should have 2 attributes
        (assert (= (length attrs) 2))
        ;; First attribute
        (let ((attr1 (elt attrs 0)))
          (assert (string= (coalton:coalton
                             (web3/erc721-metadata:nft-attribute-trait-type
                              (coalton:lisp web3/erc721-metadata:NftAttribute () attr1)))
                            "Background"))
          (assert (string= (coalton:coalton
                             (web3/erc721-metadata:nft-attribute-value
                              (coalton:lisp web3/erc721-metadata:NftAttribute () attr1)))
                            "Blue"))))))

  (test-case "fetch-metadata-json handles missing fields gracefully"
    (let* ((json "{\"name\":\"Minimal\"}")
           (data-uri (concatenate 'string "data:application/json," json))
           (result (coalton:coalton
                    (web3/erc721-metadata:fetch-metadata-json
                     (coalton:lisp coalton:String () data-uri)))))
      (assert (is-ok result))
      (let ((meta (result-value result)))
        (assert (string= (coalton:coalton
                           (web3/erc721-metadata:nft-metadata-name
                            (coalton:lisp web3/erc721-metadata:NftMetadata () meta)))
                          "Minimal"))
        ;; Missing fields default to empty string
        (assert (string= (coalton:coalton
                           (web3/erc721-metadata:nft-metadata-description
                            (coalton:lisp web3/erc721-metadata:NftMetadata () meta)))
                          ""))
        (assert (string= (coalton:coalton
                           (web3/erc721-metadata:nft-metadata-image
                            (coalton:lisp web3/erc721-metadata:NftMetadata () meta)))
                          "")))))

  (test-case "fetch-metadata-json handles numeric attribute values"
    (let* ((json "{\"name\":\"NFT\",\"description\":\"\",\"image\":\"\",\"attributes\":[{\"trait_type\":\"Level\",\"value\":42}]}")
           (data-uri (concatenate 'string "data:application/json," json))
           (result (coalton:coalton
                    (web3/erc721-metadata:fetch-metadata-json
                     (coalton:lisp coalton:String () data-uri)))))
      (assert (is-ok result))
      (let* ((meta (result-value result))
             (attrs (coalton:coalton
                     (web3/erc721-metadata:nft-metadata-attributes
                      (coalton:lisp web3/erc721-metadata:NftMetadata () meta)))))
        (assert (= (length attrs) 1))
        (let ((attr (elt attrs 0)))
          (assert (string= (coalton:coalton
                             (web3/erc721-metadata:nft-attribute-value
                              (coalton:lisp web3/erc721-metadata:NftAttribute () attr)))
                            "42")))))))
