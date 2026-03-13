;;; ERC-1155 Metadata tests - Pure Common Lisp

(in-package #:web3-tests/runner)

(defun run-erc1155-metadata-tests ()
  (format t "~%=== ERC-1155 Metadata Tests ===~%")

  ;;; =========================================================================
  ;;; URI Template Substitution Tests
  ;;; =========================================================================

  (test-case "substitute-token-id replaces {id} with hex token ID"
    (let ((result (coalton:coalton
                   (web3/erc1155-metadata:substitute-token-id
                    "https://example.com/{id}.json"
                    (web3/types:u256-from-integer 1)))))
      ;; Token ID 1 = 64-char hex: 0000...0001
      (assert (string= result
                        "https://example.com/0000000000000000000000000000000000000000000000000000000000000001.json"))))

  (test-case "substitute-token-id handles URI without {id} placeholder"
    (let ((result (coalton:coalton
                   (web3/erc1155-metadata:substitute-token-id
                    "https://example.com/metadata.json"
                    (web3/types:u256-from-integer 42)))))
      (assert (string= result "https://example.com/metadata.json"))))

  (test-case "substitute-token-id handles token ID 0"
    (let ((result (coalton:coalton
                   (web3/erc1155-metadata:substitute-token-id
                    "https://api.example.com/token/{id}"
                    web3/types:u256-zero))))
      (assert (string= result
                        "https://api.example.com/token/0000000000000000000000000000000000000000000000000000000000000000"))))

  (test-case "substitute-token-id handles large token ID"
    (let ((result (coalton:coalton
                   (web3/erc1155-metadata:substitute-token-id
                    "https://example.com/{id}"
                    (web3/types:u256-from-integer 255)))))
      (assert (string= result
                        "https://example.com/00000000000000000000000000000000000000000000000000000000000000ff"))))

  (test-case "substitute-token-id replaces multiple {id} occurrences"
    (let ((result (coalton:coalton
                   (web3/erc1155-metadata:substitute-token-id
                    "{id}/{id}"
                    (web3/types:u256-from-integer 1)))))
      (let ((hex-one "0000000000000000000000000000000000000000000000000000000000000001"))
        (assert (string= result (concatenate 'string hex-one "/" hex-one))))))

  ;;; =========================================================================
  ;;; Metadata Fetching via data: URI (unit test, no network)
  ;;; =========================================================================

  (test-case "fetch-metadata-json works for ERC-1155 style JSON via data URI"
    (let* ((json "{\"name\":\"Gold Sword\",\"description\":\"A shiny gold sword\",\"image\":\"https://example.com/sword.png\",\"attributes\":[{\"trait_type\":\"Damage\",\"value\":42},{\"trait_type\":\"Element\",\"value\":\"Fire\"}]}")
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
                          "Gold Sword"))
        (assert (string= (coalton:coalton
                           (web3/erc721-metadata:nft-metadata-description
                            (coalton:lisp web3/erc721-metadata:NftMetadata () meta)))
                          "A shiny gold sword"))
        (assert (string= (coalton:coalton
                           (web3/erc721-metadata:nft-metadata-image
                            (coalton:lisp web3/erc721-metadata:NftMetadata () meta)))
                          "https://example.com/sword.png"))
        ;; Check attributes
        (let ((attrs (coalton:coalton
                      (web3/erc721-metadata:nft-metadata-attributes
                       (coalton:lisp web3/erc721-metadata:NftMetadata () meta)))))
          (assert (= (length attrs) 2))
          (let ((attr1 (elt attrs 0)))
            (assert (string= (coalton:coalton
                               (web3/erc721-metadata:nft-attribute-trait-type
                                (coalton:lisp web3/erc721-metadata:NftAttribute () attr1)))
                              "Damage"))
            (assert (string= (coalton:coalton
                               (web3/erc721-metadata:nft-attribute-value
                                (coalton:lisp web3/erc721-metadata:NftAttribute () attr1)))
                              "42"))))))))
