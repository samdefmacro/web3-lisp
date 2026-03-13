;;;; ERC-721 Metadata Fetching and Parsing
;;;;
;;;; Handles the full pipeline:
;;;;   1. Call tokenURI(uint256) on the contract
;;;;   2. Resolve the URI (HTTP, IPFS gateway, data: URI)
;;;;   3. Parse the JSON metadata (name, description, image, attributes)
;;;;
;;;; Supports:
;;;;   - HTTP/HTTPS URIs (fetched via dexador)
;;;;   - IPFS URIs (ipfs://... converted to gateway URL)
;;;;   - Data URIs (data:application/json;base64,...)

(in-package #:web3/erc721-metadata)

(named-readtables:in-readtable coalton:coalton)

;;; =========================================================================
;;; CL Helpers (outside coalton-toplevel)
;;; =========================================================================

(cl:defvar *ipfs-gateway* "https://ipfs.io/ipfs/"
  "Default IPFS gateway URL prefix.")

(cl:defun %ipfs-to-gateway (uri)
  "Convert ipfs:// URI to gateway URL, or return URI unchanged."
  (cl:if (cl:and (cl:>= (cl:length uri) 7)
                 (cl:string= uri "ipfs://" :end1 7))
         (cl:concatenate 'cl:string *ipfs-gateway* (cl:subseq uri 7))
         uri))

(cl:defun %fetch-uri (uri)
  "Fetch JSON string from a URI. Returns the response body as a string.
   Handles http://, https://, ipfs://, and data: URIs."
  (cl:cond
    ;; data:application/json;base64,...
    ((cl:and (cl:>= (cl:length uri) 5)
             (cl:string= uri "data:" :end1 5))
     (%decode-data-uri uri))
    ;; http, https, or ipfs (converted to gateway URL)
    (cl:t
     (dexador:get (%ipfs-to-gateway uri)))))

(cl:defun %decode-data-uri (uri)
  "Decode a data: URI. Supports base64 and plain text.
   data:application/json;base64,<data>
   data:application/json,<data>"
  (cl:let* ((after-data (cl:subseq uri 5))  ; strip 'data:'
            (comma-pos (cl:position #\, after-data)))
    (cl:if (cl:null comma-pos)
           (cl:error "Invalid data URI: no comma found")
           (cl:let ((media-part (cl:subseq after-data 0 comma-pos))
                    (data-part (cl:subseq after-data (cl:1+ comma-pos))))
             (cl:if (cl:search "base64" media-part)
                    ;; Base64-encoded
                    (cl-base64:base64-string-to-string data-part)
                    ;; Plain text
                    data-part)))))

(cl:defun %parse-metadata-json (json-string)
  "Parse NFT metadata JSON. Returns (values name description image external-url animation-url attributes).
   attributes is a list of (trait-type . value) dotted pairs."
  (cl:let* ((json (cl-json:decode-json-from-string json-string))
            (name (cl:or (cl:cdr (cl:assoc :name json)) ""))
            (description (cl:or (cl:cdr (cl:assoc :description json)) ""))
            (image (cl:or (cl:cdr (cl:assoc :image json)) ""))
            (external-url (cl:or (cl:cdr (cl:assoc :external--url json)) ""))
            (animation-url (cl:or (cl:cdr (cl:assoc :animation--url json)) ""))
            (attrs-raw (cl:cdr (cl:assoc :attributes json)))
            (attrs (cl:if (cl:listp attrs-raw)
                          (cl:mapcar
                           (cl:lambda (attr)
                             (cl:cons
                              (cl:or (cl:cdr (cl:assoc :trait--type attr)) "")
                              (cl:let ((val (cl:cdr (cl:assoc :value attr))))
                                (cl:typecase val
                                  (cl:string val)
                                  (cl:number (cl:format cl:nil "~A" val))
                                  (cl:t (cl:format cl:nil "~A" val))))))
                           attrs-raw)
                          cl:nil)))
    (cl:values name description image external-url animation-url attrs)))

;;; =========================================================================
;;; Coalton Types and Functions
;;; =========================================================================

(coalton-toplevel

  ;;; =========================================================================
  ;;; NFT Metadata Types
  ;;; =========================================================================

  (define-type NftAttribute
    "A single NFT trait/attribute (trait_type + value as strings)."
    (NftAttribute String String))

  (declare nft-attribute-trait-type (NftAttribute -> String))
  (define (nft-attribute-trait-type attr)
    (match attr ((NftAttribute tt _) tt)))

  (declare nft-attribute-value (NftAttribute -> String))
  (define (nft-attribute-value attr)
    (match attr ((NftAttribute _ v) v)))

  (define-type NftMetadata
    "Parsed ERC-721 token metadata (from tokenURI JSON)."
    (NftMetadata String     ; name
                 String     ; description
                 String     ; image
                 String     ; external_url
                 String     ; animation_url
                 (List NftAttribute)))  ; attributes

  (declare nft-metadata-name (NftMetadata -> String))
  (define (nft-metadata-name m)
    (match m ((NftMetadata n _ _ _ _ _) n)))

  (declare nft-metadata-description (NftMetadata -> String))
  (define (nft-metadata-description m)
    (match m ((NftMetadata _ d _ _ _ _) d)))

  (declare nft-metadata-image (NftMetadata -> String))
  (define (nft-metadata-image m)
    (match m ((NftMetadata _ _ i _ _ _) i)))

  (declare nft-metadata-external-url (NftMetadata -> String))
  (define (nft-metadata-external-url m)
    (match m ((NftMetadata _ _ _ e _ _) e)))

  (declare nft-metadata-animation-url (NftMetadata -> String))
  (define (nft-metadata-animation-url m)
    (match m ((NftMetadata _ _ _ _ a _) a)))

  (declare nft-metadata-attributes (NftMetadata -> (List NftAttribute)))
  (define (nft-metadata-attributes m)
    (match m ((NftMetadata _ _ _ _ _ attrs) attrs)))

  ;;; =========================================================================
  ;;; URI Helpers
  ;;; =========================================================================

  (declare ipfs-to-gateway-url (String -> String))
  (define (ipfs-to-gateway-url ipfs-uri)
    "Convert an ipfs:// URI to an IPFS gateway HTTP URL.
     E.g., ipfs://QmXyz... => https://ipfs.io/ipfs/QmXyz..."
    (lisp String (ipfs-uri)
      (%ipfs-to-gateway ipfs-uri)))

  (declare parse-data-uri (String -> (types:Web3Result String)))
  (define (parse-data-uri uri)
    "Decode a data: URI to its content string.
     Supports base64 and plain text data URIs."
    (lisp (types:Web3Result String) (uri)
      (cl:handler-case
          (coalton (Ok (lisp String () (%decode-data-uri uri))))
        (cl:error (e)
          (coalton (Err (types:ProviderError
                         (lisp String ()
                           (cl:format cl:nil "Data URI decode error: ~A" e)))))))))

  ;;; =========================================================================
  ;;; Metadata Fetching
  ;;; =========================================================================

  (declare fetch-metadata-json (String -> (types:Web3Result NftMetadata)))
  (define (fetch-metadata-json uri)
    "Fetch and parse NFT metadata from a URI.
     Supports http://, https://, ipfs://, and data: URIs."
    (lisp (types:Web3Result NftMetadata) (uri)
      (cl:handler-case
          (cl:let ((json-str (%fetch-uri uri)))
            (cl:multiple-value-bind (name description image external-url animation-url attrs)
                (%parse-metadata-json json-str)
              (cl:let ((coalton-attrs
                         (cl:reduce (cl:lambda (acc pair)
                                      (coalton:Cons
                                       (NftAttribute (cl:car pair) (cl:cdr pair))
                                       acc))
                                    (cl:reverse attrs)
                                    :initial-value coalton:Nil)))
                (coalton (Ok (NftMetadata
                              (lisp String () name)
                              (lisp String () description)
                              (lisp String () image)
                              (lisp String () external-url)
                              (lisp String () animation-url)
                              (lisp (List NftAttribute) () coalton-attrs)))))))
        (cl:error (e)
          (coalton (Err (types:ProviderError
                         (lisp String ()
                           (cl:format cl:nil "Metadata fetch error: ~A" e)))))))))

  (declare fetch-token-metadata (provider:HttpProvider -> addr:Address -> types:U256 ->
                                  (types:Web3Result NftMetadata)))
  (define (fetch-token-metadata provider nft-address token-id)
    "Fetch and parse metadata for an ERC-721 token.
     Calls tokenURI(uint256) on the contract, then fetches and parses the JSON."
    (match (erc721:erc721-token-uri provider nft-address token-id)
      ((Err e) (Err e))
      ((Ok uri) (fetch-metadata-json uri)))))
