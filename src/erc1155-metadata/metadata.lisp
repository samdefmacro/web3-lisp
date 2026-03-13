;;;; ERC-1155 Metadata Fetching
;;;;
;;;; ERC-1155 uses a URI template with {id} placeholder that must be
;;;; replaced with the hex-encoded token ID (lowercase, 64 chars, no 0x prefix).
;;;; The metadata JSON schema is the same as ERC-721 (name, description, image, attributes).
;;;;
;;;; See: https://eips.ethereum.org/EIPS/eip-1155#metadata

(in-package #:web3/erc1155-metadata)

(named-readtables:in-readtable coalton:coalton)

;;; =========================================================================
;;; CL Helpers
;;; =========================================================================

(cl:defun %substitute-id (uri hex-id)
  "Replace all occurrences of {id} in URI with hex-id."
  (cl:let ((target "{id}")
           (result uri)
           (target-len 4))
    (cl:loop
      (cl:let ((pos (cl:search target result)))
        (cl:if (cl:null pos)
               (cl:return result)
               (cl:setf result
                        (cl:concatenate 'cl:string
                                        (cl:subseq result 0 pos)
                                        hex-id
                                        (cl:subseq result (cl:+ pos target-len)))))))))

;;; =========================================================================
;;; Coalton Functions
;;; =========================================================================

(coalton-toplevel

  (declare substitute-token-id (String -> types:U256 -> String))
  (define (substitute-token-id uri token-id)
    "Substitute {id} in an ERC-1155 URI template with the hex-encoded token ID.
     The ID is zero-padded to 64 hex characters (32 bytes), lowercase, no 0x prefix.
     E.g., 'https://example.com/{id}.json' with token 1 =>
           'https://example.com/0000...0001.json'"
    (let ((hex-id (types:hex-encode (types:u256-to-bytes token-id))))
      (lisp String (uri hex-id)
        (%substitute-id uri hex-id))))

  (declare fetch-erc1155-metadata (provider:HttpProvider -> addr:Address -> types:U256 ->
                                    (types:Web3Result nft:NftMetadata)))
  (define (fetch-erc1155-metadata provider contract-address token-id)
    "Fetch and parse metadata for an ERC-1155 token.
     Calls uri(uint256) on the contract, substitutes {id} in the template,
     then fetches and parses the JSON metadata."
    (match (erc1155:erc1155-uri provider contract-address token-id)
      ((Err e) (Err e))
      ((Ok uri)
       (let ((resolved-uri (substitute-token-id uri token-id)))
         (nft:fetch-metadata-json resolved-uri))))))
