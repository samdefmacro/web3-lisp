;;;; Revert reason decoding package definition
;;;; Decode Error(string) and Panic(uint256) from reverted calls

(defpackage #:web3/revert
  (:documentation "Decode Solidity revert reasons from call return data")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:abi #:web3/abi))
  (:export
   ;; Revert reason type
   #:RevertReason
   #:RevertString
   #:RevertPanic
   #:RevertCustom
   #:RevertEmpty

   ;; Decoding
   #:decode-revert-reason
   #:decode-revert-hex
   #:revert-reason-message

   ;; Panic code descriptions
   #:panic-code-description

   ;; Known selectors
   #:error-selector
   #:panic-selector))
