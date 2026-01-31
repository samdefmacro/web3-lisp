;;;; BIP-39 Wordlist loader
;;;; Downloads or loads the standard English wordlist

(cl:in-package #:web3/hdwallet)

(cl:defvar *bip39-wordlist* cl:nil
  "BIP-39 English wordlist (2048 words)")

(cl:defvar *bip39-wordlist-url*
  "https://raw.githubusercontent.com/bitcoin/bips/master/bip-0039/english.txt"
  "URL to download BIP-39 English wordlist")

(cl:defun load-wordlist-from-string (text)
  "Parse wordlist from newline-separated string."
  (cl:let* ((words (uiop:split-string text :separator '(#\Newline #\Return)))
            (words (cl:remove-if (cl:lambda (s) (cl:zerop (cl:length s))) words)))
    (cl:make-array (cl:length words) :initial-contents words)))

(cl:defun load-wordlist-from-file (path)
  "Load wordlist from a file."
  (cl:let ((text (uiop:read-file-string path)))
    (load-wordlist-from-string text)))

(cl:defun download-wordlist ()
  "Download the BIP-39 English wordlist from GitHub."
  (cl:handler-case
      (cl:let ((response (dex:get *bip39-wordlist-url*)))
        ;; Response might be bytes or string, convert if necessary
        (cl:let ((text (cl:if (cl:stringp response)
                              response
                              (sb-ext:octets-to-string response :external-format :utf-8))))
          (load-wordlist-from-string text)))
    (cl:error (e)
      (cl:error "Failed to download BIP-39 wordlist: ~A" e))))

(cl:defun ensure-wordlist ()
  "Ensure the wordlist is loaded. Downloads if necessary."
  (cl:unless *bip39-wordlist*
    (cl:setf *bip39-wordlist* (download-wordlist)))
  *bip39-wordlist*)

;; Try to load wordlist on package load
(cl:handler-case
    (ensure-wordlist)
  (cl:error (e)
    (cl:warn "Could not load BIP-39 wordlist: ~A. Call (ensure-wordlist) manually." e)))
