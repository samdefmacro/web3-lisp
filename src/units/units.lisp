;;;; Units implementation
;;;; parseUnits/formatUnits with custom decimals

(in-package #:web3/units)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Core Functions
  ;;; =========================================================================

  (declare parse-units (String -> UFix -> (types:Web3Result types:U256)))
  (define (parse-units value-str decimals)
    "Parse a decimal string into U256 with specified decimals.
     Example: (parse-units \"1.5\" 18) -> 1500000000000000000"
    (lisp (types:Web3Result types:U256) (value-str decimals)
      (cl:block parse-units-block
        (cl:handler-case
            (cl:let* ((str (cl:string-trim " " value-str))
                      (negative-p (cl:and (cl:> (cl:length str) 0)
                                          (cl:char= (cl:char str 0) #\-))))
              ;; Check for negative values
              (cl:when negative-p
                (cl:return-from parse-units-block
                  (coalton-prelude:Err
                   (web3/types:HexError "Negative values not supported"))))
              ;; Remove leading + if present
              (cl:when (cl:and (cl:> (cl:length str) 0)
                               (cl:char= (cl:char str 0) #\+))
                (cl:setf str (cl:subseq str 1)))
              ;; Handle empty string
              (cl:when (cl:zerop (cl:length str))
                (cl:return-from parse-units-block
                  (coalton-prelude:Err
                   (web3/types:HexError "Empty value string"))))
              ;; Find decimal point
              (cl:let* ((dot-pos (cl:position #\. str))
                        (whole-str (cl:if dot-pos
                                          (cl:subseq str 0 dot-pos)
                                          str))
                        (frac-str (cl:if dot-pos
                                         (cl:subseq str (cl:1+ dot-pos))
                                         "")))
                ;; Validate parts contain only digits
                (cl:unless (cl:and (cl:or (cl:zerop (cl:length whole-str))
                                          (cl:every #'cl:digit-char-p whole-str))
                                   (cl:every #'cl:digit-char-p frac-str))
                  (cl:return-from parse-units-block
                    (coalton-prelude:Err
                     (web3/types:HexError "Invalid characters in value string"))))
                ;; Handle fractional part longer than decimals (truncate)
                (cl:let* ((frac-len (cl:length frac-str))
                          (frac-str-adj (cl:if (cl:> frac-len decimals)
                                               (cl:subseq frac-str 0 decimals)
                                               frac-str))
                          ;; Pad fractional part to decimals digits
                          (frac-padded (cl:concatenate 'cl:string
                                                       frac-str-adj
                                                       (cl:make-string
                                                        (cl:- decimals (cl:length frac-str-adj))
                                                        :initial-element #\0)))
                          ;; Parse whole and fractional parts
                          (whole (cl:if (cl:zerop (cl:length whole-str))
                                        0
                                        (cl:parse-integer whole-str)))
                          (frac (cl:if (cl:string= frac-padded "")
                                       0
                                       (cl:parse-integer frac-padded)))
                          ;; Compute final value
                          (multiplier (cl:expt 10 decimals))
                          (result (cl:+ (cl:* whole multiplier) frac)))
                  (coalton-prelude:Ok (web3/types:u256-from-integer result)))))
          (cl:error (e)
            (coalton-prelude:Err
             (web3/types:HexError (cl:format cl:nil "Invalid value string: ~A" e))))))))

  (declare format-units (types:U256 -> UFix -> String))
  (define (format-units value decimals)
    "Format a U256 value with specified decimals into a decimal string.
     Example: (format-units 1500000000000000000 18) -> \"1.5\""
    (lisp String (value decimals)
      (cl:let* ((n (web3/types:u256-to-integer
                    (coalton (lisp types:U256 () value))))
                (divisor (cl:expt 10 decimals))
                (whole (cl:floor n divisor))
                (frac (cl:mod n divisor)))
        (cl:if (cl:zerop frac)
               (cl:format cl:nil "~D.0" whole)
               (cl:let* ((frac-str (cl:format cl:nil "~V,'0D" decimals frac))
                         ;; Trim trailing zeros
                         (trimmed (cl:string-right-trim "0" frac-str)))
                 (cl:format cl:nil "~D.~A" whole trimmed))))))

  ;;; =========================================================================
  ;;; Ether Convenience Functions (18 decimals)
  ;;; =========================================================================

  (declare parse-ether (String -> (types:Web3Result types:U256)))
  (define (parse-ether value-str)
    "Parse an ether string into wei (U256).
     Example: (parse-ether \"1.5\") -> 1500000000000000000"
    (parse-units value-str 18))

  (declare format-ether (types:U256 -> String))
  (define (format-ether wei)
    "Format wei (U256) as an ether string.
     Example: (format-ether 1500000000000000000) -> \"1.5\""
    (format-units wei 18))

  ;;; =========================================================================
  ;;; Gwei Convenience Functions (9 decimals)
  ;;; =========================================================================

  (declare parse-gwei (String -> (types:Web3Result types:U256)))
  (define (parse-gwei value-str)
    "Parse a gwei string into wei (U256).
     Example: (parse-gwei \"1.5\") -> 1500000000"
    (parse-units value-str 9))

  (declare format-gwei (types:U256 -> String))
  (define (format-gwei wei)
    "Format wei (U256) as a gwei string.
     Example: (format-gwei 1500000000) -> \"1.5\""
    (format-units wei 9)))


