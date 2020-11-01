;;; json5.el --- JSON5 parsing library -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Nikita Bloshchanevich

;; Author: Nikita Bloshchanevich <nikblos@outlook.com>
;; URL: https://github.com/nbfalcon/json5.el
;; Package-Requires: ((emacs "25.1"))
;; Version: 0.1.0

;;; Commentary:
;; Emacs Lisp library to handle JSON5. Handles parsing and conversion to normal
;; JSON.
;;
;; json.el and Emacs 27+' JSON support don't handle JSON5, which has many more
;; features (comments, etc...) and is often used in VSCode's configuration
;; files.
;;
;; To reduce complexity, this library does not implement a complete parser for
;; JSON5; rather, to "parse" JSON5, it is first converted to normal JSON using
;; regex replace and then parsed using a proper json parsing library.
;;
;; Since JSON5 is a strict superset of JSON (except for NaN and Infinity
;; support), the usual encoding functions (e.g. `json-encode' from json.el) may
;; be used to generate it.

;;; Code:

;; (defmacro json5--static-rx (&rest REGEXPS)
;;   "`rx' wrapped in `eval-when-compile'.
;; REGEXPS are passed to `rx' unmodified.

;; Useful when (regexp) blocks are used with regexes that can be
;; known at compile time."
;;   `(eval-when-compile
;;      (rx ,@REGEXPS)))

;;; json5 syntax
(defconst json5--line-terminator
  (rx (or (any "\n\r\u2028\u33B0") "\r\n"))
  "Matches a JSON5 line terminator.
Corresponds to ECMAScript's LineTerminator Sequence.")

(defconst json5--escape
  (rx (: "\\" (or (: "x" (= 2 xdigit))
                  (: "u" (= 4 xdigit))
                  (any "0-9\\nlfrt'\"")
                  (regexp json5--line-terminator))))
  "Matches a JSON5 backslash escape.")

(defconst json5--whitespace
  (rx (or (any blank "\f") (regexp json5--line-terminator)
          (: "//" (* nonl) eol)
          (: "/*" (* (or (not (any ?*))
                         (: (+ ?*) (not (any ?/))))) (+ ?*) ?/)))
  "Matches JSON5 whitespace.
Includes comments.")

;;; helper functions
(defun json5--reescape (s)
  "Convert JSON5 escapes in S to normal JSON ones.
Currently, this means transforming \"\\xAB.\" to the equivalent
\"\\u\" fragment and removing escaped newlines."
  (replace-regexp-in-string
   (rx (or (: "\\" (or (: "x" (group (= 2 xdigit)))
                       (group "'")
                       ;; Make the rule below not match on only the quote.
                       (group "\"")
                       (regexp json5--line-terminator)))
           (group-n 3 "\"")))
   (lambda (match)
     (let ((xliteral (match-string 1 match))
           (quoted-quot (match-string 2 match))
           (dquote (match-string 3 match)))
       (cond (xliteral (format "\\u00%s" xliteral))
             (quoted-quot "'")
             (dquote "\\\"")
             ;; remove matches without groups
             (t ""))))
   s nil t))

(defun json5--keyword-p (kw)
  "Check if KW is a built-in JSON5 keyword."
  (member kw
          '(;; JSON5 supports Infinity and NaN, but JSON itself doesn't, so most
            ;; JSON parsers don't (neither json.el nor json.c can parse it, but
            ;; Python's can). Keep it in, letting the actual JSON parser choke
            ;; on the invalid input if it doesn't support it.
            "Infinity" "NaN"
            "+Infinity" "-Infinity"
            ;; keywords shouldn't be turned to strings
            "true" "false" "null")))

(defun json5--parse-number-lit (kw)
  "Parse number literal KW.
KW may begin with 0b, 0x, 0 or just be a decimal integer. If KW
is not a number, yield nil."
  (save-match-data
    ;; Old `match-data' may interfere, so only do something if there actually
    ;; was a match and the match data was temporarily changed because of that.
    (and
     (string-match
      (rx bos
          (group (? (any "+-")))
          (or (: "0b" (group (+ (any "01"))))
              (: "0x" (group (+ xdigit)))
              (: "0" (group (+ (any "0-7"))))
              (: (group (* digit)) (group (? "." (* digit)))
               (group (? (any "eE") (? (any "+-")) (* digit)))))
          eos)
      kw)
     (let ((sign (match-string 1 kw))
           (binary-int (match-string 2 kw))
           (hex-int (match-string 3 kw))
           (octal-int (match-string 4 kw))
           (decimal-int (match-string 5 kw))
           (period-tail (match-string 6 kw))
           (exponent-tail (match-string 7 kw)))
       (cond (binary-int (string-to-number (concat sign binary-int) 2))
             (hex-int (string-to-number (concat sign hex-int) 16))
             (octal-int (string-to-number (concat sign octal-int) 8))
             (decimal-int
              (string-to-number
               (concat sign decimal-int
                       (if (string= period-tail ".") ".0" period-tail)
                       exponent-tail))))))))

;; (defun json5--parse-number (kw)
;;   "Like `json5--parse-number-lit', but handle \".0\".
;; Pass KW to `json5--parse-number-lit'; if the latter returns a
;; float ending in 0, return it as an int."
;;   (when-let ((num (json5--parse-number-lit kw)))
;;     (let ((truncated-num (truncate num)))
;;       (if (= num truncated-num) truncated-num num))))

;;; core conversion function
(defun json5-to-json (json5)
  "Convert the string JSON5 to normal JSON.
Removes all comments, converts whitespace, .... Note that
Infinity and NaN are not removed, so will probably cause issues."
  (replace-regexp-in-string
   (rx (or
        ;; handle whitespace and comments
        (group (+ (regexp json5--whitespace)))
        ;; handle trailing commas
        (: "," (* (regexp json5--whitespace)) (group (any "]}")))
        ;; handle keyword literals and numbers
        (group (+ (or (any alnum "-$_+.?") (regexp json5--escape))))
        ;; handle string literals
        (: "\"" (group (* (or (not (any "\\\"")) (regexp json5--escape))))
         "\"")
        (: "'" (group-n 4 (* (or (not (any "\\'")) (regexp json5--escape))))
         "'")))
   (lambda (match)
     (let* ((whitespace (match-string 1 match))
            (comma-tail (match-string 2 match))
            (keyword-literal (match-string 3 match))
            (string-literal (match-string 4 match)))
       (or (and whitespace " ")
           comma-tail

           (and keyword-literal
                (when-let ((num (json5--parse-number-lit keyword-literal)))
                  (number-to-string num)))
           (and keyword-literal (json5--keyword-p keyword-literal)
                keyword-literal)

           ;; all matches must capture, so it must be either a string or basic
           ;; keyword literal.
           (format "\"%s\""
                   (json5--reescape (or string-literal keyword-literal))))))
   json5 nil t))

;;; convenience functions
(defun json5-parse-string (s)
  "Parse JSON5 string S.
This function uses `json-read-from-string' internally, so
json.el's globals may be overridden to change types."
  (json-read-from-string (json5-to-json s)))

(defun json5--buffer-contents ()
  "Get the contents of the entire buffer, as a string.
The result has no properties; respect narrowing."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun json5-parse-buffer ()
  "Parse the current buffer, which must contain JSON5.
Respects narrowing. A convenience wrapper around
`json5-parse-string', which see."
  (json5-parse-string (json5--buffer-contents)))

(defun json5-parse-file (f)
  "Parse the JSON5-file F.
See `json5-parse-string' for details."
  (with-temp-buffer (insert-file-contents f) (json5-parse-buffer)))

;; (defun json5-to-json-region (start end)
;;   "Convert JSON5 buffer region (START END) to json."
;;   (interactive "r")
;;   (cl-callf json5-to-json (buffer-substring start end)))

;;; LocalWords: Nikita
;;; LocalWords: Bloshchanevich
;;; LocalWords: JSON
;;; LocalWords: json
;;; LocalWords: JSON5
;;; LocalWords: json5
;;; LocalWords: emacs
;;; LocalWords: VSCode's

(provide 'json5)
;;; json5.el ends here
