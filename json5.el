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

(defun json5--unescape (s)
  "Convert JSON5 escapes in S to normal JSON ones.
Currently, this means transforming \"\\xAB.\" to the equivalent
\"\\u\" fragment."
  (replace-regexp-in-string (rx "\\x" (group (= 2 xdigit))) "\\\\u00\\1" s))

(defun json5--keyword-p (kw)
  "Check if KW is a built-in JSON5 keyword."
  (member kw
          '(;; JSON5 supports Infinity and NaN, but JSON itself doesn't, so most
            ;; JSON parsers don't (neither json.el nor json.c can parse it, but
            ;; Python's can). Keep it in, letting the actual JSON parser choke
            ;; on the invalid input if it doesn't support it.
            "Infinity" "NaN"
            ;; keywords shouldn't be turned to strings
            "true" "false" "null")))

(defun json5--parse-number-literal (kw)
  "Parse number literal KW.
KW may begin with 0b, 0x, 0 or just be a decimal integer. If KW
is not a number, yield nil."
  (save-match-data
    ;; Old `match-data' may interfere, so only do something if there actually
    ;; was a match and the match data was temporarily changed because of that.
    (and
     (string-match
      (rx
       bos (group (? (any "+-")))
       (or (: "0b" (group (+ (any "01"))))
           (: "0x" (group (+ xdigit)))
           (: "0" (group (+ (any "0-7"))))
           (group (* digit)))
       eos)
      kw)
     (let ((sign (match-string 1 kw))
           (binary-int (match-string 2 kw))
           (hex-int (match-string 3 kw))
           (octal-int (match-string 4 kw))
           (decimal-int (match-string 5 kw)))
       (cond (binary-int (string-to-number (concat sign binary-int) 2))
             (hex-int (string-to-number (concat sign hex-int) 16))
             (octal-int (string-to-number (concat sign octal-int) 8))
             (decimal-int (string-to-number (concat sign decimal-int))))))))

(defun json5-to-json (json5)
  "Convert string JSON5 to normal JSON.
Removes all comments, converts whitespace, .... Tries to be as
conservative as possible, only changing what is necessary."
  (replace-regexp-in-string
   (rx (or
        ;; handle comments (/* ... */, // ...)
        (group
         (or (: "//" (* nonl) eol)
             (: "/*" (* (or (not (any ?*))
                            (: (+ ?*) (not (any ?/))))) (+ ?*) ?/)))
        ;; handle trailing commas
        (: "," (group (* (any space "\n")) (any "]}")))

        ;; keyword literals
        (group (+ (or (any alnum "$_") (: "\\" (or (: "x" (= 2 xdigit))
                                                   (: "u" (= 4 xdigit)))))))
        ;; string literals: \\x -> \\u
        (: "\"" (group (* (or (not (any "\\\""))
                              (: "\\" (or (: "x" (= 2 xdigit))
                                          (: "u" (= 4 xdigit))))))) "\"")))
   (lambda (match)
     (let* ((comment (match-string 1 match))
            (comma-tail (match-string 2 match))
            (keyword-literal (match-string 3 match))
            (string-literal (match-string 4 match)))
       (or (and comment "")
           comma-tail

           (and keyword-literal
                (when-let ((num (json5--parse-number-literal keyword-literal)))
                  (number-to-string num)))
           (and keyword-literal (json5--keyword-p keyword-literal)
                keyword-literal)

           ;; all matches must capture, so it must be either a string or basic
           ;; keyword literal.
           (format "\"%s\""
                   (json5--unescape (or string-literal keyword-literal))))))
   json5 nil t))

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

(defun json5--read-file-contents (f)
  "Return the contents of file F, as a string.
See `insert-file' for details."
  (with-temp-buffer (insert-file-contents f) (buffer-string)))

(defun json5-parse-file (f)
  "Parse the JSON5-file F.
See `json5-parse-string' for details."
  (json5-parse-string (json5--read-file-contents f)))

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
