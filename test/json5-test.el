;;; json5-test.el --- json5.el's ert test suite -*- lexical-binding: t -*-

;; Copyright (C) 2020  Nikita Bloshchanevich

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; json5.el's ert-based test suite. Run with 'cask exec ert-runner'.

;;; Code:

(require 'json5)
(require 'ert)

(when (boundp 'ert-runner-test-path)
  (cd ert-runner-test-path))

(defun json5-test--parse-json5 (file)
  "Parse FILE using a real JSON5 parser.
If there is a parse error, return `:error'.

Relies on the \"json5\" npm package being installed and in the
PATH."
  (require 'json)
  (declare-function json-read "json" ())
  (with-temp-buffer
    (insert-file-contents file)
    (if (= 0 (call-process-region (point-min) (point-max) "json5" t t))
        (progn (goto-char (point-min)) (json-read))
      :error)))

(defun json5-test--parse-noerror (file)
  "Parse FILE using json5.el ignoring errors.
If an error is thrown, return `:error'."
  (condition-case nil (json5-parse-file file)
    (error :error)))

(defun json5-test--register-tests (prefix test-dir)
  "Add parsing-tests for JSON(5) files in TEST-DIR.
Each test is an `ert' asserting that parsing them with
`json5-test--parse-noerror' and `json5-test--parse-json5' yields
equal results. Start every test with PREFIX."
  (dolist (file (directory-files-recursively test-dir "\\.json5?\\'"))
    (let ((name (intern (concat prefix file))))
      (ert-set-test
       name
       (make-ert-test
        :name name
        :documentation (format "Ensure that %S parses correctly." file)
        :body (lambda () (should
                     (equal (json5-test--parse-json5 file)
                            (json5-test--parse-noerror file)))))))))

(json5-test--register-tests "json5/" "rsc/json5-tests/")

;;; json5-test.el ends here
