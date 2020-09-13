;;; shortdoc.el --- Short function summaries  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Keywords: lisp, help
;; Package: emacs

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'macroexp)
(eval-when-compile (require 'cl-lib))

(defface shortdoc-section
  '((((class color) (background dark))
     (:inherit variable-pitch
               :background "#808080" :extend t))
    (((class color) (background light))
     (:inherit variable-pitch
               :background "#c0c0c0" :extend t)))
  "Face used for a section.")

(defface shortdoc-example
  '((((class color) (background dark))
     (:background "#808080" :extend t))
    (((class color) (background light))
     (:background "#c0c0c0" :extend t)))
  "Face used for examples.")

(defvar shortdoc--groups nil)

(defmacro define-short-documentation-group (group &rest functions)
  "Add GROUP to the list of defined documentation groups.
FUNCTIONS is a list of elements on the form:

  (fun
   :no-manual BOOL
   :args ARGS
   :example EXAMPLE-FORM
   :result RESULT-FORM)

BOOL should be non-nil if the function isn't documented in the
manual.

ARGS is optional, and the functions definition is displayed
instead in not present.

If EXAMPLE-FORM isn't a string, it will be printed with `prin1',
and then evaled to give a result, which is also printed.  If it's
a string, it'll be inserted as is.  In that case, there should be
a form that says what the result should be.

There can be any number of :example/:result elements."
  `(eval-and-compile
     (setq shortdoc--groups (delq (assq ',group shortdoc--groups)
                                  shortdoc--groups))
     (push (cons ',group ',functions) shortdoc--groups)))

(define-short-documentation-group string
  "Making Strings"
  (make-string
   :example "(make-string 5 ?x)"
   :result "xxxxx")
  (string
   :example "(string ?a ?b ?c)"
   :result "abc")
  (concat
   :example (concat "foo" "bar" "zot"))
  (string-join
   :example (string-join '("foo" "bar" "zot") " "))
  (mapconcat
   :example (mapconcat (lambda (a) (concat "[" a "]"))
                       '("foo" "bar" "zot") " "))
  (format
   :example (format "This number is %d" 4))
  "Manipulating Strings"
  (substring
   :example (substring "foobar" 0 3)
   :example (substring "foobar" 3))
  (split-string
   :example (split-string "foo bar")
   :example (split-string "|foo|bar|" "|")
   :example (split-string "|foo|bar|" "|" t))
  (string-trim
   :no-manual t
   :args (string)
   :doc "Trim STRING of leading and trailing white space."
   :example (string-trim " foo "))
  (string-trim-left
   :no-manual t
   :example (string-trim-left "oofoo" "o+"))
  (string-trim-right
   :no-manual t
   :example (string-trim-right "barkss" "s+"))
  (string-truncate-left
   :example (string-truncate-left "longstring" 8))
  (string-remove-suffix
   :example (string-remove-suffix "bar" "foobar"))
  (string-remove-prefix
   :example (string-remove-prefix "foo" "foobar"))
  (reverse
   :example (reverse "foo"))
  (substring-no-properties
   :example (substring (propertize "foobar" 'face 'bold) 0 3))
  "Predicates for String"
  (string-equal
   :example (string-equal "foo" "foo"))
  (stringp
   :example "(stringp ?a)"
   :result t)
  (string-empty-p
   :example (string-empty-p ""))
  (string-blank-p
   :example (string-blank-p " \n"))
  (string-lessp
   :example (string-lessp "foo" "bar"))
  (string-greaterp
   :example (string-greaterp "foo" "bar"))
  (string-version-lessp
   :example (string-lessp "foo32.png" "bar4.png"))
  (string-prefix-p
   :example (string-prefix-p "foo" "foobar"))
  (string-suffix-p
   :example (string-suffix-p "bar" "foobar"))
  "Case Manipulation"
  (upcase
   :example (upcase "foo"))
  (downcase
   :example (downcase "FOObar"))
  (capitalize
   :example (capitalize "foo bar zot"))
  (upcase-initials
   :example (upcase-initials "The CAT in the hAt"))
  "Converting Strings"
  (string-to-number
   :example (string-to-number "42")
   :example (string-to-number "deadbeef" 16))
  (number-to-string
   :example (number-to-string 42))
  "Data About Strings"
  (length
   :example (length "foo"))
  (seq-position
   :example "(seq-position \"foobarzot\" ?z)"
   :result 6))

(defun shortdoc-display-group (group)
  "Pop to a buffer and display short documentation for functions in GROUP."
  (unless (assq group shortdoc--groups)
    (error "No such documentation group %s" group))
  (pop-to-buffer (format "*Shortdoc %s*" group))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (special-mode)
    (button-mode)
    (mapc
     (lambda (data)
       (if (stringp data)
           (insert (propertize
                    (concat data "\n\n")
                    'face '(variable-pitch (:height 1.3 :weight bold))))
         (shortdoc--display-function data)))
     (cdr (assq group shortdoc--groups))))
  (goto-char (point-min)))

(defun shortdoc--display-function (data)
  (let ((function (pop data))
        (start-section (point)))
    ;; Function calling convention.
    (insert "(")
    (if (getf data :no-manual)
        (insert (symbol-name function))
      (insert-text-button
       (symbol-name function)
       'face 'button
       'action (lambda (_)
                 (info-lookup-symbol function 'emacs-lisp-mode))))
    (dolist (param (or (plist-get data :args)
                       (help-function-arglist function t)))
      (insert " " (symbol-name param)))
    (insert ")\n")
    ;; Doc string.
    (insert "  "
            (or (plist-get data :doc)
                (car (split-string (documentation function) "\n"))))
    (insert "\n\n")
    (add-face-text-property start-section (point) 'shortdoc-section t)
    (let ((start (point))
          (print-escape-newlines t))
      (cl-loop for (type value) on data by #'cddr
               when (eq type :example)
               do (if (stringp value)
                      (insert "  " value "\n")
                    (insert "  ")
                    (prin1 value (current-buffer))
                    (insert "\n")
                    (insert "    => ")
                    (prin1 (eval value) (current-buffer))
                    (insert "\n"))
               when (eq type :result)
               do (progn
                    (insert "    => ")
                    (prin1 value (current-buffer))
                    (insert "\n")))
      (put-text-property start (point) 'face 'shortdoc-example))
    (insert "\n")))

(defun shortdoc-function-groups (function)
  "Return all shortdoc groups FUNCTION appears in."
  (cl-loop for group in shortdoc--groups
           when (assq function (cdr group))
           collect (car group)))

(provide 'shortdoc)

;;; shortdoc.el ends here
