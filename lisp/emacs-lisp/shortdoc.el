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
(require 'seq)
(eval-when-compile (require 'cl-lib))

(defgroup shortdoc nil
  "Short documentation."
  :group 'lisp)

(defface shortdoc-section
  '((((class color) (background dark))
     (:inherit variable-pitch
               :background "#303030" :extend t))
    (((class color) (background light))
     (:inherit variable-pitch
               :background "#d0d0d0" :extend t)))
  "Face used for a section.")

(defface shortdoc-example
  '((((class color) (background dark))
     (:background "#202020" :extend t))
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
  `(progn
     (setq shortdoc--groups (delq (assq ',group shortdoc--groups)
                                  shortdoc--groups))
     (push (cons ',group ',functions) shortdoc--groups)))

(define-short-documentation-group string
  "Making Strings"
  (make-string
   :args (length init)
   :example "(make-string 5 ?x)"
   :result "xxxxx")
  (string
   :example "(string ?a ?b ?c)"
   :result "abc")
  (concat
   :example (concat "foo" "bar" "zot"))
  (string-join
   :no-manual t
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
  (replace-regexp-in-string
   :example (replace-regexp-in-string "[a-z]+" "_" "*foo*"))
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
   :no-manual t
   :example (string-truncate-left "longstring" 8))
  (string-remove-suffix
   :no-manual t
   :example (string-remove-suffix "bar" "foobar"))
  (string-remove-prefix
   :no-manual t
   :example (string-remove-prefix "foo" "foobar"))
  (reverse
   :example (reverse "foo"))
  (substring-no-properties
   :example (substring-no-properties (propertize "foobar" 'face 'bold) 0 3))
  "Predicates for Strings"
  (string-equal
   :example (string-equal "foo" "foo"))
  (stringp
   :example "(stringp ?a)"
   :result t)
  (string-empty-p
   :no-manual t
   :example (string-empty-p ""))
  (string-blank-p
   :no-manual t
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

(define-short-documentation-group list
  "Making Lists"
  (make-list
   :example (make-list 5 'a))
  (cons
   :example (cons 1 '(2 3 4)))
  (list
   :example (list 1 2 3))
  "Operations on Lists"
  (append
   :example (append '("foo" "bar") '("zot")))
  (flatten-tree
   :example (flatten-tree '(1 (2 3) 4)))
  (car
   :example (car '(one two three)))
  (cdr
   :example (cdr '(one two three)))
  (push
   :example-no-result (push 'a list))
  "Mapping Over Lists"
  (mapcar
   :example (mapcar #'1+ '(1 2 3)))
  (reduce
   :example (reduce #'+ '(1 2 3)))
  (mapconcat
   :example (mapconcat #'identity '("foo" "bar") "|"))
  "Predicates"
  (listp
   :example (listp '(1 2 3))
   :example (listp nil)
   :example (listp '(1 . 2)))
  (consp
   :example (consp '(1 2 3))
   :example (consp nil))
  (proper-list-p
   :example (proper-list-p '(1 2 3))
   :example (proper-list-p nil)
   :example (proper-list-p '(1 . 2)))
  (null
   :example (null nil)))

(define-short-documentation-group vector
  (make-vector
   :example (make-vector 5 "foo"))
  (vectorp
   :example (vectorp [1])
   :example (vectorp "1"))
  (length
   :example (length [1 2 3]))
  (seq-subseq
   :example (seq-subseq [1 2 3 4 5] 1 3)
   :example (seq-subseq [1 2 3 4 5] 1)))

(define-short-documentation-group regexp
  "Matching Strings"
  (replace-regexp-in-string
   :example (replace-regexp-in-string "[a-z]+" "_" "*foo*"))
  (string-match-p
   :example (string-match-p "^[fo]+" "foobar"))
  (match-string
   :example (and (string-match "^[fo]+" "foobar") (match-string 0 "foobar")))
  (match-beginning
   :example-no-result (match-beginning 1))
  (match-end
   :example-no-result (match-end 2))
  "Looking in Buffers"
  (re-search-forward
   :example-no-result (re-search-forward "^foo$" nil t))
  (re-search-backward
   :example-no-result (re-search-backward "^foo$" nil t))
  (looking-at-p
   :example-no-result (looking-at "f[0-9"))
  "Utilities"
  (regexp-quote
   :example (regexp-quote "foo.*bar"))
  (regexp-opt
   :example (regexp-opt '("foo" "bar"))))

(define-short-documentation-group sequence
  "Sequence Predicates"
  (seq-contains-p
   :example (seq-contains '(a b c) 'b)
   :example (seq-contains '(a b c) 'd))
  (seq-every-p
   :example (seq-every-p #'numberp '(1 2 3)))
  (seq-empty-p
   :example (seq-empty-p []))
  (seq-set-equal-p
   :example (seq-set-equal-p '(1 2 3) '(3 1 2)))
  (seq-some
   :example (seq-some #'cl-evenp '(1 2 3)))
  "Building Sequences"
  (seq-concatenate
   :example (seq-concatenate 'vector '(1 2) '(c d)))
  (seq-copy
   :example (seq-copy '(a 2)))
  (seq-into
   :example (seq-into '(1 2 3) 'vector))
  "Utility Functions"
  (seq-count
   :example (seq-count #'numberp '(1 b c 4)))
  (seq-elt
   :example (seq-elt '(a b c) 1))
  (seq-random-elt
   :example-no-result (seq-random-elt '(a b c)))
  (seq-find
   :example (seq-find #'numberp '(a b 3 4 f 6)))
  (seq-position
   :example (seq-position '(a b c) 'c))
  (seq-length
   :example (seq-length "abcde"))
  (seq-max
   :example (seq-max [1 2 3]))
  (seq-min
   :example (seq-min [1 2 3]))
  (seq-first
   :example (seq-first [a b c]))
  (seq-rest
   :example (seq-rest '[1 2 3]))
  (seq-reverse
   :example (seq-reverse '(1 2 3)))
  (seq-sort
   :example (seq-sort #'> '(1 2 3)))
  (seq-sort-by
   :example (seq-sort-by (lambda (a) (/ 1.0 a)) #'< '(1 2 3)))
  "Mapping Over Sequences"
  (seq-map
   :example (seq-map #'1+ '(1 2 3)))
  (seq-map-indexed
   :example (seq-map-indexed (lambda (a i) (cons i a)) '(a b c)))
  (seq-mapcat
   :example (seq-mapcat #'upcase '("a" "b" "c") 'string))
  (seq-do
   :example-no-result (seq-do (lambda (a) (insert a)) '("foo" "bar")))
  (seq-do-indexed
   :example-no-result (seq-do-indexed
                       (lambda (a index) (message "%s:%s" index a))
                       '("foo" "bar")))
  (seq-reduce
   :example (seq-reduce #'* [1 2 3] 2))
  "Excerpting Sequences"
  (seq-drop
   :example (seq-drop '(a b c) 2))
  (seq-drop-while
   :example (seq-drop-while #'numberp '(1 2 c d 5)))
  (seq-filter
   :example (seq-filter #'numberp '(a b 3 4 f 6)))
  (seq-remove
   :example (seq-remove #'numberp '(1 2 c d 5)))
  (seq-group-by
   :example (seq-group-by #'cl-plusp '(-1 2 3 -4 -5 6)))
  (seq-difference
   :example (seq-difference '(1 2 3) '(2 3 4)))
  (seq-intersection
   :example (seq-intersection '(1 2 3) '(2 3 4)))
  (seq-partition
   :example (seq-partition '(a b c d e f g h) 3))
  (seq-subseq
   :example (seq-subseq '(a b c d e) 2 4))
  (seq-take
   :example (seq-take '(a b c d e) 3))
  (seq-take-while
   :example (seq-take-while #'cl-evenp [2 4 9 6 5]))
  (seq-uniq
   :example (seq-uniq '(a b d b a c))))

(define-short-documentation-group buffer
  "Buffer Basics"
  (current-buffer
   :example-no-result (current-buffer))
  (bufferp
   :example (bufferp 23))
  (buffer-live-p
   :example-no-result (buffer-live-p))
  (buffer-modified-p
   :example (buffer-modified-p (current-buffer)))
  (buffer-name
   :example (buffer-name))
  (window-buffer
   :example (window-buffer))
  "Selecting Buffers"
  (get-buffer-create
   :example-no-result (get-buffer-create "*foo*"))
  (pop-to-buffer
   :example-no-result (pop-to-buffer "*foo*"))
  (with-current-buffer
      :example-no-result (with-current-buffer buffer (buffer-size)))
  "Points and Positions"
  (point
   :example (point))
  (point-min
   :example (point-max))
  (point-max
   :example (point-max))
  (line-beginning-position
   :example (line-beginning-position))
  (line-end-position
   :example (line-end-position))
  (buffer-size
   :example (buffer-size))
  "Moving Around"
  (goto-char
   :example-no-result (goto-char (point-max)))
  (search-forward
   :example-no-result (search-forward "some-string" nil t))
  (re-search-forward
   :example-no-result (re-search-forward "some-s.*g" nil t))
  (forward-line
   :example-no-result (forward-line 1))
  (backward-line
   :example-no-result (backward-line 4))
  "Strings from Buffers"
  (buffer-string
   :example-no-result (buffer-string))
  (buffer-substring
   :example (buffer-substring (point-min) (1+ (point-min))))
  (buffer-substring-no-properties
   :example (buffer-substring-no-properties (point-min) (+ (point-min) 10)))
  (following-char
   :example-no-result (following-char))
  (char-after
   :example (char-after 45))
  "Altering Buffers"
  (delete-region
   :example-no-result (delete-region (point-min) (point-max)))
  (erase-buffer
   :example-no-result (erase-buffer))
  (insert
   :example-no-result (insert "This string will be inserted in the buffer\n")))

(define-short-documentation-group process
  (make-process
   :example-no-result (make-process :name "foo" :command '("cat" "/tmp/foo")))
  (processp
   :example (processp t))
  (delete-process
   :example-no-result (delete-process process))
  (kill-process
   :example-no-result (kill-process process))
  (set-process-sentinel
   :example-no-result (set-process-sentinel process (lambda (proc string))))
  (process-buffer
   :example-no-result (process-buffer process))
  (get-buffer-process
   :example-no-result (get-buffer-process buffer))
  (process-live-p
   :example-no-result (process-live-p process)))

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
    (if (plist-get data :no-manual)
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
    (insert "\n")
    (add-face-text-property start-section (point) 'shortdoc-section t)
    (let ((start (point))
          (print-escape-newlines t))
      (cl-loop for (type value) on data by #'cddr
               do
               (cl-case type
                (:example
                 (if (stringp value)
                      (insert "  " value "\n")
                    (insert "  ")
                    (prin1 value (current-buffer))
                    (insert "\n")
                    (insert "    => ")
                    (prin1 (eval value) (current-buffer))
                    (insert "\n")))
                (:example-no-result
                 (insert "  ")
                 (prin1 value (current-buffer))
                 (insert "\n    -> "
                         (propertize "[it depends]"
                                     'face 'variable-pitch)
                         "\n"))
                (:result
                 (insert "    => ")
                 (prin1 value (current-buffer))
                 (insert "\n"))))
      (put-text-property start (point) 'face 'shortdoc-example))
    (insert "\n")))

(defun shortdoc-function-groups (function)
  "Return all shortdoc groups FUNCTION appears in."
  (cl-loop for group in shortdoc--groups
           when (assq function (cdr group))
           collect (car group)))

(provide 'shortdoc)

;;; shortdoc.el ends here
