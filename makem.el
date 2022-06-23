;;; makem.el --- Tools for using makem.sh in Emacs   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/makem.sh
;; Requires: ((emacs "27.1") (transient "0.3.7"))
;; Version: 0.6-pre

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

;; This library contains tools to make using makem.sh in Emacs more
;; convenient.  Use the `makem' command to show a Transient menu to
;; run "makem.sh" with selected options.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'transient)

;;;; Transient

;; FIXME: Some of the infixes seem to incorrectly share whole-transient history.

;;;###autoload
(transient-define-prefix makem ()
  "Transient for running makem.sh."
  [:pad-keys t
             ["Verbosity"
              ("-d" "Debug" ("-d" "--debug"))
              ("-D" "Debug load path" "--debug-load-path")
              ("-n" "No color" "--no-color")
              (makem:-v)]
             ["Emacs Options"
              (makem:--)
              ("-E" "Emacs path" ("-E" "--emacs="))]]
  [:pad-keys t
             ["Files"
              (makem:-e)
              (makem:-f)
              ]
             ["Compilation"
              ("-c" #("Batch-compile files" 0 19 (help-echo "Instead of compiling separately; quicker, but may hide problems"))
               ("-c" "--compile-batch"))
              ("-C" "Don't compile files automatically" ("-C" "--no-compile"))]]
  [:pad-keys t
             ["Sandbox"
              ;; FIXME: -s doesn't require an argument, but it can take one.
              (makem:-s)
              ("-id" "Automatically install package dependencies" "--install-deps")
              ("-il" "Automatically install linters" "--install-linters")
              ("-ii" "Install package before running rules" ("-i" "--install="))]]
  [:pad-keys t
             ["Rules"
              ("a" "All lints and tests" (lambda () (interactive) (makem-run "all")))
              ("c" "Byte-compile source files" (lambda () (interactive) (makem-run "compile")))
              ("b" "Batch mode (arguments after \"--\" passed to Emacs)" (lambda () (interactive) (makem-run "batch")))
              ("i" "Interactive (arguments after \"--\" passed to Emacs)" (lambda () (interactive) (makem-run "interactive")))]]
  [:pad-keys t
             ["Linting"
              ("ll" "All linters (ignoring unavailable ones)" (lambda () (interactive) (makem-run "lint")))
              ("lc" "Compile source files (with warnings as errors)" (lambda () (interactive) (makem-run "lint-compile")))
              ("ld" "Docstrings (Checkdoc)" (lambda () (interactive) (makem-run "lint-checkdoc")))
              ("lD" "Declarations" (lambda () (interactive) (makem-run "lint-declare")))
              ("lE" "Elsa (not included in \"lint\" rule)" (lambda () (interactive) (makem-run "lint-elsa")))
              ("li" "Indentation" (lambda () (interactive) (makem-run "lint-indent")))
              ("lp" "Package" (lambda () (interactive) (makem-run "lint-package")))
              ("lr" "Regexps" (lambda () (interactive) (makem-run "lint-regexps")))]
             ["Testing"
              ("tt" "All tests (ignoring missing test types)" (lambda () (interactive) (makem-run "test")))
              ("tb" "Buttercup" (lambda () (interactive) (makem-run "test-buttercup")))
              ("te" "ERT" (lambda () (interactive) (makem-run "test-ert")))
              ("tE" "ERT (interactively)" (lambda () (interactive) (makem-run "test-ert-interactive")))]])

(transient-define-argument makem:-s ()
  :description "Sandbox directory"
  :class 'transient-option
  :key "-s"
  :argument "--sandbox"
  :prompt "Sandbox directory (blank for temporary one): "
  :reader (lambda (prompt initial-input _history)
            (pcase (read-directory-name prompt nil nil 'confirm (or initial-input ".sandbox"))
              (""
               ;; Use temporary one.
               " ")
              (else (concat "=" (file-relative-name else))))))

(transient-define-argument makem:-- ()
  :description "Emacs arguments"
  :class 'transient-option
  :key "--"
  :argument "--"
  :prompt "Emacs arguments: "
  :reader (lambda (prompt initial-input history)
            (concat " " (read-string prompt initial-input history))))

(transient-define-argument makem:-v ()
  :description "Verbosity"
  :class 'transient-option
  :key "-v"
  :argument "-"
  :prompt "Verbosity level (up to 3): "
  :reader (lambda (prompt initial-input history)
            (let ((level (read-number prompt initial-input history)))
              (if (< 0 level 4)
                  (make-string level ?v)
                (message "Verbosity level must be from 1-3")
                ""))))

(transient-define-argument makem:-e ()
  :description "Exclude files"
  :class 'transient-option
  :key "-e"
  :argument "--exclude="
  :prompt "Exclude files: "
  :reader (lambda (prompt initial-input history)
            (let ((files (mapcar #'file-relative-name
                                 (completing-read-multiple prompt (project-files (project-current))
                                                           nil nil initial-input history))))
              (string-join files " --exclude="))))

(transient-define-argument makem:-f ()
  :description "Include extra files (besides ones in VC)"
  :class 'transient-option
  :key "-f"
  :argument "--file="
  :prompt "Include files: "
  :reader (lambda (prompt initial-input history)
            (let ((files (mapcar #'file-relative-name
                                 (completing-read-multiple prompt (project-files (project-current))
                                                           nil nil initial-input history))))
              (string-join files " --file="))))

;;;; Functions

(defun makem-run (rule)
  "Run \"makem.sh\" with RULE and Transient arguments."
  (unless (makem-ensure-script)
    (user-error "File \"makem.sh\" not present in %s" default-directory))
  (let ((command (concat "./makem.sh "
                         (mapconcat #'shell-quote-argument (transient-args 'makem) " ")
                         " " rule)))
    (compile command)))

(defun makem-ensure-script ()
  "Return non-nil if \"makem.sh\" exists, or offer to download it."
  (or (file-exists-p "makem.sh")
      (when (yes-or-no-p "File \"makem.sh\" not present in current directory.  Download it? ")
        (when-let
            (buffer (url-retrieve-synchronously "https://raw.githubusercontent.com/alphapapa/makem.sh/master/makem.sh"))
          (with-current-buffer buffer
            ;; Why doesn't Emacs have a function to download a file yet?
            (unless (re-search-forward "\r?\n\r?\n")
              (error "End of HTTP headers not found"))
            (narrow-to-region (point) (point-max)))
          (let ((coding-system-for-write 'no-conversion))
            (with-temp-file "makem.sh"
              (insert-buffer-substring buffer)))
          (chmod "makem.sh" 493))
        t)))

(provide 'makem)

;;; makem.el ends here
