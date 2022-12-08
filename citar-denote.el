;;; citar-denote.el --- Creating and accessing bibliography Denote notes with Citar -*- lexical-binding: t -*-

;; Copyright (C) 2022  Peter Prevos

;; Author: Peter Prevos <peter@prevos.net>
;; Maintainer: Peter Prevos <peter@prevos.net>
;; URL: https://github.com/pprevos/denote
;; Version: 0.1
;; Package-Requires: ((emacs "28.2") (citar "1.0") (denote "1.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; citar-denote offers integration of Denote notes with bibliographies
;; using the Citar package.  It provides the following functionality:
;; 1. Link notes to citations with `#+reference: citation-key' in the front matter
;; 2. Create new notes linked to citations
;; 3. Access existing notes linked to citations
;;
;; This code would not have existed without the help of others:
;; - Protesilaos Stavrou for creating Denote and encouraging me to write elisp.
;; - Bruce D'Arcus for creating Citar and help creating this package.
;; - Joel Lööw for adding the caching functionality.
;; - Noboru Ota and u/L-Szos provided some suggestions

;;; Code:

(require 'citar)
(require 'denote)

(defgroup citar-denote ()
  "Creating and accessing bibliography files with Citar and Denote."
  :group 'files)

(defcustom citar-denote-keyword "bib"
  "Denote keyword (file tag) to indicate bibliographical notes."
  :group 'citar-denote
  :type '(repeat string))

;; New variables to enable non-Org file types.  These are candidates for
;; defcustom.
(defvar citar-denote-file-type denote-file-type
  "File Type used by Citar-Denote.
Default is `denote-file-type'.  Users can use another file type
for their bibliographic notes.")

(defvar citar-denote-reference-format "#+reference:  %s\n"
  "Property added to bibliographic notes' front matter.
The default assumes Org file type.  Users can define their own;
e.g. for the markdown file type the following might be preferred.

   \"reference: \%s\\n\"")
(defvar citar-denote-reference-regexp "^#\\+reference\\s-*:"
  "Regexp used to look for the citekey in a bibliographic notes.
The default assumes Org file type.  This must correspond to
`citar-denote-reference-format'.")

(defvar citar-denote-files-regexp (concat "_" citar-denote-keyword)
  "Regexp used to look for file names of bibliographic notes.
The default assumes \"_bib\" tag is part of the file name.")

(defconst citar-denote-orig-source
  citar-notes-source
  "Store the `citar-notes-source' value prior to enabling citar-denote.")

(defconst citar-denote-config
  (list :name "Denote"
        :category 'file
        :items #'citar-denote-get-notes
        :hasitems #'citar-denote-has-notes
        :open #'find-file
        :create #'citar-denote-create-note)
  "Instructing citar to use citar-denote functions.")

(defvar citar-notes-source)
(defvar citar-notes-sources)

(defun citar-denote-keywords-prompt ()
  "Prompt for one or more keywords and include `citar-denote-keyword'."
  (let ((choice (append (list citar-denote-keyword)
                        (denote--keywords-crm (denote-keywords)))))
    (setq denote-last-keywords
          (if denote-sort-keywords
              (sort choice #'string-lessp)
            choice))))

(defun citar-denote-add-reference (key file-type)
  "Add reference property with KEY in front matter of FILE-TYPE.
Currently it is added after keywords property, thus it needs to
present in the front matter."
  (goto-char (point-min))
  (when (re-search-forward (denote--keywords-key-regexp file-type) nil t 1)
    (goto-char (line-beginning-position 2))
    (insert (format citar-denote-reference-format key))))

(defun citar-denote-create-note (key &optional _entry)
  "Create a bibliography note for `KEY' with properties `ENTRY'.
The file type for the note to be created is determined by user
option `denote-file-type'."
  (let ((denote-file-type citar-denote-file-type))
    (denote
     ;;(citar-get-value "title" key)
     (read-string "Title: " (citar-get-value "title" key))
     (citar-denote-keywords-prompt))
    ;; TODO: Check if with-current-buffer (current-buffer) is needed.
    ;; Left as is because the original version had it -- it's probably
    ;; redandunt.
    (with-current-buffer (current-buffer)
      (citar-denote-add-reference key denote-file-type))))

(defun citar-denote-retrieve-reference-key-value (file file-type)
  "Return cite key value from FILE front matter per FILE-TYPE.
This function assume title and reference values can be retrieved
by the same function per file-type; hence, it callls
`denote--title-value-reverse-function' instead of requiring cite
key specific function."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward citar-denote-reference-regexp nil t 1)
      ;; Reuse denote--title-value-reverse-function
      (funcall (denote--title-value-reverse-function file-type)
               (buffer-substring-no-properties (point) (line-end-position))))))

(defun citar-denote-get-notes (&optional keys)
  "Return Denote files associated with the `KEYS' list.
Return a hash table mapping elements of `KEY'` to associated notes.
If `KEYS' is omitted, return notes for all Denote files tagged with
`ditar-denote-keyword'."
  (let ((files (make-hash-table :test 'equal)))
    (prog1 files
      (dolist (file (denote-directory-files-matching-regexp
                     citar-denote-files-regexp))
        (let ((key-in-file (citar-denote-retrieve-reference-key-value
                            file denote-file-type)))
          (if keys (dolist (key keys)
                     (when (string= key key-in-file)
                       (push file (gethash key-in-file files))))
            ;; If optional arg keys are not provided
            (push file (gethash key-in-file files)))))
      (maphash (lambda (key filelist)
                 (puthash key (nreverse filelist) files))
               files))))

(defun citar-denote-has-notes ()
  "Return predicate testing whether entry has associated denote files.
See documentation for `citar-has-notes'."
  (setq notes (citar-denote-get-notes))
  (unless (hash-table-empty-p notes)
    (lambda (citekey) (and (gethash citekey notes) t))))

(defun citar-denote-setup ()
  "Setup `citar-denote-mode'."
  (citar-register-notes-source
   'citar-denote-source citar-denote-config)
  (setq citar-notes-source 'citar-denote-source))

(defun citar-denote-reset ()
  "Reset citar to default values."
  (setq citar-notes-source citar-denote-orig-source)
  (citar-remove-notes-source 'citar-denote))

;;;###autoload
(define-minor-mode citar-denote-mode
  "Toggle `citar-denote-mode'."
  :global t
  :group 'citar
  :lighter " citar-denote"
  (if citar-denote-mode
      (citar-denote-setup)
    (citar-denote-reset)))

(provide 'citar-denote)
;;; citar-denote.el ends here
