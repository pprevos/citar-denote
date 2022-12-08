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

(defun citar-denote-create-note (key &optional entry)
  "Create a bibliography note for `KEY' with properties `ENTRY'."
  (let ((denote-file-type denote-file-type)) 
    (denote
     (read-string "Title: " (citar-get-value "title" key))
     (citar-denote-keywords-prompt))
    (with-current-buffer (current-buffer)
      (goto-char (point-min))
      (while (not(eq (char-after) 10))
	(next-line))
      (insert (format "#+reference:  %s" key))
      (newline)
      (newline))))

(defun citar-denote-get-notes (&optional keys)
  "Return Denote files associated with the `KEYS' list.
Return a hash table mapping elements of `KEY'` to associated notes.
If `KEYS' is omitted, return notes for all Denote files tagged with
`ditar-denote-keyword'."
  (let ((files (make-hash-table :test 'equal)))
    (prog1 files
      (dolist (file (denote-directory-files-matching-regexp
		     (concat "_" citar-denote-keyword)))
	(with-current-buffer (find-file-noselect file)
	  (save-excursion
	    (beginning-of-buffer)
	    (when (search-forward "#+reference:" nil t)
	      (forward-to-word 1)
	      (setq marker1 (point))
	      (end-of-line)
	      (setq marker2 (point))
	      (if keys
		  (dolist (key keys)
		    (when (string= key
				   (buffer-substring-no-properties
				    marker1 marker2))
		      (push file (gethash key files))))
		(let ((key (buffer-substring-no-properties marker1 marker2)))
		  (push file (gethash key files))))))))
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
