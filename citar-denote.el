;;; citar-denote.el --- Creating and accessing bibliography Denote notes with Citar -*- lexical-binding: t -*-

;; Copyright (C) 2022  Peter Prevos

;; Author: Peter Prevos <peter@prevos.net>
;; Maintainer: Peter Prevos <peter@prevos.net>
;; URL: https://github.com/pprevos/denote
;; Version: 0.1
;; Package-Requires: ((emacs "28.2"))

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
;; citar-denote offers integration of Denote with bibliographies through Citar
;; The main task of citar-denote is to seamlessly integrate Denote and Citar
;;
;; This code would not have existed without the help of others.
;; Thanks to:
;; Protesilaos Stavrou for creating Denote and encouraging me to write this code.
;; Bruce D'Arcus for creating Citar and given me some guidance.
;; Joel Lööw for adding the caching functionality.

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

(defun citar-denote--keywords-prompt ()
  "Prompt for one or more keywords and include `citar-denote-keyword'."
  (let ((choice (append (list citar-denote-keyword)
                        (denote--keywords-crm (denote-keywords)))))
    (setq denote-last-keywords
          (if denote-sort-keywords
              (sort choice #'string-lessp)
            choice))))

(defun citar-denote--create-note (key &optional entry)
  "Create a bibliography note for `KEY' with properties `ENTRY'."
  (let ((denote-file-type nil)) ; make sure it is Org
    (denote
     ;;(citar-get-value "title" key)
     (read-string "Title?: " (citar-get-value "title" key))
     (citar-denote--keywords-prompt))
    (with-current-buffer (current-buffer)
      (goto-char (point-min))
      (while (not(eq (char-after) 10))
	(next-line))
      (insert (format "#+reference:  %s" key))
      (newline)
      (newline))))

(defun citar-denote--get-notes (&optional keys)
  "Return denote files associated with `KEYS'."
  (let ((files (make-hash-table :test 'equal))
	(marker1 nil)
	(marker2 nil)
	(dirs citar-notes-paths))
    (prog1 files
      (dolist (dir dirs)
	(when (file-directory-p dir)
	  (dolist (file
		   (denote-directory-files-matching-regexp
		    (concat "_" citar-denote-keyword)))
	    ;; There certainly is a more elegant way of doing this
	    (with-current-buffer (get-buffer (find-file-noselect file))
	      (save-excursion
		;; Not sure if this is needed
		(goto-char (point-min))
		;; This should put us at the beginnig of the reference
		;; name. If this search fails, we are not dealing with
		;; a notes file for a reference.
		(when (search-forward "#+reference: " nil t)
		  ;; Here we select the reference name
		  (forward-to-word 1)
		  (setq marker1 (point))
		  (end-of-line)
		  (setq marker2 (point))
		  (if keys
		      (dolist (key keys)
			(when (string= key
				       (buffer-substring-no-properties marker1 marker2))
			  (push file (gethash key files))))
		    (let ((key (buffer-substring-no-properties marker1 marker2)))
		      (push file (gethash key files))))))))))
      ;; Reverse file lists because push adds elements to the front
      (maphash (lambda (key filelist)
                 (puthash key (nreverse filelist) files))
	       files))))

(defun citar-denote--has-notes ()
  "Return predicate testing whether entry has associated denote files.
See documentation for `citar-has-notes'."
  (setq notes (citar-denote--get-notes))
  (unless (hash-table-empty-p notes)
    (lambda (citekey) (and (gethash citekey notes) t))))

;; Modify the way Citar links notes to bibliographies
(citar-register-notes-source
 'citar-denote-source (list :name "Denote Notes"
			    :category 'file
			    :items 'citar-denote--get-notes
			    :hasitems 'citar-denote--has-notes
			    :open 'find-file
                            :create 'citar-denote--create-note))

(setq citar-notes-source 'citar-denote-source)

(provide 'citar-denote)

;;; citar-denote.el ends here
