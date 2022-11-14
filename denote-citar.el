;;; denote-citar.el --- Creating and accessing bibliography notes with Denote and Citar -*- lexical-binding: t -*-

;; Copyright (C) 2022  Peter Prevos

;; Author: Peter Prevos <peter@prevos.net>
;; Maintainer: Peter Prevos <peter@prevos.net>
;; URL: https://github.com/pprevos/denote
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.2"))

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
;; denote-citar offers integration of Denote with BibTeX through Citar
;; The main task of denote-citar is to seamlessly integrate Denote and Citar
;;
;; This code would not have existed without the help of Protesilaos Stavrou.

(require 'citar)
(require 'denote)

(defgroup denote-citar ()
  "Creating and accessing ."
  :group 'files)

;; Denote keyword to indicate bibliographic notes
(defcustom denote-citar-keyword '("bib")
  "List of strings with predefined denote keywords to indicate bibliographical notes"
  :group 'denote-citar
  :type '(repeat string))

(defconst denote-citar-retrieve--ref-front-matter-key-regexp
  "^\\(?:#\\+\\)?\\(?:reference\\)\\s-*[:=]"
  "Regular expression for reference key.")

;; Helper function for selecting keywords
(defun denote-citar--keywords-prompt ()
  "Prompt for one or more keywords and include `denote-citar-keyword'."
  (let ((choice (append denote-citar-keyword
                        (denote--keywords-crm (denote-keywords)))))
    (setq denote-last-keywords
          (if denote-sort-keywords
              (sort choice #'string-lessp)
            choice))))

;; Helper function to create new note
(defun denote-citar-file--create-note (key &optional entry)
  "Create a bibliography note through Citar."
  (let ((denote-file-type nil)) ; make sure it is Org
    (denote
     ;; Replace underscores in citation key
     (replace-regexp-in-string "_" "-" key)
     (denote-citar--keywords-prompt))
    (with-current-buffer (current-buffer) ;; This is the buffer
					  ;; created by denote
      (save-excursion
	(goto-char (point-min))
	;; Find the end of the front matter and insert the key there.
	;; This can probably be solved in a better way.
	(re-search-forward "^[^#]")
	(search-backward "#")
	(goto-char (point-at-eol))
        (newline)
        (insert (format "#+reference:  %s" key))))))

;; Prot: Check 'denote-retrieve--value-title'.
;; You would basically just need to create a copy of it and
;; 'denote-retrieve--title-front-matter-key-regexp' with "reference"
;; instead of "title".

(defun denote-citar-retrieve--value-ref (file &optional key)
  "Return title value from FILE.
If optional KEY is non-nil, return the key instead."
  (denote-retrieve--search
   file
   denote-citar-retrieve--ref-front-matter-key-regexp key))

;; Modify the way Citar links notes to bibliographies
(setq citar-notes-sources
      `((citar-file .
                    ,(list :name "Notes"
                           :category 'file
                           :items #'citar-file--get-notes
                           :hasitems #'citar-file--has-notes
                           :open #'find-file
                           :create #'denote-citar-file--create-note
                           :transform #'file-name-nondirectory))))

(provide 'denote-citar)
