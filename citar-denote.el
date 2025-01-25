;;; citar-denote.el --- Minor mode integrating Citar and Denote -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Peter Prevos

;; Author: Peter Prevos <peter@prevos.net>
;; Maintainer: Peter Prevos <peter@prevos.net>
;; Homepage: https://github.com/pprevos/citar-denote
;; Version: 2.3
;; Package-Requires: ((emacs "28.1") (citar "1.4") (denote "3.1") (dash "2.19.1"))

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
;; A minor-mode integrating 'citar' and 'denote' to create and manage
;; bibliographic notes.
;;
;; Citar-Denote integrates the Citar bibliography package with the Denote
;; note-taking system.  Citar lets you browse and act on bibliographic data
;; in BibTeX, BibLaTeX or JSON format.  Denote is a versatile Emacs package
;; for taking and managing notes. Combining these two packages enables
;; linking notes in your Denote collection to your bibliography, providing
;; a complete solution for documenting literature reviews.
;;
;; Read the manual with `C-h R citar-denote`.

;;; Code:

(require 'denote)
(require 'dash)
(require 'citar)
(eval-when-compile
  (require 'subr-x))

;; Customisable variables

(defgroup citar-denote ()
  "Creating and accessing bibliography files with Citar and Denote."
  :group 'files
  :link  '(url-link :tag "Homepage" "https://github.com/pprevos/citar-denote/"))

(defcustom citar-denote-keyword "bib"
  "Denote keyword (file tag) to indicate bibliographical notes.
Minimises the search space for connecting notes to a bibliography."
  :group 'citar-denote
  :type  'string)

(defcustom citar-denote-use-bib-keywords nil
  "Extract keywords from bibliography and use as Denote keywords.
Extracts keywords from comma-separated list in the `keywords' BibTeX field."
  ;; https://github.com/pprevos/citar-denote/issues/17
  :group 'citar-denote
  :type  'boolean)

(defcustom citar-denote-file-type (or denote-file-type 'org)
  "File Type used by Citar-Denote.

Default is `denote-file-type', or org if the former is nil.

When the value is `markdown-yaml', the file type is Markdown mode with front
matter in YAML notation.  Similarly, `markdown-toml' is Markdown with TOML
syntax as front matter.  When the value is `text', the file will be in
text mode."
  :group 'citar-denote
  :type '(choice
          (const :tag "Org mode (default)" org)
          (const :tag "Markdown (YAML front matter)" markdown-yaml)
          (const :tag "Markdown (TOML front matter)" markdown-toml)
          (const :tag "Plain text" text)))

(defcustom citar-denote-subdir nil
  "Save new bibliographic notes in a chosen or defined subdirectory.
Options:
- nil, note is stored in `denote-directory'.
- t, Denote asks for subdirectory to store the note.
- `string': When entering a string, the note is save in a subdirectory
  under `denote-directory'."
  ;; https://github.com/pprevos/citar-denote/issues/11
  :group 'citar-denote
  :type  '(choice (const  :tag "Store in `denote-directory'" nil)
                  (const  :tag "Ask for subdirectory to store note" t)
                  (string :tag "Save in named subdirectory")))

(defcustom citar-denote-signature nil
  "Ask for a signature, or use citation key when saving new bibliographic notes."
  :group 'citar-denote
  :type  '(choice (const :tag "No signature" nil)
                  (const :tag "Ask for signature" ask)
                  (const :tag "Use citation key as signature" citekey)))

(defcustom citar-denote-template nil
  "Use a template when creating a new bibliographic note."
  :group 'citar-denote
  :type  '(choice (const :tag "No template" nil)
                  (const :tag "Ask for template" t)
                  (symbol :tag "Specific template")))

(defcustom citar-denote-title-format "title"
  "Title format for new bibliographic notes.

- `title': Extract title (or short title) from bibliographic entry
- `author-year': Author-year citation style, e.g. Stallman (1981)
- `author-year-title': Combine author, year and title
- `full': Full citation
- nil: BibTeX citekey

For `author-year' and `author-year-title' you can configure:
- `citar-denote-title-format-authors'
- `citar-denote-title-format-andstr'."
  :group 'citar-denote
  :type  '(choice
           (const :tag "Title" "title")
           (const :tag "Author (Year)" "author-year")
           (const :tag "Author (Year). Title" "author-year-title")
           (const :tag "Full citation" "full")
           (const :tag "Citekey" nil)))

(defcustom citar-denote-title-format-authors 1
  "Maximum number of authors befor et al. in `citar-denote--format-author-editor'."
  :group 'citar-denote
  :type  'integer)

(defcustom citar-denote-title-format-andstr "and"
  "Connecting word for last two authors `citar-denote--format-author-editor'."
  :group 'citar-denote
  :type  'string)

(defcustom citar-denote-cite-includes-reference nil
  "Include reference notes in cite search.
When non-nil, searching for files citing a bibtex key includes Denote files
that only contain the citekey in the reference front matter (and not as a
@-style citation)."
  ;; https://github.com/pprevos/citar-denote/issues/34
  :group 'citar-denote
  :type  'boolean)

(defvar citar-denote-file-types
  `((org
     :reference-format "#+reference:  %s\n"
     :reference-regex "^#\\+reference\\s-*:")
    (markdown-yaml
     :reference-format "reference:  %s\n"
     :reference-regex "^reference\\s-*:")
    (markdown-toml
     :reference-format "reference  = %s\n"
     :reference-regex "^reference\\s-*=")
    (text
     :reference-format "reference:  %s\n"
     :reference-regex "^reference\\s-*:"))
  "Alist of `citar-denote-file-types' and their format properties.

Each element is of the form (SYMBOL . PROPERTY-LIST).  SYMBOL is
one of those specified in `citar-denote-file-type'.

PROPERTY-LIST is a plist that consists of two elements:

- `:reference-format' Front matter identifier for citation key.
- `:reference-regex' Regular expression for the citekey in
  front matter.")

(make-obsolete-variable 'citar-denote-citekey-regex "" "2.1.1")
(make-obsolete-variable 'citar-denote-files-regex "" "2.1.2")

;; Auxiliary functions

(defun citar-denote--reference-format (file-type)
  "Return reference format for FILE-TYPE from `citar-denote-file-types'."
  (plist-get
   (alist-get (or file-type 'org) citar-denote-file-types)
   :reference-format))

(defun citar-denote--reference-regex (file-type)
  "Return reference regex for FILE-TYPE from `citar-denote-file-types'."
  (plist-get
   (alist-get (or file-type 'org) citar-denote-file-types)
   :reference-regex))

(defun citar-denote--extract-keywords (citekey)
  "Extract keywords of CITEKEY from BibTeX entry.

Used when `citar-denote-use-bib-keywords' is non-nil."
  (if-let* ((keywords (citar-get-value "keywords" citekey))
            (filetags (split-string keywords ", *")))
      (mapcar (lambda (kwd) (denote-sluggify-keyword kwd))
              filetags)))

(defun citar-denote--keywords-prompt (citekey)
  "Prompt for one or more keywords and include `citar-denote-keyword'.

When `citar-denote-use-bib-keywords' is not nil, use keywords from entry with
citation key CITEKEY."
  (let ((keywords (if citar-denote-use-bib-keywords
		      (citar-denote--extract-keywords citekey)
		    (denote-keywords-sort (denote--keywords-crm
		                           (delete citar-denote-keyword
			                           (denote-keywords)))))))
    (append keywords (list citar-denote-keyword))))

(defun citar-denote--add-reference (citekeys)
  "Add CITEKEYS to the front matter of the current buffer.
`citar-denote-add-reference' is the interactive version of this function."
  (let* ((file (buffer-file-name))
         (filetype (denote-filetype-heuristics file))
         (new-references (mapconcat #'identity citekeys ";"))
         (existing-references (citar-denote--retrieve-references file)))
    (if existing-references
        ;; Add to existing
        (save-excursion
          (goto-char (point-min))
          (re-search-forward (citar-denote--reference-regex filetype))
          (end-of-line)
          (insert (concat ";" new-references))
          (save-buffer))
      ;; New reference line
      (citar-denote--add-new-reference-line citekeys filetype))))

(defun citar-denote--add-new-reference-line (citekeys filetype)
  "Add a new reference line with CITEKEYS to buffer of FILETYPE."
  (save-excursion
    (goto-char (point-min))
    (while (not (looking-at "^$")) ;; Search for the first empty line
      (forward-line 1))
    (when (not (eq filetype 'org)) (forward-line -1))
    (insert (format (citar-denote--reference-format filetype)
                    (mapconcat #'identity citekeys ";")))))

(defun citar-denote--retrieve-references (file)
  "Return reference citekey(s) from FILE front matter."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((trims "[ \t\n\r]+")
          (file-type (denote-filetype-heuristics file)))
      (when (re-search-forward
             (citar-denote--reference-regex file-type) nil t 1)
        (split-string
         (string-trim
          (buffer-substring-no-properties (point) (line-end-position))
          trims trims) ";")))))

(defun citar-denote--get-notes (&optional citekeys)
  "Generate hash table of Denote files associated with CITEKEYS.

If CITEKEYS is omitted, return all Denote files tagged with
`citar-denote-keyword'."
  (let ((files (make-hash-table :test 'equal)))
    (prog1 files
      (dolist (file (denote-directory-files
                     (concat "_" citar-denote-keyword)))
        (let ((keys-in-file (citar-denote--retrieve-references file)))
          (dolist (key keys-in-file)
            (if citekeys
                (dolist (k citekeys)
                  (when (string= k key)
                    (push file (gethash key files))))
              (push file (gethash key files))))))
      (maphash
       (lambda (key filelist)
         (puthash key (nreverse filelist) files)) files))))

(defun citar-denote--retrieve-cite-files (citekey)
  "Return names of Denote files that contain CITEKEY.

If `citar-denote-cite-includes-reference' is non-nil, the results
will include Denote files with CITEKEY only in the reference
front matter."
  (let ((cite-sign (if citar-denote-cite-includes-reference "" "@"))
                    ;; Include '@' in the xref query only when front
                    ;; matter '#+reference:' to bibtex keyword should
                    ;; not be included in the results
        (files (denote-directory-files nil nil t)))
    (delete-dups
     (mapcar
      #'xref-location-group
      (mapcar #'xref-match-item-location
              (xref-matches-in-files
               (format "%s%s" cite-sign citekey) files))))))

(defun citar-denote--has-notes ()
  "Return a list of all citekeys referenced in a Denote file.

See documentation for `citar-has-notes'."
  (let ((notes (citar-denote--get-notes)))
    (unless (hash-table-empty-p notes)
      (lambda (citekey) (and (gethash citekey notes) t)))))

(defun citar-denote--has-citekeys (citekeys)
  "Return hash table of citar entries with associated CITEKEYS."
  (let ((citar-entries (citar-get-entries))
        (citekey-entries (make-hash-table :test 'equal)))
    (mapc (lambda (key)
            (puthash key (gethash key citar-entries) citekey-entries))
          citekeys)
    (unless (hash-table-empty-p citekey-entries)
      (lambda (citekey) (and (gethash citekey citekey-entries) t)))))

(defun citar-denote--remove-bibkey (file)
  "Remove `citar-denote-bibkey' file tag from FILE."
  ;; https://github.com/pprevos/citar-denote/issues/43
  (let* ((file-type (denote-filetype-heuristics file))
         (keywords (denote-retrieve-keywords-value file file-type))
         (new-keywords (delete citar-denote-keyword keywords)))
    (denote-rewrite-keywords file new-keywords file-type)
    (denote-rename-file-using-front-matter file)))

(defun citar-denote--add-bibkey (file)
  "Add `citar-denote-bibkey' file tag from FILE."
  ;; https://github.com/pprevos/citar-denote/issues/44
  (when-let* ((file-type (denote-filetype-heuristics file))
              (keywords (string-split (denote-retrieve-filename-keywords file) "_"))
              (existing-p (not (member citar-denote-keyword keywords)))
              (new-keywords (delete-dups (denote-keywords-sort
                                          (cons citar-denote-keyword keywords)))))
    (denote-rewrite-keywords file new-keywords file-type)
    (denote-rename-file-using-front-matter file)))

(defun citar-denote--extract-citations-blocks (line)
  "Extract all Org mode citation blocks from LINE."
  (let ((org-cite-regex "\\[\\(?:cite\\(?:/[a-z]\\)?\\)\\:@\\([^]]+\\)\\]")
	(citation-blocks '())
        (start 0))
    (while (string-match org-cite-regex line start)
      (push (match-string 0 line) citation-blocks)
      (setq start (match-end 0)))
    citation-blocks))

(defun citar-denote--extract-citation-keys (string)
  "Extracts all citation keys from the given STRING."
  (let ((matches '())
        (pos 0))
    (while (string-match "@\\([a-zA-Z0-9-_:?]+\\)" string pos)
      (setq matches (cons (match-string 1 string) matches))
      (setq pos (match-end 0)))
    (reverse matches)))

(defun citar-denote--extract-citations ()
  "Extract citations from all Denote files."
  ;; Extract lines with citations
  (let* ((xrefs (xref-matches-in-files "\\[cite:@.*\\]"
                 (denote-directory-files nil nil t)))
         (citation-lines (mapcar #'substring-no-properties
                                 (mapcar #'xref-match-item-summary
                                         xrefs)))
         (citation-blocks (mapcar #'citar-denote--extract-citations-blocks
				       citation-lines))
	 (citation-blocks-flat  (apply #'append citation-blocks))
	 (citations (mapcar #'citar-denote--extract-citation-keys
			    citation-blocks-flat)))
	 (delete-dups (apply #'append citations))))

(defun citar-denote--format-author-editor (citekey)
  "Extract author or editor from CITEKEY.
Format author(s) or editor(s) in accordance with:
- `citar-denote-title-format-authors': Number of authors before et al.
- `citar-denote-title-format-andstr': Connecting string between last two authors."
  (let ((author-names (or (citar-get-value "author" citekey)
                          (citar-get-value "editor" citekey))))
    (citar--shorten-names
     author-names
     citar-denote-title-format-authors
     citar-denote-title-format-andstr)))

(defun citar-denote--generate-title (citekey)
  "Generate title for new bibliographic note using CITEKEY.

Either title, author/year, full reference or citation key.
Based on the `citar-denote-title-format' variable."
  ;; https://github.com/pprevos/citar-denote/issues/15
  (let ((author-editor (citar-denote--format-author-editor citekey))
        (title (citar-get-value "title" citekey))
        (year (or (citar-get-value "year" citekey)
                  (citar-get-value "date" citekey)
                  (citar-get-value "issued" citekey))))
    (cond ((equal citar-denote-title-format "title")
           title)
          ((equal citar-denote-title-format "author-year")
           (concat author-editor " (" year ")"))
          ((equal citar-denote-title-format "author-year-title")
           (let* ((citar-denote-title-format "author-year")
                  (author-year (citar-denote--generate-title citekey))
                  (title (citar-get-value "title" citekey)))
             (concat author-year " " title)))
          ((equal citar-denote-title-format "full")
           (let ((ref (citar-format-reference (list citekey))))
             (substring ref 0 (- (length ref) 2))))
          (t citekey))))

(defun citar-denote--get-nocite ()
  "Select item(s) from Citar entries not cited or referenced in Denote files."
  (let* ((bibliography (hash-table-keys (citar-get-entries)))
         (citations (citar-denote--extract-citations))
         (references (hash-table-keys (citar-denote--get-notes)))
	 (cite-ref (cl-union citations references))
	 (nocite (cl-set-difference bibliography cite-ref :test #'string=)))
    (when nocite (citar-select-refs
                  :multiple t
                  :filter (citar-denote--has-citekeys nocite)))))

(defun citar-denote--get-non-referenced (file)
  "Select Citar entry not already referenced in FILE."
  (let* ((bibliography (hash-table-keys (citar-get-entries)))
         (references (citar-denote--retrieve-references file))
         (unused (-difference bibliography references)))
    (when unused (citar-select-refs
                  :multiple t
                  :filter (citar-denote--has-citekeys unused)))))

(defun citar-denote--select-file-using-title (files)
  "Select a file based on a list of note titles from a list of FILES."
  (let* ((description-file-alist
          (mapcar
           (lambda (file)
             (cons (denote--link-get-description file) file))
           files))
         (selected-description (completing-read "Select note: " description-file-alist))
         (selected-file (cdr (assoc selected-description description-file-alist))))
    selected-file))

(defun citar-denote--create-note (citekey &optional _entry)
  "Create a bibliographic note for CITEKEY with properties ENTRY.

- The note file type is determined by `citar-denote-file-type'.
- The title format is set by `citar-denote-title-format'.
- When `citar-denote-subdir' is non-nil, prompt for a subdirectory.
- When `citar-denote-template' is a symbol, use the specified template,
  if otherwise non-nil, prompt for a Denote template.
- When `citar-denote-signature' is non-nil, prompt for a signature or use
  the citation key."
  (denote
   (read-string "Title: " (citar-denote--generate-title citekey))
   (citar-denote--keywords-prompt citekey)
   citar-denote-file-type
   (when citar-denote-subdir
     (if (stringp citar-denote-subdir)
         (expand-file-name
          (concat denote-directory citar-denote-subdir))
       (denote-subdirectory-prompt)))
   nil
   (when citar-denote-template
     (or (alist-get citar-denote-template denote-templates)
         (denote-template-prompt)))
   (cond ((eq citar-denote-signature 'ask)
          (denote-signature-prompt nil "Signature: "))
         ((eq citar-denote-signature 'citekey)
          citekey)
         (nil nil)))
  (citar-denote--add-new-reference-line (list citekey)
                                        citar-denote-file-type)
  ;; Open available atachment in other window
  (when (citar-get-value "file" citekey)
    (when (one-window-p)
      (split-window-right))
    (other-window 1)
    (citar-open-files citekey)))

;; Interactive functions

;;;###autoload
(defun citar-denote-open-note ()
  "Open a bibliographic note using Citar.

Provides a selection list of all bibliographic entries with notes."
  (interactive)
  (let* ((citekeys (citar-select-refs :filter (citar-denote--has-notes)))
         (file (citar--select-resource
                citekeys :notes t :create-notes t)))
    (find-file (cdr file))))

;;;###autoload
(defun citar-denote-find-citation (citekey)
  "Find Denote files that cite a bibliographic entry CITEKEY.

When called interactively, select an entry from a list with all
bibliographic entries cited in Denote files."
  (interactive
    (if-let* ((citations (citar-denote--extract-citations))
              (citekey (citar-select-ref
                        :filter (citar-denote--has-citekeys citations))))
        (list citekey)
      (user-error "No citations found in Denote files")))
  (if-let ((files (citar-denote--retrieve-cite-files citekey)))
      (progn (find-file (denote-get-path-by-id
                         (denote-extract-id-from-string
                          (if (= (length files) 1)
                              (car files)
                            (denote-link--find-file-prompt files)))))
             (goto-char (point-min))
             (search-forward citekey))
    (message "No citations of %s found in Denote files" citekey)))

;;;###autoload
(defun citar-denote-dwim ()
  "Access attachments, notes and links of a bibliographic reference.

When more than one bibliographic item is referenced, select item first."
  (interactive)
  ;; Any citation keys in the note?
  (if-let* ((keys (citar-denote--retrieve-references (buffer-file-name)))
            (key (if (= (length keys) 1)
                     (car keys)
                   (citar-select-ref
                    :filter (citar-denote--has-citekeys keys)))))
      (citar-open (list key))
    (if (denote-file-is-note-p (buffer-file-name))
        (when (yes-or-no-p "Current buffer does not reference a citation key.
Add a reference? ")
          (citar-denote-add-citekey)
          (citar-denote-dwim))
      (message "Buffer is not a Denote file"))))

;;;###autoload
(defun citar-denote-open-reference-entry ()
  "Open BibTeX or JSON file associated with a bibliographic reference.

When more than one bibliographic item is referenced, select item first."
  (interactive)
  (if-let* ((keys (citar-denote--retrieve-references (buffer-file-name)))
            (key (if (= (length keys) 1)
                     (car keys)
                   (citar-select-ref
                    :filter (citar-denote--has-citekeys keys)))))
      (citar-open-entry key)
    (if (denote-file-is-note-p (buffer-file-name))
        (when (yes-or-no-p "Current buffer does not reference a citation key.
Add a reference? ")
          (citar-denote-add-citekey)
          (citar-denote-open-reference-entry))
      (message "Buffer is not a Denote file"))))

;;;###autoload
(defun citar-denote-add-reference (&optional nocite)
  "Add citation key(s) to existing note.
With universal argument choose from entries not yet used in Denote (NOCITE).
Add `citar-denote-bibkey' keyword when no existing reference exists."
  (interactive "P")
  (if-let* ((file (buffer-file-name))
            (denote-p (denote-file-is-note-p file))
            (citekeys (if nocite
                          (citar-denote--get-nocite)
                        (citar-denote--get-non-referenced file))))
      (progn
        (citar-denote--add-reference citekeys)
        (citar-denote--add-bibkey file)
        (save-buffer))
    (if (not denote-p)
        (message "Buffer is not a Denote file")
      (message "All bibliogary entries have been cited or referenced"))))

;;;###autoload
(defun citar-denote-remove-reference ()
  "Remove a reference from a bibliographic note.
If the only or last reference is removed, also remove `citar-denote-keyword'."
  (interactive)
  (if-let* ((file (buffer-file-name))
            (filetype (denote-filetype-heuristics file))
            (citekeys (citar-denote--retrieve-references file))
            (selected (if (< (length citekeys) 2)
                          (car citekeys)
                        (citar-select-ref
                         :filter
                         (citar-denote--has-citekeys citekeys)))))
      (let ((new-citekeys (delete selected citekeys)))
        (save-excursion
          ;; Remove references line
          (goto-char (point-min))
          (re-search-forward
           (citar-denote--reference-regex filetype))
          (move-beginning-of-line nil)
          (kill-line 1)
          ;; Add new line or remove file tags when applicable
          (if (> (length new-citekeys) 0)
              (citar-denote--add-new-reference-line new-citekeys filetype)
            (citar-denote--remove-bibkey file))
          (save-buffer)))
    (message "No references in this buffer, or not a Denote file")))

;;;###autoload
(defun citar-denote-find-reference ()
  "Find Denote file(s) citing one of the current reference(s).

When more than one bibliographic item is referenced, select item first."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (denote-file-is-note-p file)
        (let* ((citekeys (citar-denote--retrieve-references file))
               (citekey (when citekeys
                          (if (= (length citekeys) 1)
                              (car citekeys)
                            (citar-select-ref
                             :filter
                             (citar-denote--has-citekeys citekeys)))))
               (files (delete file (citar-denote--retrieve-cite-files citekey))))
          (cond
           (files
            (find-file (denote-get-path-by-id
                        (denote-extract-id-from-string
                         (denote-link--find-file-prompt files))))
            (goto-char (point-min))
            (search-forward citekey))
           ((null citekey)
            (message "This is not a bibliographic note"))
           (t (message "Reference not cited in Denote files"))))
      (message "Buffer is not a Denote file"))))

;;;###autoload
(defun citar-denote-link-reference ()
  "Insert a Denote link to a bibliographic note using Citar selection."
  ;; https://github.com/pprevos/citar-denote/issues/20
  (interactive)
  (if (denote-file-is-note-p (buffer-file-name))
      (let* ((citekey (citar-select-refs
		       :filter (citar-denote--has-notes)
		       :multiple nil))
	     (files (gethash (car citekey)
			     (citar-denote--get-notes citekey)))
	     (file (if (= (length files) 1)
		       (car files)
		     (citar-denote--select-file-using-title files)))
             (file-type (denote-filetype-heuristics file))
             (description (read-string
                           "Description: "
                           (denote--link-get-description file))))
        (denote-link file file-type description))
    (message "Buffer is not a Denote file")))

;;;###autoload
(defun citar-denote-nocite ()
  "Open bibliographic entries not cited or referenced in Denote files with Citar."
  (interactive)
  (if-let ((citekeys (citar-denote--get-nocite)))
      (citar-open citekeys)
    (message "All bibliogary entries have been cited or referenced")))
  
;;;###autoload
(defun citar-denote-cite-nocite ()
  "Cite bibliographic entries not cited or referenced in Denote files."
  (interactive)
  (if (denote-file-is-note-p (buffer-file-name))
      (if-let (citations (citar-denote--get-nocite))
	  (citar-insert-citation citations)
	(message "All bibliogary entries have been cited or referenced"))
    (message "Buffer is not a Denote file")))

;;;###autoload
(defun citar-denote-nobib ()
  "List citation keys referenced or cited in Denote, but not in bibliography."
  (interactive)
  (let* ((bibliography (hash-table-keys (citar-get-entries)))
         (citations (citar-denote--extract-citations))
         (references (hash-table-keys (citar-denote--get-notes)))
	 (union (cl-union citations references))
	 (nobib (cl-set-difference union bibliography :test #'string=)))
    (message "%s citations not in bibliography: %s"
             (length nobib)
             (mapconcat #'identity nobib ", "))))

;;;###autoload
(defun citar-denote-check-keywords ()
  "Check that all notes with references has a bib keyword.
Remove bib keyword when no reference, but `citar-denote-keyword' is present.
Add bib keyword when refenece is present, but `citar-denote-keyword' is missing."
  (interactive)
  (let ((files (denote-directory-files nil nil t)))
    (dolist (file files)
      (let* ((file-type (denote-filetype-heuristics file))
             (regex (citar-denote--reference-regex file-type))
             (reference-p (with-temp-buffer
                            (insert-file-contents file)
                            (re-search-forward regex nil t)))
             (keywords (denote-retrieve-front-matter-keywords-value file file-type))
             (bib-keyword-p (not (null (member citar-denote-keyword keywords)))))
        ;; Remove bib keyword when no reference but citar-denote-keyword
        (when (and (not reference-p) bib-keyword-p)
          (citar-denote--remove-bibkey file))
        ;; Add bib keyword when refenece but no citar-denote-keyword
        (when (and reference-p (not bib-keyword-p))
          (citar-denote--add-bibkey file))))))

;;;###autoload
(defun citar-denote-create-silo-note (silo)
  "Select SILO and run `citar-create-denote' in it.
SILO is a file path from `denote-silo-extras-directories'.

When called from Lisp, SILO is a directory path."
  ;; https://github.com/pprevos/citar-denote/issues/41
  (interactive
   (progn
     (require 'denote-silos-extra nil t)
     (when (featurep 'denote-silo-extras)
       (list (denote-silo-extras-directory-prompt)))))
  (if-let ((denote-directory silo))
      (progn (message "creating note in: %s" silo)
             (call-interactively #'citar-create-note))
    (message "Denote extra silo functionality not enabled")))

(define-obsolete-function-alias
  'citar-denote-find-nocite
  'citar-denote-cite-nocite "1.6")

(define-obsolete-function-alias
  'citar-denote-reference-nocite
  'citar-denote-nocite "1.8.1")

(define-obsolete-function-alias
  'citar-denote-add-citekey
  'citar-denote-add-reference "2.3")

(define-obsolete-function-alias
  'citar-denote-remove-citekey
  'citar-denote-remove-reference "2.3")

;; Citar integration

(defconst citar-denote-config
  (list :name "Denote file"
        :category 'citar-denote-mode
        :items #'citar-denote--get-notes
        :hasitems #'citar-denote--has-notes
        :open #'find-file
        :create #'citar-denote--create-note)
  "Instructing citar to use citar-denote functions.")

(defvar citar-denote--orig-source
  citar-notes-source
  "Store the `citar-notes-source' value prior to enabling `citar-denote-mode'.")

;; Initialise minor mode

(defun citar-denote--setup ()
  "Setup `citar-denote-mode'."
  (citar-register-notes-source
   'citar-denote citar-denote-config)
  (setq citar-notes-source 'citar-denote))

(defun citar-denote--reset ()
  "Reset citar to default values."
  (setq citar-notes-source citar-denote--orig-source)
  (citar-remove-notes-source 'citar-denote))

;;;###autoload
(define-minor-mode citar-denote-mode
  "Toggle integration between Citar and Denote."
  :global t
  :group 'citar
  :lighter nil
  (if citar-denote-mode
      (citar-denote--setup)
    (citar-denote--reset)))

(provide 'citar-denote)
;;; citar-denote.el ends here
