;; Citar-Denote minimum configuration

;; Configure package manager and use-package
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Mini Buffer Completion
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package citar
  :ensure t
  :defer t
  :custom
  ;; set bibliography's location
  (citar-bibliography '("~/documents/library/magic-tricks.bib"))
  ;; Allow multiple notes per bibliographic entry
  (citar-open-always-create-notes nil)
  :config
  :bind ("C-c w c" . citar-create-note))

(use-package denote
  :defer t
  :custom
  (denote-directory "~/documents/notes"))

(use-package citar-denote
  :ensure t
  :after (:any citar denote)
  :custom
  ;; Package defaults
  (citar-denote-file-type 'org)
  (citar-denote-keyword "bib")
  (citar-denote-signature nil)
  (citar-denote-subdir nil)
  (citar-denote-template nil)
  (citar-denote-title-format "title")
  (citar-denote-title-format-andstr "and")
  (citar-denote-title-format-authors 1)
  (citar-denote-use-bib-keywords nil)
  :config
  (citar-denote-mode)
  ;; Bind all available commands
  :bind (("C-c w n" . citar-denote-open-note)
         ("C-c w d" . citar-denote-dwim)
         ("C-c w e" . citar-denote-open-reference-entry)
         ("C-c w a" . citar-denote-add-citekey)
         ("C-c w k" . citar-denote-remove-citekey)
         ("C-c w n" . citar-denote-no-bibliography)
         ("C-c w r" . citar-denote-find-reference)
         ("C-c w l" . citar-denote-link-reference)
         ("C-c w f" . citar-denote-find-citation)
         ("C-c w x" . citar-denote-nocite)
         ("C-c w y" . citar-denote-cite-nocite)
         ("C-c w z" . citar-denote-nobib)))
