;; Minimum citar-denote configuration
;; For texting

;; Configure package manager and use-package
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Mini Buffer Completion
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic)))

;; Citar
(use-package citar
  :ensure t
  :custom
  ;; Location of bibliography
  (citar-bibliography (directory-files "~/documents/library/" t "bib$"))
  :bind
  (("C-c w o" . citar-open)))

;; Denote
(use-package denote
  :ensure t
  :custom
  ;; Folder where Denote files live
  (denote-directory "~/documents/notes/")
  :hook
  ;; Pretty filenames
  (dired-mode . denote-dired-mode))

;; Citar-denote
(use-package citar-denote
  :load-path
  "~/documents/projects/citar-denote/"
  :demand t
  :custom
  ;; Package defaults
  (citar-denote-keyword "bib")
  (citar-denote-file-type 'org)
  (citar-denote-subdir nil)
  (citar-denote-signature nil)
  (citar-denote-template nil)
  (citar-denote-use-bib-keywords nil)
  (citar-denote-title-format "title")
  (citar-denote-title-format-authors 1)
  (citar-denote-title-format-andstr "and")
  ;; Allow multiple notes per entry
  (citar-open-always-create-notes nil)
  :init
  (citar-denote-mode)
  ;; Bind all available commands
  :bind (("C-c w c" . citar-create-note)
         ("C-c w n" . citar-denote-open-note)
         ("C-c w d" . citar-denote-dwim)
         ("C-c w e" . citar-denote-open-reference-entry)
         ("C-c w a" . citar-denote-add-citekey)
         ("C-c w k" . citar-denote-remove-citekey)
         ("C-c w r" . citar-denote-find-reference)
         ("C-c w f" . citar-denote-find-citation)
         ("C-c w l" . citar-denote-link-reference)
         ("C-c w x" . citar-denote-nocite)
         ("C-c w y" . citar-denote-cite-nocite)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(denote citar orderless vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight regular :height 120 :width normal)))))
