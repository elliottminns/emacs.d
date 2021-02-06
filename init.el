;;; init.el --- elliott's emacs configuration
;; copyright (c) 2021 elliott minns

;; author: elliott minns <elliott.minns@me.com>
;; load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.

(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

(require 'init-window)

(require 'init-package)
(require 'init-key)
(require 'init-lsp)
(require 'init-theme)
(require 'init-evil)
(require 'init-ui)
(require 'init-flycheck)
(require 'init-dev)
(require 'init-project)
(require 'init-search)
(require 'init-company)
(require 'init-go)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "fce3524887a0994f8b9b047aef9cc4cc017c5a93a5fb1f84d300391fba313743" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "e3b2bad7b781a968692759ad12cb6552bc39d7057762eefaf168dbe604ce3a4b" default))
 '(package-selected-packages '(nord-theme evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


