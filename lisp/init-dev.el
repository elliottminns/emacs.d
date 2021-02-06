;; init-dev.el

(use-package projectile
               :ensure t
               :init
	       (setq projectile-completion-system 'ivy)
               (projectile-mode +1)
               :bind 
               (:map projectile-mode-map ("C-c p" . projectile-command-map)))

(use-package counsel-projectile
  :config (counsel-projectile-mode 1))

(column-number-mode)

;; Enable line numbers for prog modes only
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))

(use-package paren
  :hook (prog-mode . show-paren-mode))

(use-package rainbow-mode
  :commands rainbow-mode)

(use-package flycheck
  :defer t
  ;; :hook ((clojure-mode . flycheck-mode)
  ;;        (clojurec-mode . flycheck-mode)
  ;;        (clojurescript-mode . flycheck-mode))
)

(provide 'init-dev)
