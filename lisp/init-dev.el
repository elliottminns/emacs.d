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

(provide 'init-dev)
