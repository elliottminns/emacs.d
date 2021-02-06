;; init-project.el

(use-package projectile
  :blackout
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1)
  :bind 
  (:map projectile-mode-map ("C-c p" . projectile-command-map)))

(use-package counsel-projectile
  :config (counsel-projectile-mode 1))

(provide 'init-project)
