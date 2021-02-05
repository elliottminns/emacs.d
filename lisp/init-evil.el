;; init-evil.el

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after (evil)
  :init
  (evil-collection-init))

(provide 'init-evil)
