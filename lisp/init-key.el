;; init-key.el

(use-package general
  :config
  (general-create-definer my-leader-def
    :prefix "SPC")

  (general-create-definer my-local-leader-def 
    :prefix "SPC m")

  (my-leader-def
   :keymaps 'normal
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(load-theme :which-key "choose theme")
    "tw" 'whitespace-mode
    "tm" 'toggle-frame-maximized
    "tM" 'toggle-frame-fullscreen))

(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-M-j") #'switch-to-buffer)
(global-set-key (kbd "M-:") 'pp-eval-expression)

(provide 'init-key)
