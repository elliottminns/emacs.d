;; init-ui.el

;(use-package ido
             ;:ensure t
             ;:init
             ;(setq ido-enable-flex-matching t)
             ;(setq ido-default-file-method 'selected-window)
             ;(setq ido-default-buffer-method 'selected-window)
             ;:config
             ;(ido-mode 1)
             ;(define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil))

;(use-package ido-grid-mode)

(use-package ivy
             :ensure t
             :config
             (ivy-mode 1))

(provide 'init-ui)
