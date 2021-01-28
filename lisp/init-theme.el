;; init-theme.el

(unless (package-installed-p 'nord-theme)
  (package-install 'nord-theme))

(use-package nord-theme
             :init
             (load-theme 'nord t))

(provide 'init-theme)
