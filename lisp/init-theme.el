;; init-theme.el

(unless (package-installed-p 'nord-theme)
  (package-install 'nord-theme))

(load-theme 'nord t)

(provide 'init-theme)
