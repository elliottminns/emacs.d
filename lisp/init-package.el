;; init-package.el

(eval-and-compile
  (require 'package)
  (setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                           ;("marmalade" . "https://marmalade-repo.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")))
  (package-initialize)
  ;; i always fetch the archive contents on startup and during compilation, which is slow
  (package-refresh-contents)
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (require 'use-package)
  (setf use-package-always-ensure t))

(provide 'init-package)
