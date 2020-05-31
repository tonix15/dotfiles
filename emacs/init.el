;;; package --- init.el config

; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/usr/local/libressl/cert.pem")

;; (setq package-check-signature nil)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Server Mode
;; (use-package server
;;  :ensure nil
;;  :hook (after-init . server-mode))

;; Load which-key
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :hook
  (after-init . which-key-mode))

(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

(put 'narrow-to-region 'disabled nil)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (material)))
 '(custom-safe-themes
   (quote
    ("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" default)))
 '(package-selected-packages
   (quote
    (emmet-mode company-jedi material-theme tide web-mode elpy yasnippet-snippets which-key use-package switch-window s rainbow-mode rainbow-delimiters magit ivy-hydra init-open-recentf hungry-delete gnu-elpa-keyring-update git-gutter flycheck-inline flycheck-color-mode-line editorconfig counsel-projectile company avy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'material t)
