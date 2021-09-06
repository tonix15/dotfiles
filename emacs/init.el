;;; package --- init.el config

;; (org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
					; (org-babel-load-file (expand-file-name "~/.dotfiles/emacs/custom.org"))

;; User Interface
(setq inhibit-startup-message t)

(scroll-bar-mode -1) ; disable visible scrollbar
(tool-bar-mode -1)   ; disable the toolbar
(tooltip-mode -1)    ; disable tooltips
(set-fringe-mode 10) ; give some breathing room

(menu-bar-mode -1)   ; disable menu bar

(setq visible-bell t) ; set up visual bell

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

; set font
(set-face-attribute 'default nil :font "Fira Code")

; set default theme
(load-theme 'wombat t)

;; enable line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
;; disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode(lambda () (display-line-numbers-mode 0))))

;; Initialize package sources
(require 'package)

; (setq package-enable-at-startup nil)

(setq package-archives
             '(("melpa" . "https://melpa.org/packages/")
	       ("org" . "https://orgmode.org/elpa/")
	       ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Load which-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :hook
  (after-init . which-key-mode))

;; add ivy package
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; add doom modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; add rainbow delimiter
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
