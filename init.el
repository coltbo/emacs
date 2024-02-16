;;; packge --- Summary

;;; Commentary:
;;; Colten's Emacs config

;;; Code:

;; Update some UI
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(dolist (mode '(term-mode-hook
		eshell-mode-hook
		help-mode-hook)))

(load-theme 'zenburn t)

(set-face-attribute 'default nil
		    :font "Iosevka Nerd Font"
		    :height 110
		    :weight 'medium)
(set-face-attribute 'variable-pitch nil
		    :font "Iosevka Nerd Font"
		    :height 120
		    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
		    :font "Iosevka Nerd Font"
		    :height 110
		    :weight 'medium)

(add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font-11"))

(setq-default line-spacing 0.12)

;; Changing bar
(setq-default cursor-type 't)

;; Turn of autosave
(setq auto-save-default nil)
(setq make-backup-files nil)

(setq gdb-many-windows 1)

;; Package management
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package zenburn-theme)

;; Which-key
(use-package which-key
  :init (which-key-mode 1))

;; Treesitter
(require 'treesit)
(setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(cpp "https://github.com/tree-sitter/tree-sitter-cpp")))

(dolist (lang treesit-language-source-alist)
  (unless (treesit-language-available-p (car lang))
    (treesit-install-language-grammar (car lang))))

(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))

;; Eglot
(use-package eglot
  :hook (c-ts-mode . eglot-ensure)
        (c++-ts-mode . eglot-ensure))

;; Company
(use-package company
  :hook
    (c-ts-mode . company-mode)
    (c++-ts-mode . company-mode))

(setq company-minimum-prefix-length 1
      company-idle-delay 0.0)

(use-package counsel)
(use-package ivy
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-h f") 'counsel-describe-function)
  (global-set-key (kbd "C-h v") 'counsel-describe-variable)
  (global-set-key (kbd "C-h l") 'counsel-find-library)
  (global-set-key (kbd "C-h i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "C-h u") 'counsel-unicode-char)
  (global-set-key (kbd "C-h j") 'counsel-set-variable)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-c v") 'ivy-push-view)
  (global-set-key (kbd "C-c V") 'ivy-pop-view)
  (global-set-key (kbd "C-c c") 'counsel-compile)
  (global-set-key (kbd "C-c L") 'counsel-git-log)
  (global-set-key (kbd "C-c k") 'counsel-rg)
  (global-set-key (kbd "C-c n") 'counsel-fzf)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-c J") 'counsel-file-jump))

(use-package elfeed)

(setq elfeed-feeds
      '("http://lwn.net/headlines"
	"https://itsfoss.com/rss"
	"https://archlinux.org/feeds/news"
	"https://www.linux.com/feed"
	"https://podcast.thelinuxexp.com/@tlenewspodcast/feed.xml"
	"https://planet.emacslife.com/atom.xml"))

(use-package magit
  :config
  (global-set-key (kbd "C-c g") 'magit))

;; windmove
(use-package windmove
  :config
  (windmove-default-keybindings)
  (setq windmove-wrap-around t))

;; keybinds
(keymap-global-set "C-c C-u" 'uncomment-region)	
