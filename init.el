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
(setq display-line-numbers 'relative)

(dolist (mode '(term-mode-hook
		eshell-mode-hook
		help-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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
(setq-default cursor-type 'bar)

;; Turn of autosave
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Package management
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(load-theme 'doom-tokyo-night)

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

(setq treesit-load-name-override-list
   '((c++ "libtree-sitter-cpp")))

(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))

;; Ivy
(use-package counsel)
(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

(global-set-key "\C-s" 'swiper)

;; LSP
(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap
  (setq lsp-keymap-prefix "C-x l")
  :hook (;; replace XXX-mode with concrete major-mode
	 (c-mode . lsp)
	 (cpp-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package all-the-icons
  :if (display-graphic-p))

;; windmove
(use-package windmove
  :config
  (windmove-default-keybindings)
  (setq windmove-wrap-around t))

;; Company
(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
	      ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))

  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;;

 ;; Keybinds
 (keymap-global-set "C-x j" 'load-theme)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3de5c795291a145452aeb961b1151e63ef1cb9565e3cdbd10521582b5fd02e9a" "9013233028d9798f901e5e8efb31841c24c12444d3b6e92580080505d56fd392" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8" "a44e2d1636a0114c5e407a748841f6723ed442dc3a0ed086542dc71b92a87aee" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "6adeb971e4d5fe32bee0d5b1302bc0dfd70d4b42bad61e1c346599a6dc9569b5" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "c517e98fa036a0c21af481aadd2bdd6f44495be3d4ac2ce9d69201fcb2578533" "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" default))
 '(package-selected-packages
   '(doom-themes magit flycheck counsel which-key tree-sitter-langs lsp-ui lsp-ivy company-box all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
