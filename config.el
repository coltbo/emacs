(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; Vterm keybinds
(keymap-global-set "C-x t o" 'vterm-other-window)

;; Window keybinds
(keymap-global-set "C-x w c" 'delete-window)
(keymap-global-set "C-x w s" 'split-window-below)
(keymap-global-set "C-x w v" 'split-window-right)
(keymap-global-set "C-x w h" 'windmove-left)
(keymap-global-set "C-x w l" 'windmove-right)
(keymap-global-set "C-x w j" 'windmove-down)
(keymap-global-set "C-x w k" 'windmove-up)

(use-package toc-org
  :straight t
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets
  :straight t)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

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
;; Makes commented text and keywords italics.
;; This is working in emacslient but not emacs
;; Your font must have an italic face available
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

(add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font-11"))

(setq-default line-spacing 0.12)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(load-theme 'modus-vivendi t)

(global-display-line-numbers-mode 1)
(global-visual-line-mode t)
(setq display-line-numbers 'relative)

(use-package which-key
  :straight t
  :init (which-key-mode 1))

(use-package tree-sitter
  :straight t)
(use-package tree-sitter-langs
  :straight t)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(use-package lsp-mode
  :straight t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c-mode . lsp)
         (cpp-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)

(setq lsp-ui-sideline-enable nil)
(setq lsp-ui-sideline-show-hover nil)
;; if you are ivy user
(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
  :straight t
  :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode
  :straight t)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; The path to lsp-mode needs to be added to load-path as well as the
;; path to the `clients' subdirectory.
(add-to-list 'load-path (expand-file-name "lib/lsp-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lib/lsp-mode/clients" user-emacs-directory))

(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

(use-package company
  :straight t
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
  :straight t
  :hook (company-mode . company-box-mode))

(use-package magit
  :straight t)

(use-package vterm
  :load-path "/home/colten/.config/emacs/modules/emacs-libvterm/")
