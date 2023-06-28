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

(use-package evil
  :straight t
  :ensure t ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
  (setq evil-want-keybinding nil)
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :config ;; tweak evil after loading it
  (evil-mode)

 (evil-define-key 'normal 'global (kbd "C-l") 'evil-window-right)
 (evil-define-key 'normal 'global (kbd "C-h") 'evil-window-left)
 (evil-define-key 'normal 'global (kbd "C-k") 'evil-window-up)
 (evil-define-key 'normal 'global (kbd "C-j") 'evil-window-down)
 (evil-define-key 'normal 'global (kbd ", v") 'evil-window-vsplit)
 (evil-define-key 'normal 'global (kbd ", s") 'evil-window-split))

(use-package evil-collection
   :straight t
   :after evil
   :ensure t
   :config
   (evil-collection-init '(magit)))

(use-package general
  :straight t
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer cb/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC")) ;; access leader in insert mode

(cb/leader-keys
  "b" '(:ignore t :wk "Buffer")
  "b b" '(switch-to-buffer :wk "Switch buffer")
  "b i" '(ibuffer :wk "IBuffer")
  "b k" '(kill-this-buffer :wk "Kill this buffer")
  "b n" '(next-buffer :wk "Next buffer")
  "b p" '(previous-buffer :wk "Previous buffer")
  "b r" '(revert-buffer :wk "Reload buffer"))

(cb/leader-keys
  "f" '(:ignore t :wk "file")
  "f f" '(find-file :wk "Find file"))

(cb/leader-keys
  "e" '(:ignore t :wk "Evaluate")
  "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
  "e d" '(eval-defun :wk "Evaluate defun containing or after point")
  "e e" '(eval-expression :wk "Evalaute an elisp expression")
  "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
  "e r" '(eval-region :wk "Evaluate elisp in region"))

(use-package toc-org
  :straight t
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets
  :straight t)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(set-face-attribute 'default nil
  :font "JetBrains Mono Nerd Font"
  :height 110
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "Ubuntu"
  :height 120
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "JetBrains Mono Nerd Font"
  :height 110
  :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacslient but not emacs
;; Your font must have an italic face available
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

(add-to-list 'default-frame-alist '(font . "JetBrains Mono Nerd Font-11"))

(setq-default line-spacing 0.12)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(load-theme 'modus-vivendi t)

(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

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
