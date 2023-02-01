;;; package --- Summary:
;; Config file for my Emacs distribution.
;;; Commentary:
;;
;;; Code:
;; -*- lexical-binding: t; -*-
;; Global keymaps
;; Make ESC be immediate
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Minimize setup
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(setq visible-bell nil)

;;; Setup font
(defvar my-font-name)
(defvar my-font-size)
(defvar my-fontspec)

(setq my-font-name (cond
		    ((eq system-type 'darwin) "Iosevka Nerd Font")
		    (t "Iosevka")))

(setq my-font-size (cond
		    (t 15)))

(setq my-fontspec (concat my-font-name "-" (number-to-string my-font-size)))
(add-to-list 'default-frame-alist `(font . ,my-fontspec))
(set-face-attribute 'default t :font my-font-name :height (* my-font-size 10))

;; Install use-package

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
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

;; Plugins

;; Which-key

(use-package which-key
  :config (which-key-mode 1))

;; Ivy

(use-package ivy-prescient)

(use-package ivy
  :diminish
  :requires (ivy-prescient)
  :config
  (ivy-mode 1)
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1))

;; Evil mode

(defvar evil-want-keybinding)
(defvar evil-want-integration)

(setq evil-want-keybinding nil)
(setq evil-want-integration t)

(use-package undo-tree
  :config (global-undo-tree-mode))

(use-package evil-leader)

(use-package evil
  :init
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1)
  (evil--add-to-alist
   'evil-surround-pairs-alist
   ?\( '("(" . ")")
   ?\[ '("[" . "]")
   ?\{ '("{" . "}")
   ?\) '("( " . " )")
   ?\] '("[ " . " ]")
   ?\} '("{ " . " }")))

(evil-set-leader 'normal (kbd "SPC"))

;; Magit

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit)

(use-package diff-hl
  :config
  (global-diff-hl-mode))

;; Better keybinding

(use-package general
  :after (evil)
  :config (general-evil-setup))

;; Theming

(use-package telephone-line
  :config
  (telephone-line-mode 1))

;; (use-package gruvbox-theme
;;  :config (load-theme 'gruvbox-dark-hard t))

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-woodland t))
;; Beancount

(straight-use-package '(beancount-mode :host github :repo "beancount/beancount-mode"))
(use-package no-littering)

(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))

;; YAML

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

;; JS/TS

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package typescript-mode
  :after (:all tree-sitter)
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

(straight-use-package '(tsi :type git :host github :repo "orzechowskid/tsi.el"))
(straight-use-package '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el" :branch "emacs28"))
(require 'tsx-mode)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-mode))

;; Treemacs

(use-package treemacs
  :defer t)
(use-package treemacs-evil
  :after (treemacs evil))

;; Lua

(use-package lua-mode)

;; LSP

(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  (setq lsp-beancount-langserver-executable "beancount-language-server")
  (setq lsp-beancount-journal-file "tx.beancount")
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  :hook ((lsp-mode . lsp-enable-which-key-integration)
	 (yaml-mode . lsp-deferred)
	 (lua-mode . lsp-deferred)
	 (javascript-mode . lsp-deferred)
	 (typescript-mode . lsp-deferred))
  :commands (lsp lsp-deferred))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package dap-mode)

(use-package company
  :config
  (global-company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; Projectile

(use-package projectile
  :config
  (projectile-mode +1)
  (treemacs-project-follow-mode +1))

(use-package treemacs-projectile
  :after '(treemacs projectile))
(use-package counsel-projectile
  :after projectile)
(use-package ripgrep)
(use-package ag)

;; Terminal

(use-package vterm)
(use-package popper
  :bind (("C-`"   . popper-toggle-latest)
	 ("C-M-`"   . popper-cycle))
  :init
  (setq popper-reference-buffers
	'("\\*Messages\\*"
	  "Output\\*$"
	  "\\*Async Shell Command\\*"
	  "\\*vterm\\*"
	  "\\*ripgrep-search\\*"
	  help-mode
	  compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

;; Dashboard

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-content-center t)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-items '((recents  . 5)
			  (bookmarks . 5)
			  (projects . 5)))
  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name))

;; Centaur Tabs

(use-package all-the-icons
  :if (display-graphic-p))

(use-package centaur-tabs
  :after all-the-icons
  :init
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-style "wave")
  (setq centaur-tabs-height 32)
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-group-by-projectile-project)
  :bind
  ("C-S-h" . centaur-tabs-backward)
  ("C-S-l" . centaur-tabs-forward))

;; Key binding

(general-nmap
  :prefix "<leader>"
  "o" 'treemacs
  "t" 'vterm
  "p" 'counsel-projectile-switch-project
  "g" 'magit-status
  "e" '(:ignore t :which-key "emacs"))

(general-nmap
  :prefix "<leader> e"
  "c" 'calc)

(general-nmap
  :keymaps 'emacs-lisp-mode-map
  :prefix "<leader> e"
  "x" 'eval-buffer)

(general-nmap
  "C-h" 'evil-window-left
  "C-l" 'evil-window-right
  "C-j" 'evil-window-down
  "C-k" 'evil-window-up)

(general-imap
  "C-g" 'evil-normal-state
  "C-SPC" 'company-manual-begin)

(general-imap
  :keymaps 'company-mode-map
  "ESC" 'company-abort)

(general-nmap
  :prefix "SPC"
  "x" 'kill-current-buffer)

(provide 'init)
;;; init.el ends here
