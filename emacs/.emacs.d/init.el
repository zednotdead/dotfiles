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

(add-to-list 'default-frame-alist '(font . "Iosevka Term-15"))
(set-face-attribute 'default nil :font "Iosevka Term" :height 150)

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

(use-package undo-tree
  :config (global-undo-tree-mode))

(use-package evil-leader)

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
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
  (global-evil-surround-mode 1))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(evil-set-leader nil (kbd "SPC"))
(evil-set-leader 'normal (kbd "C-SPC"))

;; Better keybinding

(use-package general
  :after (evil)
  :config (general-evil-setup))

;; Theming

(use-package telephone-line
  :config
  (telephone-line-mode 1))

(use-package gruvbox-theme
  :config (load-theme 'gruvbox-dark-hard t))

;; Beancount

(straight-use-package '(beancount-mode :host github :repo "beancount/beancount-mode"))
(use-package no-littering)

(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))

;; Treemacs

(use-package treemacs
  :defer t)
(use-package treemacs-evil
  :after (treemacs evil))

;; LSP

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  (setq lsp-beancount-langserver-executable "beancount-language-server")
  (setq lsp-beancount-journal-file "tx.beancount")
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(add-to-list 'auto-mode-alist '("\\.yaml\\'" . lsp-mode))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package dap-mode)

;; Projectile

(use-package projectile
  :config
  (projectile-mode +1)
  (treemacs-project-follow-mode +1))

(use-package treemacs-projectile
  :after '(treemacs projectile))

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
          "\\*Warnings\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

;; Key bindings

(general-nmap
  :prefix "<leader>"
  "o" 'treemacs
  "t" 'vterm
  "p" 'projectile-switch-project
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
  "C-g" 'evil-normal-state)
