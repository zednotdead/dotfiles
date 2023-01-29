;; -*- lexical-binding: t; -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7" default))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Global keymaps
;; Make ESC be immediate
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Minimize setup
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(set-face-attribute 'default nil :font "Iosevka Term" :height 150)
(setq visible-bell nil)

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

;; 

;; Evil mode

(use-package undo-tree
  :config (global-undo-tree-mode))

(use-package evil-leader)

(use-package evil
  :requires (undo-tree)
  :config (evil-mode 1)
  (evil-set-undo-system 'undo-tree))

(use-package evil-surround
  :after '(evil)
  :config
  (global-evil-surround-mode 1))

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
  :config (load-theme 'gruvbox-dark-hard))

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

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package dap-mode)

;; Projectile

(use-package projectile
  :init
  (projectile-mode +1))

(general-nmap
  :prefix "SPC"
  "o" 'treemacs)
