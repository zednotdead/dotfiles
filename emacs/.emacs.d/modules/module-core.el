(setq
 history-length 25
 use-dialogue-box nil
 display-line-numbers 'relative)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(recentf-mode 1)
(save-place-mode 1)
(savehist-mode 1)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package no-littering
             :ensure t
             :demand t
             :init
             (setq
               backup-directory-alist `(("." . ,(no-littering-expand-var-file-name "backups/")))
               auto-save-list-file-prefix (no-littering-expand-var-file-name "auto-saves/sessions/")
               auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-saves/") t))
               url-cookie-file (no-littering-expand-var-file-name "cookies/cookies")))

(setq tab-always-indent t)
(setq text-mode-ispell-word-completion nil)

(provide 'module-core)
