(use-package general
  :ensure (:wait t)
  :demand t
  :config
  (general-define-key
   :keymaps 'normal
   :prefix "SPC"
   "TAB" 'treemacs)
  (general-define-key
   :keymaps 'normal
   :prefix "SPC"
   "p" 'projectile-command-map)
  (general-define-key
   :keymaps 'normal
   :prefix "SPC f"
   "i" 'projectile-find-file)
  (general-define-key
   :keymaps 'normal
   :prefix "SPC SPC"
   "SPC" 'evilnc-comment-or-uncomment-lines)
  (general-define-key
   :keymaps 'insert
   "C-SPC" 'company-complete)
  (general-define-key
   :keymaps 'normal
   :prefix "SPC l"
   "d" 'flymake-show-buffer-diagnostics)
  (general-define-key
   :keymaps 'normal
   :prefix "SPC l"
   "D" 'flymake-show-project-diagnostics)
  (general-define-key
    :keymaps 'normal
    :prefix "g"
    "t" 'centaur-tabs-forward-group)
  (general-define-key
    :keymaps 'normal
    :prefix "g"
    "T" 'centaur-tabs-backward-group)
  (general-define-key
    :keymaps 'normal
    :prefix "SPC t"
    "o" 'centaur-tabs-kill-other-buffers-in-current-group)
  (general-define-key
    :keymaps 'normal
    :prefix "SPC t"
    "a" 'centaur-tabs-ace-jump)
  (general-define-key
    :keymaps '(normal visual)
    ";" 'avy-goto-char-2)
  )

(use-package which-key
  :config (which-key-mode))

(provide 'module-keymaps)
