(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq user-full-name "Zbigniew Żołnierowicz"
      user-mail-address "zbigniew.zolnierowicz@gmail.com")

(setq doom-theme 'doom-one)
(setq org-directory "~/org/")
(setq display-line-numbers-type 'relative)

(after! emms
        (emms-all)
        (emms-default-players)
        (emms-mode-line-mode 1)
        (emms-playing-time-mode 1)
        (setq emms-source-file-default-directory "~/Music/"
                emms-playlist-buffer-name "*Music*"
                emms-info-asynchronously t
                emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
  )

(map! :leader
        (:prefix ("z" . "EMMS audio player")
        :desc "Open Emms browser" "z" #'emms-browser
        :desc "Go to emms playlist" "a" #'emms-playlist-mode-go
        :desc "Emms pause track" "x" #'emms-pause
        :desc "Emms stop track" "s" #'emms-stop
        :desc "Emms play previous track" "p" #'emms-previous
        :desc "Emms play next track" "n" #'emms-next))

(setq dap-auto-configure-features '(sessions locals controls tooltip))

(global-unset-key "\C-\\")
(global-set-key "\C-\\" '+vterm/toggle)

(map! :leader
      (:prefix ("t")
       :desc "Tabs" "t" #'centaur-tabs-mode))

(after! centaur-tabs
  (centaur-tabs-group-by-projectile-project))

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(setq org-startup-indented t
          org-pretty-entities t
          org-hide-emphasis-markers t
          org-startup-with-inline-images t
          org-image-actual-width '(300))

(use-package! org-superstar
  :defer t
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-special-todo-items t))

(use-package! magit
  :defer t
  :config
  (setq magit-diff-refine-hunk (quote all)))
