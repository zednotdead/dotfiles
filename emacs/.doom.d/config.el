;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Zbigniew Żołnierowicz"
      user-mail-address "zbigniew.zolnierowicz@gmail.com")

(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

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
