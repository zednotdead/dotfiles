(use-package company
  :ensure t
  :config (global-company-mode 1))

(use-package company-quickhelp
  :custom
  (company-quickhelp-delay 2)
  :hook
  (company-mode . company-quickhelp-mode))

(provide 'module-completion)
