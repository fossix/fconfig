(setq read-process-output-max fconfig/1MB)

(use-package lsp-mode
  :diminish
  :commands (lsp lsp-deferred)
  :hook ((c-mode rust-mode go-mode) . lsp-deferred)
  :bind (:map prog-mode-map
              ("M-g r" . lsp-rename))
  :config
  (setq lsp-file-watch-threshold nil
        lsp-idle-delay 0.5)

  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(use-package consult-lsp
  :ensure t
  :after lsp-mode)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :after lsp-mode)

(use-package lsp-ui-doc
  :after lsp-ui
  :config
  (setq lsp-ui-doc-include-signature t
        lsp-ui-doc-delay 2
        lsp-ui-sideline-delay 1.5))

(use-package company-capf
  :requires company
  :after lsp-mode
  :config
  (push 'company-capf company-backends))
