(setq read-process-output-max fconfig/1MB)

(use-package lsp-mode
  :diminish
  :commands (lsp lsp-deferred)
  :hook ((c-mode rust-mode go-mode) . lsp-deferred)
  :bind (:map prog-mode-map
              ("M-g r" . lsp-rename))
  :config
  (setq lsp-file-watch-threshold nil))

(use-package ccls
  :defer t
  :init (setq ccls-executable "/usr/bin/ccls"))

(use-package lsp-clients
  :after lsp-mode)

(use-package helm-lsp
  :requires helm
  :after lsp-mode)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :after lsp-mode)

(use-package lsp-ui-doc
  :after lsp-ui
  :config
  (setq lsp-ui-doc-include-signature t
        lsp-ui-doc-delay 2
        lsp-ui-sideline-delay 1.5)

  (add-hook 'lsp-ui-doc-frame-hook
            (lambda (frame _w)
              (set-face-attribute 'default frame :font "Monaco-10"))))

(use-package company-lsp
  :requires company
  :after lsp-mode
  :config
  (push 'company-lsp company-backends))
