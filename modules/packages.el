;;; yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :commands yas-expand
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets" t)
  (use-package yasnippet-snippets
    :ensure t
    :config (yasnippet-snippets-initialize)))

;; ace jump
(use-package ace-jump-mode
  :ensure t
  :diminish ace-jump-mode
  :commands ace-jump-mode
  :config
  (progn
    (ace-jump-mode-enable-mark-sync)))

(use-package paren
  :defer 3
  :config
  (setq show-paren-style 'expression)
  (show-paren-mode +1))

; (use-package dropdown-list)
(use-package highlight-parentheses
  :ensure t
  :defer 3
  :diminish)

(use-package fic-mode
  :ensure t
  :defer 5
  :config (progn
            (add-to-list 'fic-highlighted-words "WARNING")))

;;; type-break setup
(use-package type-break
  :defer 5
  :init (progn
          (setq
           type-break-interval (* 45 60)
           type-break-good-rest-interval (* 2 60)
           type-break-good-break-interval (* 5 60)
           type-break-keystroke-threshold (cons 1350 6750)
           type-break-file-name "~/.emacs.d/typebreak"
           type-break-query-interval 60
           type-break-query-mode t)
          (type-break-mode t)
          (if (fboundp 'type-break-mode-line-message-mode)
              (type-break-mode-line-message-mode -1))
          (type-break-schedule)))

;; (require 'doxymacs)                     ; doxygen comments

(use-package ledger-mode
  :ensure t
  :defer t
  :init
  (progn
    (autoload 'ledger-mode "ledger-mode" nil t)
    (setq ledger-schedule-file (concat notes-dir "/santosh/finance/ledger-schedule")))
  :config
  (use-package company-ledger
    :ensure t
    :ensure company
    :after (company)
    :init
    (add-to-list 'company-backends 'company-ledger)
    :config
    (setq company-ledger-date-regexp "^[0-9]\\{2\\}/[0-9]\\{2\\}/[0-9]\\{4\\}")))

(use-package gas-mode
  :mode "\\.S'")

(use-package bc-mode
  :mode "\\.bc\\'")

;; Paredit
(use-package paredit
  :ensure t
  :hook (emacs-lisp-mode . paredit-mode)
  :diminish)

(use-package undo-tree
  :disabled t
  :diminish undo-tree-mode
  :defer 3
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package ispell :defer 5)

(use-package flyspell
  :defer 3
  :diminish flyspell-mode)

(use-package ace-isearch
  :ensure t
  :diminish
  :defer 3
  :init (global-ace-isearch-mode 1))

(use-package dired
  :defer 3
  :requires hl-line
  :init (progn
          (add-hook 'dired-mode-hook 'hl-line-mode)
          (add-hook 'dired-mode-hook 'dired-omit-mode))
  :config (setq dired-listing-switches "-alh"))

(use-package dired-x
  :defer 3
  :config
  (setq-default dired-omit-files-p t) ; Buffer-local variable
  (setq dired-omit-extensions (append dired-omit-extensions
                                 '(".ko" ".ko.cmd" ".o.cmd" ".o" ".mod.c"
                                   ".mod.o")))
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

(use-package hl-line :defer t)

(use-package auctex
  :defer t)

(use-package epg
  :defer 5
  :config
  (progn
    (setq epg-gpg-program "gpg2")))


(use-package openwith
  :ensure t
  :defer 5
  :config (progn (openwith-mode t)
               (setq openwith-associations '(("\\.pdf\\'" "xdg-open" (file))))))

(use-package flycheck
  :diminish
  :hook (c-mode-common . flycheck-mode))

(use-package all-the-icons
  :ensure t)

(use-package shell
  :defer t
  :bind (:map shell-mode-map ("<tab>" . completion-at-point))
  :custom   (comint-prompt-read-only t))

(use-package easy-jekyll
  :defer t
  :config
  (setq easy-jekyll-basedir "/home/santosh/dev/repos/fossix.github.io"))

(use-package stickyfunc-enhance :defer 5)

(use-package which-key
  :defer 3
  :config
  (setq which-key-sort-order 'which-key-prefix-then-key-order)
  (which-key-mode)
  :diminish)

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :diminish
  :config
  ;; todo do better faces
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background "gray5"))))
     `(company-scrollbar-bg ((t (:background "gray10"))))
     `(company-scrollbar-fg ((t (:background "light gray"))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face)))))))

(use-package treemacs
  :ensure t
  :init

  (use-package lsp-treemacs
    :ensure t
    :config
    (lsp-treemacs-sync-mode 1)

    :init
    (defun lsp-treemacs-symbols-toggle ()
      "Toggle the lsp-treemacs-symbols buffer."
      (interactive)
      (if (get-buffer "*LSP Symbols List*")
          (kill-buffer "*LSP Symbols List*")
        (progn (lsp-treemacs-symbols)
               (other-window -1)))))

  (use-package treemacs-projectile
    :ensure t)

  (use-package treemacs-magit
    :ensure t))

(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

(use-package golden-ratio-scroll-screen
  :defer 2
  :config
  (global-map! [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
  (global-map! [remap scroll-up-command] 'golden-ratio-scroll-screen-up))

(use-package writegood-mode
  :defer t)

(use-package smartparens :ensure t)

;;; diminish minor modes from mode-line-mode-menu
(diminish 'abbrev-mode)
(diminish 'eldoc-mode)
(diminish 'flyspell-mode)
(diminish 'flycheck-mode)
(diminish 'hs-minor-mode)
(diminish 'highlight-changes-mode)
(diminish 'auto-fill-function)
