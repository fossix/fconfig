;;; yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :commands yas-expand
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets" t)
  (use-package yasnippet-snippets
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

(use-package autopair
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
    (setq ledger-schedule-file (concat notes-dir "/santosh/finance/ledger-schedule"))))

(use-package gas-mode
  :mode "\\.S'")

(use-package bc-mode
  :mode "\\.bc\\'")

(use-package pos-tip
  :ensure t
  :defer t
  :config
  (defadvice popup-menu-show-quick-help
      (around pos-tip-popup-menu-show-quick-help () activate)
    "Show quick help using `pos-tip-show'."
    (if (eq window-system 'x)
        (let ((doc (popup-menu-document
                    menu (or item
                             (popup-selected-item menu)))))
          (when (stringp doc)
            (pos-tip-show doc nil
                          (if (popup-hidden-p menu)
                              (or (plist-get args :point)
                                  (point))
                            (overlay-end (popup-line-overlay
                                          menu (+ (popup-offset menu)
                                                  (popup-selected-line menu)))))
                          nil 0)
            nil))
      ad-do-it)))

;; Paredit
(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode)
  :diminish)

(use-package undo-tree
  :diminish undo-tree-mode
  :defer 3
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package ace-window
  :defer 3
  :init (ace-window-display-mode))

(use-package ispell :defer 5)

(use-package flyspell
  :defer 3
  :diminish flyspell-mode)

(use-package ace-isearch
  :diminish
  :defer 3
  :init (global-ace-isearch-mode 0))

(use-package helm-ag :defer 5)

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
  :defer 5
  :config (progn (openwith-mode t)
               (setq openwith-associations '(("\\.pdf\\'" "xdg-open" (file))))))

(use-package powerline
  :defer 2
  :config (powerline-default-theme)
  (setq display-time-default-load-average nil))

(use-package flycheck
  :diminish
  :hook (c-mode-common . flycheck-mode))

(use-package all-the-icons)

(use-package projectile
  :diminish
  :defer 5
  :init
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package helm-projectile
  :hook ((after-init . helm-projectile-on))
  :init
  (setq projectile-completion-system 'helm))

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

;;; diminish minor modes from mode-line-mode-menu
(diminish 'abbrev-mode)
(diminish 'eldoc-mode)
(diminish 'flyspell-mode)
(diminish 'flycheck-mode)
(diminish 'hs-minor-mode)
(diminish 'highlight-changes-mode)
(diminish 'auto-fill-function)
