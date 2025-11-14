;; C mode keybindings
(defun fconfig/c-mode-bindings ()
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (define-key c-mode-base-map [f4] 'speedbar-get-focus)
  (define-key c-mode-base-map [(control f5)] 'next-error)
  (define-key c-mode-base-map [f6] 'gdb)
  (define-key c-mode-base-map [f5] 'recompile))
(add-hook 'c-mode-common-hook 'fconfig/c-mode-bindings)

;; Hooks
(defun program-mode-hook ()
  (which-function-mode 1)
  (whitespace-mode 1)
  (flyspell-prog-mode)
  (hs-minor-mode)
  (fic-mode)
  (fci-mode)
  (highlight-parentheses-mode)
  (configure-prettify-symbols-alist)
  (smartparens-mode)
  (when (featurep 'yasnippet)
    (yas-minor-mode)))

(add-to-list 'prog-mode-hook 'program-mode-hook)

(defun c-specfic-hooks ()
  (c-set-style "linux")
  (setq indent-tabs-mode t
        tab-width 8
        fill-column 80
        gdb-many-windows 1
        c-hungry-delete-key t
        backward-delete-function nil))

(defun list-lang-hooks ()
  (paredit-mode +1))

(defun asm-mode-hooks ()
  (setq asm-comment-char ?\#
        indent-tabs-mode t
        tab-width 8)
  (electric-indent-mode -1)
  (define-key asm-mode-map [f5] 'recompile))

(defun python-mode-hooks ()
  (setq python-indent-offset 4
        fill-column 79))

(add-hook 'c-mode-common-hook 'c-specfic-hooks)
(add-hook 'python-mode-ts-hook 'python-mode-hooks)
(add-hook 'lisp-mode-hook 'list-lang-hooks)
(add-hook 'emacs-lisp-mode-hook 'list-lang-hooks)
(add-hook 'scheme-mode-hook 'list-lang-hooks)
(add-hook 'asm-mode-hook 'asm-mode-hooks)
(add-hook 'js-mode-hook 'program-mode-hook)
(add-hook 'html-mode-hook 'program-mode-hook)

(use-package octave
  :mode ("\\.m\\'" . octave-mode)
  :config
  (add-hook 'octave-mode-hook
            (lambda ()
              (abbrev-mode 1)
              (if (eq window-system 'x)
                  (font-lock-mode 1)))))

(use-package go-mode
  :ensure t
  :hook ((before-save . gofmt-before-save)
         (go-mode . go-eldoc-setup))
  :mode ("\\.go\\'" . go-mode)
  :config
  (setenv "GOPATH" (expand-file-name "~/dev/gws"))
  (add-to-list 'exec-path (expand-file-name "~/gws/bin"))
  (set (make-local-variable 'compile-command) "go build -v")
  (use-package go-eldoc :ensure t))

(use-package rustic
  :ensure t
  :config
  ;; Use lsp-mode as the LSP client
  (setq rustic-lsp-client 'lsp-mode)
  ;; Ensure Emacs can find cargo and rust tools
  (when (file-exists-p "~/.cargo/bin")
    (add-to-list 'exec-path (expand-file-name "~/.cargo/bin")))
  ;; Disable rustic's own flycheck in favor of LSP
  (setq rustic-flycheck-setup-mode-line-p nil)
  ;; Let LSP handle formatting instead of rustic's direct rustfmt calls
  (setq rustic-format-on-save nil)
  (setq rustic-lsp-format t))

;; c-eldoc
(use-package c-eldoc
  :ensure t
  :defer 5
  :hook (c-mode . c-turn-on-eldoc-mode)
  :diminish
  :config (setq c-eldoc-includes "-I./ -I../ -I/usr/include"))

(use-package slime
  :ensure t
  :defer 5
  :config (setq inferior-lisp-program "/usr/bin/clisp"))
