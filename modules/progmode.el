;; C mode keybindings
(defun fconfig/c-mode-bindings ()
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (define-key c-mode-base-map [f4] 'speedbar-get-focus)
  (define-key c-mode-base-map [(control f5)] 'next-error)
  (define-key c-mode-base-map [f6] 'gdb)
  (define-key c-mode-base-map [f5] 'recompile))
(add-hook 'c-mode-common-hook 'fconfig/c-mode-bindings)

;; My C style
(defconst c-style-santosh
  '( "linux"
     (c-echo-syntactic-information-p . nil)
     (setq-default c-basic-offset 8)
     (c-hanging-braces-alist . ((brace-list-open)
                                (brace-entry-open)
                                (statement-cont)
                                (defun-open before after)
                                (defun-close before after)
                                (block-open )
                                (block-close . c-snug-do-while)))
     (c-cleanup-list . (brace-else-brace
                        brace-elseif-brace
                        scope-operator
                        empty-defun-braces
                        defun-close-semi
                        comment-close-slash
                        defun-close-semi
                        list-close-comma
                        empty-defun-braces)))
    "C Style Santosh")

(c-add-style "santosh" c-style-santosh)

;; Hooks
(defun program-mode-hook ()
  (which-function-mode 1)
  (whitespace-mode 1)
  (flyspell-prog-mode)
  (hs-minor-mode)
  (linum-mode)
  (fic-mode)
  (fci-mode)
  (highlight-parentheses-mode)
  (configure-prettify-symbols-alist)
  (autopair-mode)
  (auto-fill-mode 1)
  (bind-key "<tab>" 'indent-for-tab-command)
  (when (featurep 'yasnippet)
    (yas-minor-mode)))

(add-to-list 'prog-mode-hook 'program-mode-hook)

(defun c-specfic-hooks ()
  (c-set-style "santosh")
  (setq indent-tabs-mode t
        tab-width 8
        fill-column 80
        gdb-many-windows 1
        c-hungry-delete-key t
        backward-delete-function nil)
  (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
  (set-compile-command))

;;; from emacswiki
(defun set-compile-command ()
  "our compile command"
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
         (let ((file (file-name-nondirectory buffer-file-name)))
           (format "%s -o %s %s %s"
                   (or (getenv "CC") "gcc")
                   (file-name-sans-extension file)
                   (or (getenv "CFLAGS") "-DDEBUG -pedantic -Wall -ggdb")
                   file)))))

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
(add-hook 'python-mode-hook 'python-mode-hooks)
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
  :hook ((before-save . gofmt-before-save)
         (go-mode . go-eldoc-setup))
  :mode ("\\.go\\'" . go-mode)
  :config
  (setenv "GOPATH" (expand-file-name "~/dev/gws"))
  (add-to-list 'exec-path (expand-file-name "~/gws/bin"))
  (set (make-local-variable 'compile-command) "go build -v")
  (use-package go-eldoc))

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (setq rust-format-on-save t))

;; c-eldoc
(use-package c-eldoc
  :defer 5
  :hook (c-mode . c-turn-on-eldoc-mode)
  :diminish
  :config (setq c-eldoc-includes "-I./ -I../ -I/usr/include"))

(use-package slime-autoloads
  :defer 5
  :config (setq inferior-lisp-program "/usr/bin/clisp"))

(use-package helm-xref
  :diminish
  :bind (:map prog-mode-map
              ("M-." . xref-find-definitions)
              ("C-M-," . xref-find-references)
              ("C-M-." . xref-find-apropos)
              ("M-," . xref-pop-marker-stack)))
