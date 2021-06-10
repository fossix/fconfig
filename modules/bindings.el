;;; For bookmarks and registers, Lets make all bindings follow C-c
;;; C-x prefix for single key commands

(general-unbind "C-c")                  ; we will C-c as base for all modules
(general-unbind "C-x v")                ; unbind vc map
(general-unbind "C-x r")                ; unbind bookmark and register

(general-create-definer fconfig-C-c-bind
  :prefix "C-c"
  :name "Global shortcuts with C-c prefix")

(general-create-definer fconfig-C-x-bind
  :prefix "C-x"
  :name "Global shortcuts with C-x prefix")

(general-create-definer fconfig-bookmark-bind
  :prefix "C-c r"
  "" '(:ignore t :which-key "Bookmark and registers")
  :name "Bookmark and registers")

(fconfig-C-c-bind
  "x s" 'eshell
  "x r" 'projectile-run-async-shell-command-in-root
  "s" 'projectile-run-eshell)

(fconfig-bookmark-bind
  "B" 'bookmark-set
  "b" 'bookmark-set-no-overwrite
  "l" 'list-bookmarks
  "j" 'bookmark-jump
  "J" 'bookmark-jump-other-window
  "r" 'view-register
  "C-l" 'list-registers
  "d" 'register-describe-oneline
  "p" 'point-to-register
  "." 'jump-to-register
  "s" 'copy-to-register
  "i" 'insert-register
  "a" 'append-to-register
  "C-p" 'prepend-to-register
  "R" 'copy-rectangle-to-register
  "w" 'window-configuration-to-register
  "f" 'frame-configuration-to-register
  "n" 'number-to-register
  "+" 'increment-register
  "m" 'kmacro-to-register
  "C-s" 'bookmark-save)

(when (featurep 'fconfig-buffer)
  (fconfig-buffer-bind
    "g" 'writegood-mode
    "w" 'whitespace-mode
    "l" 'recenter-top-bottom
    "p" 'reposition-window
    "s" 'flyspell-buffer
    "b" 'bury-buffer
    "r" 'revert-buffer
    "f" 'fci-mode))

(when (featurep 'fconfig-org-config)
  (fconfig-org-config-bind
    "I" 'punch-in
    "O" 'punch-out
    "l" 'clock-in-last-task
    "c" 'org-capture
    "a" 'org-agenda
    "l" 'org-store-link
    "t" 'org-todo-list
    "b" 'org-brain-goto
    "v" 'org-brain-visualize
    "o" 'org-occur-in-agenda-files
    "s" 'org-search-view
    "r" 'org-refile
    "m" 'org-timer-set-timer))

(when (featurep 'fconfig-vc)
  (fconfig-vc-bind
    "d" 'magit-diff-buffer-file
    "D" 'magit-diff-dwim
    "C-d" 'magit-diff-staged
    "F" 'magit-file-dispatch
    "M" 'magit-dispatch
    "l" 'magit-log-buffer-file
    "L" 'magit-log-all
    "b" 'magit-blame-addition
    "s" 'magit-status
    "t" 'git-timemachine-toggle
    "g" 'helm-git-grep
    "c" 'magit-branch-checkout
    "<RET>" 'helm-ls-git-ls
    "i" 'gist-region-or-buffer-private
    "I" 'gist-region-or-buffer
    "M-p" 'magit-pull-from-upstream
    "M-P" 'magit-pull
    "M-f" 'magit-fetch-from-upstream
    "M-F" 'magit-fetch-all
    "M-u" 'magit-push-current-to-upstream
    "M-U" 'magit-push-current
    "M-b" 'magit-rebase-branch
    "M-B" 'magit-rebase
    "M-r" 'magit-reset-worktree))

;; searching and jumping
(general-create-definer fconfig-search-bind
  :prefix "M-g"
  :name "Search and jump"
  "" '(:ignore t :which-key "Search and jump"))

(fconfig-search-bind
  "o" 'switch-to-or-occur
  "O" 'helm-occur
  "s" 'helm-ag
  "SPC" 'ace-jump-mode
  "C-x SPC" 'ace-jump-mode-pop-mark
  "w" 'ace-window)

;; Everything related to frame
(general-create-definer fconfig-frame-bind
  :prefix "C-c f"
  :name "Frame actions"
    "" '(:ignore t :which-key "Frame options"))

(fconfig-frame-bind
  "f" 'toggle-frame-fullscreen)

;; (when (featurep 'fconfig-mm)
;;   (fconfig-mm-bind
;;     "," #'spotify-previous
;;     "." #'spotify-next
;;     "<return>" #'spotify-playpause))

;;; setting this with general-def doesn't work!
(global-set-key "\C-l" 'backward-kill-line)
(general-def "C-x C-\\" 'save-buffers-kill-emacs)
(general-def "C-x C-c" 'kill-this-buffer)
(general-def "C-x C-b" 'ibuffer)
(general-def "<S-f3>" 'match-paren)

;; remap C-a to `smarter-move-beginning-of-line'
(general-def [remap move-beginning-of-line]
  'smarter-move-beginning-of-line)

;; buffer related global shortcuts
(general-def "<f11>" 'cycle-buffer-backward)
(general-def "<f12>" 'cycle-buffer)
(general-def "<shift f11>" 'cycle-buffer-backward-permissive)
(general-def "<shift f12>" 'cycle-buffer-permissive)

(general-def "M-o" 'ace-window)

;; type-break
(general-def "M-S-<f10>" 'type-break-statistics)

;; notmuch
(general-def "C-<f3>" 'notmuch)
(general-def "C-<f4>" 'helm-notmuch)

;;; treemacs
(general-def "C-c t t" 'treemacs)
(general-def "C-c t s" 'treemacs-select-window)
(general-def "C-c t e" 'lsp-treemacs-errors-list)
(general-def "C-c t y" 'lsp-treemacs-symbols-toggle)
(general-def "C-c t r" 'lsp-treemacs-references)
(general-def "C-c t i" 'lsp-treemacs-implementations)
(general-def "C-c t h c" 'lsp-treemacs-call-hierarchy)
(general-def "C-c t h t" 'lsp-treemacs-type-hierarchy)
