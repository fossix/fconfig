(use-package helm
  :defer 1
  :diminish helm-mode
  :config
  (progn
    (require 'helm-config)
    (setq helm-idle-delay 0.0
          helm-input-idle-delay 0.01
          helm-candidate-number-limit 100
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t
          helm-autoresize-max-height 0
          helm-split-window-inside-p t
          helm-move-to-line-cycle-in-source t
          helm-scroll-amount 8
          helm-ff-file-name-history-use-recentf t
          helm-autoresize-min-height 20
          helm-mode-line-string nil
          helm-candidate-number-limit 50
          helm-display-header-line nil
          helm-mode-line-string nil
          helm-ff-auto-update-initial-value nil
          helm-find-files-doc-header nil
          ;; Default helm window sizes
          helm-display-buffer-default-width nil
          helm-display-buffer-default-height 0.25
          ;; When calling `helm-semantic-or-imenu', don't immediately jump to
          ;; symbol at point
          helm-imenu-execute-action-at-once-if-one nil
          ;; disable special behavior for left/right, M-left/right keys.
          helm-ff-lynx-style-map nil
          helm-ff-kill-or-find-buffer-fname-fn 'ignore)

    ;; The default "C-x c" is quite close to "C-x C-c", which closes a file (in my
    ;; configuration).  Change to "C-c h". Note: We must set "C-c h" globally,
    ;; because we cannot change `helm-command-prefix-key' once `helm-config' is
    ;; loaded.
    (general-define-key (kbd "C-c h") 'helm-command-prefix)
    (general-unbind "C-x c")

    (add-hook 'helm-minibuffer-set-up-hook
              #'helm-hide-minibuffer-maybe)

    ;; Don't use helm's own displaying mode line function
    (fset 'helm-display-mode-line #'ignore)


    (add-hook 'helm-after-initialize-hook
              (defun hide-mode-line-in-helm-buffer ()
                "Hide mode line in `helm-buffer'."
                (with-helm-buffer
                  (setq-local mode-line-format nil))))

    (when (executable-find "curl")
      (setq helm-net-prefer-curl t))
    (helm-mode)
    (general-def helm-map "<tab>" 'helm-execute-persistent-action)
    (helm-autoresize-mode 1)))

;; key bindings
(general-create-definer fconfig-helm-bind
  :prefix "C-c h"
  :keymap helm-command-prefix
  "" '(:ignore t :which-key "Helm"))

(general-def "M-x" 'helm-M-x)
(general-def "C-x b" 'helm-buffers-list)
(general-def "M-y" 'helm-show-kill-ring)
(general-def "C-x C-f" 'helm-find-files)

(fconfig-helm-bind "SPC" 'helm-all-mark-rings)
