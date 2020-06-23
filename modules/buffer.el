;;; https://www.gnu.org/software/emacs/manual/html_node/elisp/The-Zen-of-Buffer-Display.html
(setq display-buffer-alist
      '(("\\*Help\\*"
         (display-buffer-reuse-window display-buffer-pop-up-frame)
         (reusable-frames . visible))
        (".*"
         (display-buffer-reuse-window display-buffer-pop-up-window)
         (reusable-frames . visible))
        ("\\*Org Agenda\\*"
         (inhibit-same-window . t)
         (reusable-frames . visible))))

(setq even-window-sizes nil)                 ; avoid resizing

;; Cyle Buffer - intelligent -- package unavailable in archives
(use-package cycle-buffer
  :defer 3
  :config
  (progn
    (autoload 'cycle-buffer "cycle-buffer" "Cycle forward." t)
    (autoload 'cycle-buffer-backward "cycle-buffer" "Cycle backward." t)
    (autoload 'cycle-buffer-permissive "cycle-buffer"
      "Cycle forward allowing *buffers*." t)
    (autoload 'cycle-buffer-backward-permissive "cycle-buffer"
      "Cycle backward allowing *buffers*." t)
    (autoload 'cycle-buffer-toggle-interesting "cycle-buffer"
      "Toggle if this buffer will be considered." t)))

(use-package fill-column-indicator
  :commands fci-mode)

(use-package writegood-mode
  :defer t)

;;; Whitespace mode setup
(use-package whitespace
  :diminish whitespace-mode
  :commands whitespace-mode
  :init
  (progn
    (setq whitespace-style '(face lines-tail trailing empty space-before-tab))))

;; key bindings
(general-create-definer fconfig-buffer-bind
  :name "Buffer related bindings"
  :prefix "C-c b"
  "" '(:ignore t :which-key "Buffer related"))
