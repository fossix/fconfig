(require 'view)
(require 'hide-mode-line)

(make-variable-buffer-local
 (defvar rm/read-frame nil))

(make-variable-buffer-local
 (defvar rm/line-spacing 0.50))

(make-variable-buffer-local
 (defvar rm/old-line-spacing))

(defun ss/center-window (frame)
  (let ((margin-size (/ (- (frame-width frame) 100) 2)))
    (set-window-margins (frame-root-window frame) 38)))

(defun enable-read-mode ()
  (interactive)
  (setq rm/read-frame (make-frame))
  (set-frame-parameter rm/read-frame 'fullscreen  'fullboth)
  (view-mode-enable)
  (variable-pitch-mode 1)
  (setq-local rm/old-line-spacing line-spacing)
  (setq-local line-spacing rm/line-spacing)
  (ss/center-window rm/read-frame)
  (turn-on-hide-mode-line-mode))

(defun disable-read-mode ()
  (turn-off-hide-mode-line-mode)
  (setq-local line-spacing rm/old-line-spacing)
  (variable-pitch-mode -1)
  (view-mode-disable)
  (delete-frame rm/read-frame))

(define-minor-mode read-mode
  "Minor mode to enhance reading experience"

  :init-value nil
  :lighter " reading"
  :keymap (let ((read-map (make-sparse-keymap)))
            read-map)

  :after-hook (define-key read-mode-map (kbd "q")
                (lambda () (interactive) (read-mode -1)))

  (if read-mode
      (enable-read-mode)
    (disable-read-mode)))

(provide 'read-mode)
