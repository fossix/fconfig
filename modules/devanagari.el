;;; Some important Devanagari key bindings, or we will have to switch to English
;;; to perform any operation every time. (Because I don't use input-method, I
;;; switch to different keyboard layout, which is simpler than different
;;; input-methods of devanagari.
(global-map! (kbd "C-स") 'isearch-forward)
(global-map! (kbd "C-ष C-स") 'save-buffer)
(global-map! (kbd "C-ष C-ॆ") 'eval-last-sexp)
(global-map! (kbd "C-ष C-्") 'find-file)
(global-map! (kbd "C-ष C-च") 'kill-this-buffer)
(global-map! (kbd "C-ॆ") 'move-end-of-line)
(global-map! (kbd "C-ा") 'move-beginning-of-line)
(global-map! (kbd "C-न") 'next-line)
(global-map! (kbd "C-प") 'previous-line)
(global-map! (kbd "M-द") 'kill-word)
(global-map! (kbd "M-ड") 'kill-ring-save)
(global-map! (kbd "C-य") 'icicle-yank-maybe-completing)
(global-map! (kbd "C-क") 'kill-line)
(global-map! (kbd "M-ट") 'fill-paragraph)
(global-map! (kbd "C-ब") 'backward-char)
(global-map! (kbd "C-्") 'forward-char)
(global-map! (kbd "M-ब") 'backward-word)
(global-map! (kbd "M-्") 'forward-word)
