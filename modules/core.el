;;; backups
(setq
 backup-directory-alist '(("." . "~/.emacs.d/backups"))
 delete-old-versions -1
 version-control t
 vc-make-backup-files t
 auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp  'tooltip-mode) (tooltip-mode -1))
(setq inhibit-startup-screen t)

;;; specifically for cygwin and windows
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default buffer-file-coding-system 'utf-8-unix)

;;; General setup
(setq
 frame-title-format '("%b %* %m")
 delete-key-deletes-forward t
 mouse-yank-at-point t
 minibuffer-max-depth nil
 Man-swtiches "-a"
 scroll-conservatively 200			   ; Scrolling
 require-final-newline t	   ; file ends with new line?
 confirm-kill-emacs 'y-or-n-p
 completion-ignored-extensions '(".o" ".elc" )
 scroll-preserve-screen-position 1
 visible-bell t
 global-auto-revert-mode t
 scroll-preserve-screen-position 1 ; Pgup/dn will return exactly to the starting
                                   ; point.
 text-mode-hook 'turn-on-auto-fill ; Sets autofill on in text mode automatically
 ;; Some pretty stuff
 font-lock-maximum-decoration t
 inhibit-startup-message t
 query-replace-highlight t
 search-highlight t
 transient-mark-mode t
 initial-scratch-message nil
 shift-select-mode nil
 bookmark-save-flag 1
 history-delete-duplicates t)

;;; diary related
(setq
 calendar-view-diary-initially-flag t
 calendar-mark-diary-entries-flag t
 diary-number-of-entries 7
 holiday-christian-holidays nil
 holiday-bahai-holidays nil
 holiday-hebrew-holidays nil
 holiday-islamic-holidays nil
 holiday-general-holidays nil
 holiday-solar-holidays nil)

(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'list-diary-entries-hook 'sort-diary-entries t)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

(line-number-mode t)
(mouse-avoidance-mode 'jump)         ; Push the mouse out of the way
(display-time-mode)

(setq ispell-program-name "hunspell")
;; the default method for remote files fetching
(setq tramp-default-method "scp")

;;; Remember where we where in files between sessions
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")

(setq-default
 x-stretch-cursor t              ; when on a tab, make the cursor the tab length
 fill-column 80                  ; Nothing over 80 characters please
 indent-tabs-mode nil)

(add-hook 'post-command-hook 'set-cursor-with-file-mode)

;; Setup text mode
(add-hook 'text-mode-hook (lambda() (auto-fill-mode 1)))
(add-hook 'text-mode-hook (lambda() (setq fill-column 80)))

;; byte compile elisp files when killing files
;(add-hook 'kill-buffer-hook 'auto-byte-compile)

;; Some "nice to have" things
;; Hideshow/folding stuff
(defvar hs-special-modes-alist
  (mapcar 'purecopy
          '((c-mode "{" "}" "/[*/]" nil nil))))

(defadvice goto-line (after expand-after-goto-line
                            activate compile)
  "hideshow-expand affected block when using goto-line in a collapsed buffer"
  (save-excursion
    (hs-show-block)))

;;; set occur "grep" context to 2
(setq list-matching-lines-default-context-lines 2)

;; Some ediff settings
(setq ediff-split-window-function (if (> (frame-width) 150)
                                      'split-window-horizontally
                                    'split-window-vertically))

;; enable disabled commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
