;;; -*- mode:emacs-lisp -*-

;; Toggling fullscreen -from emacswiki
(defvar my-fullscreen-p t "Check if fullscreen is on or off")

(defun my-non-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
	  ;; WM_SYSCOMMAND restore #xf120
	  (w32-send-sys-command 61728)
	(progn (set-frame-parameter nil 'width 82)
		   (set-frame-parameter nil 'fullscreen 'fullheight))))

(defun my-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
	  ;; WM_SYSCOMMAND maximaze #xf030
	  (w32-send-sys-command 61488)
	(set-frame-parameter nil 'fullscreen 'fullboth)))

(defun my-toggle-fullscreen ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
	  (my-non-fullscreen)
	(my-fullscreen)))

(defun insert-time()
  (interactive)
  (insert (format-time-string "%a, %d %b %Y at %T")))

(defun insert-date ()
  (interactive)
  (insert (format-time-string "%d/%m/%Y")))

;; Setting cross compile environment
(defvar cross-compilation-targets '(("powerpc" "powerpc64-linux-gnu-")
                                    ("arm" "arm-none-linux-gnueabi-")
                                    ("go" nil (("GOPATH" . "~/dev/gws") ("PATH" . ~/dev/gws/bin)))))
(defun cc-environ ()
  "Set the environment for cross compilation"
  (interactive)
  (let* ((arch (completing-read "Select from the list: "
                               cross-compilation-targets nil t))
        (prefix (car (cdr (assoc arch cross-compilation-targets))))
        (env (cdr (cdr (assoc arch cross-compilation-targets)))))
    (cc-setenv arch prefix)))

(defun cc-setenv (arch prefix &optional env)
  (setenv "ARCH" arch)
  (if prefix
      (setenv "CROSS_COMPILE" prefix))
  (loop for (key . value) in (car (cdr (cdr (assoc "go" cross-compilation-targets))))
        do (if (string= key "PATH")
               (setenv "PATH" (concat (getenv "PATH") (format ":%s" value)))
             (setenv key (format "%s" value))))

  (message "Cross compile enviroment set."))

;; Auto compile init and .el files
(defun auto-byte-compile ()
  "Auto compile the any file which is in emacs-lisp mode"
  (if (not (and buffer-file-name
                (file-exists-p buffer-file-name)
                (file-writable-p buffer-file-name)))
      nil
    (progn
      (when (or (string= "el" (file-name-extension buffer-file-name))
                (string= user-init-file buffer-file-name)
                ;; if init file is .emacs, and byte-compiled then the
                ;; user-init-file will be .emacs.el, so we check without
                ;; extension
                (string= (file-name-sans-extension user-init-file)
                         buffer-file-name)
                (string= major-mode "emacs-lisp-mode"))
        (message buffer-file-name)
        (when (file-newer-than-file-p buffer-file-name
                                      (concat
                                       (file-name-sans-extension
                                        buffer-file-name) ".elc"))
          (byte-compile-file buffer-file-name))))))

;; for mutt
(declare-function mail-text nil)        ; suppress compiler warning
(defun my-mail-mode-hook ()
  (turn-on-auto-fill)
  (turn-on-font-lock)
  (abbrev-mode 1)
  (local-set-key "\C-xk" 'server-edit)
  (flush-lines "^\\(> \n\\)*> -- \n\\(\n?> .*\\)*")
                      ;;; Kills quoted sigs.
  (mail-text)         ;;; Jumps to the beginning of the mail text
  (setq make-backup-files nil))

;; change cursor color according to the file mode
(defun set-cursor-with-file-mode ()
  "change cursor color and type according to file mode."
  (cond
    (buffer-read-only
     (setq cursor-type 'bar)
     (set-cursor-color "gray"))
    (overwrite-mode
     (set-cursor-color "red")
     (setq cursor-type 'hbar))
    (t
     (set-cursor-color "gray")
     (setq cursor-type 'box))))

;; Replace "yes or no" with y or n
(defun yes-or-no-p (arg)
  "An alias for y-or-n-p, because I hate having to type yes or no."
  (y-or-n-p arg))

(defun backward-kill-line (arg)
  "Kill chars backward until encountering the end of a line."
  (interactive "p")
  (kill-line 0))

(defadvice show-paren-function
  (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (interactive)
  (if (not (minibuffer-prompt))
      (let ((matching-text nil))
        ;; Only call `blink-matching-open' if the character before point
        ;; is a close parentheses type character. Otherwise, there's not
        ;; really any point, and `blink-matching-open' would just echo
        ;; "Mismatched parentheses", which gets really annoying.
        (if (not (equal (char-before (point)) nil))
            (if (char-equal (char-syntax (char-before (point))) ?\))
                (setq matching-text (blink-matching-open)))
          (if (not (null matching-text))
              (message matching-text))))))

;; matching brace code
(defun match-paren (arg)
  "Jump to the matching parenthesis"
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (message "No match"))))

(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

(defun read-mail-with-mutt ()
  "Run mutt in gnome-terminal."
  (interactive)
  (call-process "gnome-terminal" nil 0 nil "-e" "neomutt"))

(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished, close the *compilation* buffer if
   the compilation is successful, and set the focus back to Emacs frame"
  (if (string-match "^finished" msg)
      (progn
        (delete-windows-on buffer)
        (tooltip-show "Compilation Successful :-)"))
    (tooltip-show "Compilation Failed :-("))
  (let (current-frame (car (car (cdr (current-frame-configuration)))))
    (select-frame-set-input-focus current-frame)))

;; eshell - from emacswiki
(defun eshell/emacs (&rest args)
  "Open a file in emacs. Some habits die hard."
  (if (null args)
      ;; If I just ran "emacs", I probably expect to be launching
      ;; Emacs, which is rather silly since I'm already in Emacs.
      ;; So just pretend to do what I ask.
      (bury-buffer)
    ;; We have to expand the file names or else naming a directory in an
    ;; argument causes later arguments to be looked for in that directory,
    ;; not the starting directory
    (mapc #'find-file (mapcar #'expand-file-name (flatten-tree
                                                  (reverse args))))))

;;; from emacswiki :: https://www.emacswiki.org/emacs/KeyboardMacrosTricks
(defun save-macro (name)
  "save a macro. Take a name as argument and save the last
   defined macro under this name at the end of your .emacs"
  (interactive "Name of the macro :")   ; ask for the name of the macro
  (kmacro-name-last-macro name)         ; use this name for the macro
  (find-file user-init-file)            ; open ~/.emacs or other user init file
  (goto-char (point-max))               ; go to the end of the .emacs
  (newline)                             ; insert a newline
  (insert-kbd-macro name)               ; copy the macro
  (newline)                             ; insert a newline
  (switch-to-buffer nil))               ; return to the initial buffer

;;; occur customization
(defun occur-all ()
  "Switch to occur buffer if present or run occur in all
   the open buffers."
  (interactive)
  (if (get-buffer "*Occur*")
      (switch-to-buffer-other-window "*Occur*")
    (let (pattern (read-from-minibuffer "Regular Expression: "))
         (multi-occur-in-matching-buffers "" pattern t))))

(defun switch-to-or-occur ()
  "Switch to occur buffer, or run `occur'."
  (interactive)
  (if (get-buffer "*Occur*")
      (switch-to-buffer-other-window "*Occur*")
    (call-interactively 'occur)))

(defun add-all-to-list (list items)
  "Add multiple items to a list."
  (dolist (item items)
    (add-to-list list item)))

;;; from: http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; Use variable width font face
(defun buffer-face-mode-variable ()
  "Set font to proportional width font in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Go Regular" :height 100))
  (buffer-face-mode))

;; Use fixed font faces
(defun buffer-face-mode-fixed ()
  "Set fixed width font in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Monaco" :height 110))
  (buffer-face-mode))

(defun apply-frame-font ()
  (when fconfig-default-font
    (set-frame-font fconfig-default-font nil t))
  (setq-default line-spacing 0.25))

;;; What should be done after frame creation?
(defun create-frame-hook (&optional frame)
  (when fconfig-default-theme
    (load-theme fconfig-default-theme t))
  (apply-frame-font))

(defun read-mode (width)
  (interactive "nBuffer width: ")
  (let* ((adj (- (window-text-width)
                 width))
         (total-margin (+ adj
                          left-margin-width
                          right-margin-width)))
    (setq left-margin-width  (/ total-margin 2))
    (setq right-margin-width (- total-margin left-margin-width)))
  (set-window-buffer (selected-window) (current-buffer))
  (view-mode)
  (text-scale-increase 1)
  (toggle-frame-fullscreen))

;;; flash the modeline
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.2 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))


(defun configure-prettify-symbols-alist ()
  "Set prettify symbols alist."
  (progn
    (setq prettify-symbols-alist '(("lambda" . ?λ)
                                   ("->" . ?→)
                                   ("->>" . ?↠)
                                   ("=>" . ?⇒)
                                   ("map" . ?↦)
                                   ("/=" . ?≠)
                                   ("!=" . ?≠)
                                   ("==" . ?≡)
                                   ("<=" . ?≤)
                                   (">=" . ?≥)
                                   ("<=<" . ?↢)
                                   (">=>" . ?↣)
                                   ("&&" . ?∧)
                                   ("||" . ?∨)
                                   ("not" . ?¬)))
    (prettify-symbols-mode)))
