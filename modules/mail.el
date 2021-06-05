(defun fconfig/org-capture-email ()
  (interactive)
  (org-capture nil "d"))

(defun fconfig/notmuch-get-tags ()
  "Set tag for the message thread "

  (pcase major-mode
    ('notmuch-show-mode (notmuch-show-get-tags))
    ('notmuch-search-mode (notmuch-search-get-tags))
    ('notmuch-tree-mode (notmuch-tree-get-tags))))

(defun fconfig/notmuch-set-tags (tags)
  "Set tag for message"

  (pcase major-mode
    ('notmuch-search-mode (notmuch-search-tag tags))
    ('notmuch-show-mode (notmuch-show-tag tags))
    ('notmuch-tree-mode (notmuch-tree-tag tags))))

(defun fconfig/notmuch-set-tags-thread (tags)
  "Set tag for the message thread "

  (pcase major-mode
    ('notmuch-show-mode (notmuch-show-tag-all tags))
    ('notmuch-search-mode (notmuch-search-tag tags))
    ('notmuch-tree-mode (notmuch-tree-tag-thread tags))))

(defun fconfig/notmuch-toggle-tag (tag &optional threadp)
  "Toggle tag. Will strip '-' and '+' from the start and end of
the tag string."
  (when (not (stringp tag))
    (error "%s is not a string" tag))

  (setq tag (string-trim tag "[ \t\n\r+-]+" "[ \t\n\r+-]+"))
  (if (member tag (fconfig/notmuch-get-tags))
      (setq tag (concat "-" tag))
    (setq tag (concat "+" tag)))
  (if threadp
      (fconfig/notmuch-set-tags-thread (list tag))
    (fconfig/notmuch-set-tags (list tag))))

(defun fconfig/notmuch-toggle-unread ()
  "toggle unread tag for thread"
  (interactive)
  (fconfig/notmuch-toggle-tag "unread"))

(defun fconfig/notmuch-toggle-flagged ()
  "Toggle needs action/flagged tag for thread"
  (interactive)
  (fconfig/notmuch-toggle-tag "flagged"))

;; both the delete functions will remove the unread tag, even if we are
;; undeleting a message. I think that's ok for my use case. In rare cases,
;; when I undelete and mark as unread, I can toggle unread too, just one more
;; key stroke
(defun fconfig/notmuch-delete-thread ()
  "Toggle 'deleted' tag for a thread"
  (interactive)
  (fconfig/notmuch-set-tags-thread (list "-unread"))
  (fconfig/notmuch-toggle-tag "deleted" t))

(defun fconfig/notmuch-delete-message ()
  "Toggle 'deleted tag for a message"
  (interactive)
  (fconfig/notmuch-set-tags (list "-unread"))
  (fconfig/notmuch-toggle-tag "deleted"))

(defun fconfig/notmuch-show-view-as-patch ()
  "View the the current message as a patch."
  (interactive)
  (let* ((id (notmuch-show-get-message-id))
         (msg (notmuch-show-get-message-properties))
         (part (notmuch-show-get-part-properties))
         (subject (concat "Subject: " (notmuch-show-get-subject) "\n"))
         (diff-default-read-only t)
         (buf (get-buffer-create (concat "*notmuch-patch-" id "*")))
         (map (make-sparse-keymap)))
    (define-key map "q" 'notmuch-bury-or-kill-this-buffer)
    (switch-to-buffer buf)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert subject)
      (insert (notmuch-get-bodypart-text msg part nil)))
    (set-buffer-modified-p nil)
    (diff-mode)
    (lexical-let ((new-ro-bind (cons 'buffer-read-only map)))
      (add-to-list 'minor-mode-overriding-map-alist new-ro-bind))
    (goto-char (point-min))))

(defun fconfig/notmuch-bounce-message (&optional address)
  "Bounce the current message."
  (interactive "sBounce To: ")
  (notmuch-show-view-raw-message)
  (message-resend address))

;; extract patch from mail
;; from: http://www.holgerschurig.de/en/emacs-notmuch-export-patch/
(defun my-notmuch-export-patch ()
  (interactive)
  (let* ((from (notmuch-show-get-from))
         (date (notmuch-show-get-date))
         (subject (notmuch-show-get-subject))
         (id (notmuch-show-get-message-id))
         (filename subject)
         (patchnum))
    (when (string-match "\\[PATCH.+?0*\\([0-9]+\\)/[0-9]+\\]" filename)
      (setq patchnum (string-to-number (match-string 1 filename))))
    (setq filename (replace-regexp-in-string "\\[PATCH.*\\]" "" filename))
    (setq filename (replace-regexp-in-string "\[^a-zA-Z0-9]" "-" filename))
    (setq filename (replace-regexp-in-string "\\-+" "-" filename))
    (setq filename (replace-regexp-in-string "^-" "" filename))
    (setq filename (replace-regexp-in-string "-$" "" filename))

    (when patchnum
      (setq filename (concat (format "%04d" patchnum) "-" filename)))

    (setq filename (concat "/tmp/" filename ".patch"))

    (save-excursion
      (let ((buf (generate-new-buffer (concat "*notmuch-export-patch-" id "*"))))
        (with-current-buffer buf
          (insert (format "Subject: %s\n" subject))
          (insert (format "From: %s\n" from))
          (insert (format "Date: %s\n" date))
          (insert (format "Message-Id: %s\n\n" (substring id 3)))
          (let ((coding-system-for-read 'no-conversion))
            (call-process notmuch-command nil t nil "show" "--part:1" id))
          (write-file filename))
        (kill-buffer buf)))))



;; Utilities: Most copied/based on snippets from
;; 1.https://notmuchmail.org/emacstips

(defun fconfig/notmuch-show-view-as-patch ()
  "View the the current message as a patch."
  (interactive)
  (let* ((id (notmuch-show-get-message-id))
         (msg (notmuch-show-get-message-properties))
         (part (notmuch-show-get-part-properties))
         (subject (concat "Subject: " (notmuch-show-get-subject) "\n"))
         (diff-default-read-only t)
         (buf (get-buffer-create (concat "*notmuch-patch-" id "*")))
         (map (make-sparse-keymap)))
    (define-key map "q" 'notmuch-bury-or-kill-this-buffer)
    (switch-to-buffer buf)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert subject)
      (insert (notmuch-get-bodypart-text msg part nil)))
    (set-buffer-modified-p nil)
    (diff-mode)
    (lexical-let ((new-ro-bind (cons 'buffer-read-only map)))
      (add-to-list 'minor-mode-overriding-map-alist new-ro-bind))
    (goto-char (point-min))))

(defun my-count-query (query)
  (with-temp-buffer
    (insert query "\n")
    (unless (= (call-process-region (point-min) (point-max) notmuch-command
                                    t t nil "count" "--batch") 0)
      (notmuch-logged-error "notmuch count --batch failed"
                            "Please check that the notmuch CLI is new enough to support `count
--batch'. In general we recommend running matching versions of
the CLI and emacs interface."))

    (goto-char (point-min))
    (let ((n (read (current-buffer))))
      (if (= n 0)
          nil
        (notmuch-hello-nice-number n)))))

(defun my-notmuch-hello-query-insert (cnt query elem)
  (if cnt
      (let* ((str (format "%s" cnt))
             (widget-push-button-prefix "")
             (widget-push-button-suffix "")
             (oldest-first (cl-case (plist-get elem :sort-order)
                             (newest-first nil)
                             (oldest-first t)
                             (otherwise notmuch-search-oldest-first))))
        (widget-create 'push-button
                       :notify #'notmuch-hello-widget-search
                       :notmuch-search-terms query
                       :notmuch-search-oldest-first oldest-first
                       :notmuch-search-type 'tree
                       str)
        (widget-insert (make-string (- 8 (length str)) ? )))
    (widget-insert "        ")))


(defun my-notmuch-hello-insert-searches ()
  "Insert the saved-searches section."
  (widget-insert (propertize "New     Total      Key  List\n" 'face 'my-notmuch-hello-header-face))
  (mapc (lambda (elem)
          (when elem
            (let* ((q_tot (plist-get elem :query))
                   (q_new (concat q_tot " AND tag:unread"))
                   (n_tot (my-count-query q_tot))
                   (n_new (my-count-query q_new)))
              (my-notmuch-hello-query-insert n_new q_new elem)
              (my-notmuch-hello-query-insert n_tot q_tot elem)
              (widget-insert "   ")
              (widget-insert (plist-get elem :key))
              (widget-insert "    ")
              (widget-insert (plist-get elem :name))
              (widget-insert "\n"))))

        notmuch-saved-searches))

(defun my-notmuch-hello-insert-recent-searches ()
  "Insert recent searches."
  (when notmuch-search-history
    (widget-insert "Recent searches:")
    (widget-insert "\n\n")
    (let ((start (point)))
      (cl-loop for i from 1 to notmuch-hello-recent-searches-max
               for search in notmuch-search-history do
               (let ((widget-symbol (intern (format "notmuch-hello-search-%d" i))))
                 (set widget-symbol
                      (widget-create 'editable-field
                                     ;; Don't let the search boxes be
                                     ;; less than 8 characters wide.
                                     :size (max 8
                                                (- (window-width)
                                                   ;; Leave some space
                                                   ;; at the start and
                                                   ;; end of the
                                                   ;; boxes.
                                                   (* 2 notmuch-hello-indent)
                                                   ;; 1 for the space
                                                   ;; before the `[del]'
                                                   ;; button. 5 for the
                                                   ;; `[del]' button.
                                                   1 5))
                                     :action (lambda (widget &rest ignore)
                                               (notmuch-hello-search (widget-value widget)))
                                     search))
                 (widget-insert " ")
                 (widget-create 'push-button
                                :notify (lambda (widget &rest ignore)
                                          (when (y-or-n-p "Are you sure you want to delete this search? ")
                                            (notmuch-hello-delete-search-from-history widget)))
                                :notmuch-saved-search-widget widget-symbol
                                "del"))
               (widget-insert "\n"))
      (indent-rigidly start (point) notmuch-hello-indent))
    nil))

(defface my-notmuch-hello-header-face
  '((t :foreground "white"
       :background "#0088FC"
       :weight bold))
  "Font for the header in `my-notmuch-hello-insert-searches`."
  :group 'notmuch-faces)


(defun my-notmuch-hello-insert-separator ()
  (insert "\n\f\n"))
