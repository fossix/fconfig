(use-package mutt
  :defer t
  :disabled
  ;; Mutt email setup
  :init
  (setq mail-user-agent "mutt")
  (defvar default-mail-user-agent 'mutt
    "Default MUA to use. Can be mutt or vm.")
  (add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
  (add-to-list 'auto-mode-alist '(".*mutt.*" . message-mode))
  (setq mail-header-separator "")
  (or (assoc "mutt-" auto-mode-alist)
      (setq auto-mode-alist (cons '("mutt-" . mail-mode) auto-mode-alist)))
  :hook (mail-mode-hook . mail-mode-hook))


(use-package notmuch
  :config
  (setq notmuch-show-logo nil
        notmuch-column-control 1.0)

  ;; Key bindings
  (general-def notmuch-show-mode-map "C" 'fconfig/org-capture-email)
  (general-def notmuch-show-mode-map "\C-c\C-o" 'browse-url-at-point)

  (defun fconfig/org-capture-email ()
    (interactive)
    (org-capture nil "d"))

  (defun fconfig/notmuch-set-tags (tags)
    "Set tag for message"

    (pcase major-mode
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


  (defun fconfig/notmuch-get-tags ()
    "Set tag for the message thread "

    (pcase major-mode
      ('notmuch-show-mode (notmuch-show-get-tags))
      ('notmuch-search-mode (notmuch-search-get-tags))
      ('notmuch-tree-mode (notmuch-tree-get-tags))))

  (defun fconfig/notmuch-toggle-unread ()
    "toggle unread tag for thread"
    (interactive)
    (if (member "unread" (fconfig/notmuch-get-tags))
        (fconfig/notmuch-set-tags-thread '("-unread"))
      (fconfig/notmuch-set-tags-thread '("+unread"))))

  (defun fconfig/notmuch-toggle-flagged ()
    "Toggle needs action/flagged tag for thread"
    (interactive)
    (if (member "flagged" (fconfig/notmuch-get-tags))
        (fconfig/notmuch-set-tags-thread '("-flagged"))
      (fconfig/notmuch-set-tags-thread '("+flagged"))))

  (defun fconfig/notmuch-delete-thread ()
    "Toggle 'deleted' tag for a thread"
    (interactive)
    (fconfig/notmuch-toggle-tag "deleted" t))

  (defun fconfig/notmuch-delete-message ()
    "Toggle 'deleted tag for a message"
    (interactive)
    (fconfig/notmuch-toggle-tag "deleted"))

  (general-def notmuch-search-mode-map "#" 'fconfig/notmuch-toggle-unread)
  (general-def notmuch-search-mode-map "!" 'fconfig/notmuch-toggle-flagged)

  (general-def notmuch-tree-mode-map "d" 'fconfig/notmuch-delete-message)
  (general-def notmuch-search-mode-map "d" 'fconfig/notmuch-delete-thread)
  (general-def notmuch-show-mode-map "D" 'fconfig/notmuch-delete-thread)
  (general-def notmuch-tree-mode-map "D" 'fconfig/notmuch-delete-thread)

  ;; In the search mode
  (general-def notmuch-search-mode-map "<up>" 'previous-line)
  (general-def notmuch-search-mode-map "<down>" 'next-line)
  (general-def notmuch-search-mode-map "<tab>" 'notmuch-tree-from-search-thread)
  (general-def notmuch-search-mode-map "<C-tab>" 'notmuch-tree-from-search-current-query)

  ;; threaded view
  (general-def notmuch-tree-mode-map "<up>" 'previous-line)
  (general-def notmuch-tree-mode-map "<down>" 'next-line)

  ;; mail view
  (general-def notmuch-show-mode-map "<up>" 'previous-line)
  (general-def notmuch-show-mode-map "<down>" 'next-line)
  (general-def notmuch-show-mode-map "<right>" 'forward-char)
  (general-def notmuch-show-mode-map "<left>" 'backward-char)

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

  (setq notmuch-mua-compose-in 'new-frame)
  (general-def notmuch-show-mode-map "x" #'my-notmuch-export-patch)
  (general-def notmuch-tree-mode-map "x" #'my-notmuch-export-patch)

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
  (define-key 'notmuch-show-part-map "v" 'fconfig/notmuch-show-view-as-patch)

  ;; faces, this should go into the theme
  (setq notmuch-search-line-faces
        '(("unread" . notmuch-search-unread-face)
          ("flagged" . notmuch-search-flagged-face)
          ("deleted" . notmuch-tag-deleted))))

(use-package helm-notmuch
  :commands helm-notmuch)

;;; from http://www.coli.uni-saarland.de/~slemaguer/emacs/main.html
(use-package notmuch-hello
  :commands (notmuch notmuch-hello)
  :config

  (setq notmuch-hello-thousands-separator "," ;; Add a thousand separator
        notmuch-column-control 1.0)           ;; don't display columns
  (general-def notmuch-hello-mode-map "h" 'helm-notmuch)

  ;; Saved searches
  (setq notmuch-saved-searches
        '(
          ;; Quick helpers
          (:key "u" :name "Unread mails to me" :query "tag:me and tag:unread")
          (:key "c" :name "Mails from lists (to/cc me)" :query "tag:list and tag:me")
          (:key "t" :name "Today's stuff" :query "date:today and not tag:deleted")
          (:key "a" :name "Mails that need action" :query "tag:flagged")
          (:key "b" :name "Bugs" :query "tag:bug and not tag:mybug")
          (:key "m" :name "Bugs assigned to me" :query "tag:mybug")
          (:key "D" :name "Deleted" :query "tag:deleted")
          (:key "T" :name "To lists" :query "tag:sent and tag:list")
          (:key "i" :name "Unread inbox" :query "tag:inbox and tag:unread")
          (:key "r" :name "Reddit" :query "tag:reddit")

          ;; Classic box
          (:key "s" :name "sent" :query "tag:sent")
          (:key "d" :name "drafts" :query "tag:draft")

          ;; Folder
          (:key "f" :name "fossix inbox" :query "folder:fossix/INBOX and tag:inbox")
          (:key "l" :name "ltc inbox" :query "(folder:ltc/INBOX or folder:ltc/ibmnotes)")

          ;; mailing lists
          (:key "L" :name "lkml" :query "tag:lkml and not (tag:linuxmm or tag:nvdimm or tag:linuxmm or tag:lppc)")
          (:key "M" :name "linux-mm" :query "tag:linuxmm and not tag:nvdimm and not tag:lppc")
          (:key "p" :name "lppc" :query "tag:lppc")
          (:key "n" :name "nvdimm" :query "tag:nvdimm and not tag:lppc")

          ;; custom queries
          (:key "x" :name "ndctl" :query "tag:nvdimm and subject:ndctl")
          ))

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
               (oldest-first (case (plist-get elem :sort-order)
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
        (loop for i from 1 to notmuch-hello-recent-searches-max
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


  ;; We add items later in reverse order with (add-to-list ...):
  (setq notmuch-hello-sections '())
  (add-to-list 'notmuch-hello-sections 'my-notmuch-hello-insert-recent-searches)
  (add-to-list 'notmuch-hello-sections 'notmuch-hello-insert-search)
  (add-to-list 'notmuch-hello-sections 'my-notmuch-hello-insert-searches)

  ;;(add-to-list 'notmuch-hello-sections 'my-notmuch-hello-insert-header)

  ;; this is the end of use-package notmuch:
  (add-hook 'notmuch-hello-refresh-hook
            (lambda ()
              (whitespace-mode -1)))

  ;; for mails
  (setq sendmail-program "~/bin/msmtpQ"
        message-sendmail-f-is-evil nil
        message-interactive t
        message-send-mail-function 'message-send-mail-with-sendmail
        notmuch-fcc-dirs nil
        mail-envelope-from 'header
        message-sendmail-envelope-from 'header
        message-signature nil
        message-mail-alias-type 'ecomplete))

(autoload 'mail-hist-forward-header "mail-hist")
(autoload 'mail-text-start          "sendmail")
