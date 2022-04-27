;;; Org-mode setup
;;; Some (most clock related) settings taken from doc.norang.ca/org-mode.html

(require 'notifications)

(setq
 org-hide-leading-stars t
 org-clock-persist 'history
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-agenda-skip-timestamp-if-done t
 org-clock-idle-time 15
 org-deadline-warning-days 7
 org-agenda-skip-scheduled-if-deadline-is-shown t
 org-return-follows-link t
 org-enforce-todo-dependencies t
 org-agenda-dim-blocked-tasks t
 org-habit-preceding-days 7
 org-habit-following-days 1
 org-habit-show-done-always-green t
 org-habit-show-habits-only-for-today nil
 org-habit-graph-column 75
 org-agenda-start-on-weekday 1
 org-agenda-todo-ignore-deadlines t
 org-agenda-include-diary t
 org-insert-mode-line-in-empty-file t
 org-use-speed-commands t
 org-clock-out-remove-zero-time-clocks t
 org-clock-out-when-done t
 org-clock-persist-query-resume nil
 org-clock-auto-clock-resolution (quote when-no-clock-is-running)
 org-clock-report-include-clocking-task t
 org-clock-history-length 20
 org-drawers (quote ("PROPERTIES" "LOGBOOK"))
 org-clock-into-drawer t
 org-log-into-drawer t
 org-log-state-notes-insert-after-drawers t
 org-export-with-sub-superscripts "{}"
 org-catch-invisible-edits t
 org-outline-path-complete-in-steps nil
 org-refile-use-outline-path t
 org-log-note-clock-out nil
 org-password-file "~/.passwds/credentials.gpg"
 org-agenda-show-future-repeats 'next
 org-agenda-sorting-strategy '((agenda habit-down time-up priority-down
                                       effort-up category-up)
                               (todo priority-down)
                               (tags priority-down))

 ;; a 8 hour, 5 day work week
 org-duration-units
 `(("min" . 1)
   ("h" . 60)
   ("d" . ,(* 60 8))
   ("w" . ,(* 60 8 5))
   ("m" . ,(* 60 8 5 4))
   ("y" . ,(* 60 8 5 4 10)))

 ;; Any single task cannot be spanning for weeks, otherwise it becomes a habit
 ;; task. So keeping the maximum effort for a task to 1 week, for the worst
 ;; case. Ordinarily a task (subtask mostly), shouldn't be spanning more than
 ;; a day. So the maximum subtask effort is set to 7 hours.
 org-global-properties (quote
                        (("Effort_ALL" . "0:15 0:20 0:30 0:40 1:00 1:40 2:00 4:00 7:00 1d 2d 3d 4d 1w")))

 org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM"
 keep-clock-running nil
 org-todo-keywords
 '((sequence "TODO(t)" "MAYBE(m)" "NEXT(n)" "STARTED(s!)" "WAITING(w@/!)""|"
             "DONE(d!)")
   (sequence "ASSIGNED(a)" "INPROGRESS(p!)" "MOVED(o@/!)" "NEEDINFO(n@/!)" "|"
             "CLOSED(c@/!)" )
   (sequence "|" "CANCELLED(l@/!)"))

 ;; Some calendar holiday tweaks
 holiday-general-holidays nil
 holiday-christian-holidays nil
 holiday-islamic-holidays nil
 holiday-hebrew-holidays nil
 holiday-bahai-holidays nil
 holiday-oriental-holidays nil
 holiday-solar-holidays nil

 org-latex-pdf-process
 '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")

 org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

(set-face-attribute 'org-headline-done nil :strike-through t)

(org-toggle-pretty-entities)
(configure-prettify-symbols-alist)
(org-toggle-inline-images)
(setq org-latex-preview t)

(add-hook 'before-save-hook 'org-update-all-dblocks)

(setf org-special-ctrl-a/e t)
(setf org-special-ctrl-k t)
(org-clock-persistence-insinuate)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(defun org-insert-clock-range (&optional n)
  (interactive "NTime Offset (in min): ")
  (let* ((ctime (cdr (decode-time (current-time))))
         (min (car ctime))
         (start (apply 'encode-time 0 (- min n) (cdr ctime))))
    (org-insert-time-stamp start t t "CLOCK: ")
    (insert "--")
    (org-insert-time-stamp (current-time) t t)))

  ;; from: https://lists.gnu.org/archive/html/emacs-orgmode/2014-01/msg00637.html

(setq org-agenda-tag-line-face
      '(("meeting" . (:foreground "DeepSkyBlue" :weight bold))
        ("bug" . (:foreground "tan"))))

(defun org-agenda-fontify-tagged-line ()
  "Use `org-agenda-face-for-tagged-lines' to fontify lines with certain tags."
  (goto-char (point-min))
  (let (tags)
    (while (progn (forward-line 1) (not (eobp)))
      (if (setq tags (get-text-property (point) 'tags))
          (mapc
           (lambda (pair)
             (if (member (car pair) tags)
                 (add-text-properties (point-at-bol) (point-at-eol) `(face ,(cdr pair)))))
           org-agenda-tag-line-face)))))

(add-hook 'org-agenda-finalize-hook 'org-agenda-fontify-tagged-line)

(defun agenda-finalize-misc ()
  (goto-char (point-min)))

(add-hook 'org-agenda-finalize-hook 'agenda-finalize-misc)

(eval-after-load 'org-src
  '(define-key org-src-mode-map
               (kbd "C-x C-s") #'org-edit-src-exit))

  ;; Some hooks

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

                                        ; Some helpers

(defun is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun clock-in-to-next (kw)
  "Switch a task from TODO to STARTED when clocking in.
   Skips capture tasks, projects, and subprojects.
   Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and  (member (org-get-todo-state) (list "TODO" "WAITING"))
            (not (is-project-p)))
      "STARTED"))))

(setq org-clock-in-switch-to-state 'clock-in-to-next)

(defvar org-state)
(defvar org-last-state)

(defun org-clock-in-if-starting ()
  "Clock in when the task is marked STARTED."
  (when (and (string= org-state "STARTED")
             (not (string= org-last-state org-state)))
    (org-clock-in)))

(defun org-clock-out-if-waiting ()
  "Clock out when the task is marked WAITING."
  (when (and (string= org-state "WAITING")
             (not (string= org-last-state org-state)))
    (org-clock-out)))

                                        ; (add-hook 'org-after-todo-state-change-hook 'org-clock-in-if-starting)

(add-hook 'org-after-todo-state-change-hook 'org-clock-out-if-waiting)

(defun find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
                                        ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (clock-in-organization-task-as-default)))))

(defun punch-out ()
  (interactive)
  (setq keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when keep-clock-running
            (clock-in-default-task)))))))

(defvar organization-task-id "d0816446-e80d-4ad4-9e90-b518d3a9c121")

(defun clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find organization-task-id 'marker)
    (org-clock-in '(16))))

(defun clock-out-maybe ()
  (when (and keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'clock-out-maybe 'append)

(defun clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(defun org-add-note-outside-drawer (func &rest args)
  (interactive "P")
  (let ((org-log-into-drawer nil))
    (funcall func)))

(advice-add 'org-add-note :around #'org-add-note-outside-drawer)

  ;; colors, lots of colors

(setf org-todo-keyword-faces
      '(("TODO" . (:foreground "LightSkyBlue" :weight bold))
        ("MAYBE" . (:foreground "DarkSlateGray" :weight bold))
        ("STARTED" . (:foreground "royalblue" :weight bold))
        ("DONE" . (:foreground "MediumSeaGreen" :weight bold))
        ("WAITING" . (:foreground "darkgray" :weight bold))))

(setq org-priority-faces '((?A . (:foreground "#ee4e4e" :bold t :weight bold))
                           (?B . (:foreground "#9f7f7f"))
                           (?C . (:foreground "#717171"))))

  ;; Remove empty LOGBOOK drawers on clock out

(defun remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

(add-hook 'org-clock-out-hook 'remove-empty-drawer-on-clock-out 'append)

(setq org-archive-location "archive/%s::")

(setq org-stuck-projects
      '("+project|reading|learn/WAITING-CANCELLED-DONE-CLOSED-FIXED-STARTED" ()
        () "\\<IGNORE\\>"))

(add-to-list 'org-src-lang-modes
             (quote ("dot" . graphviz-dot)))

(use-package ox-md)

  ;; colour source code listings

(use-package ox-latex
  :init
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  :custom
  (org-latex-listings 'minted))

(use-package org-tempo)
(use-package ol-notmuch)
(use-package ol-git-link)
(use-package org-checklist)

(use-package org-brain
  :disabled t
  :requires org-capture
  :init
  (setq org-brain-path (concat notes-dir "/org/brain"))
  :config
  ;; (add-hook 'org-capture-mode-hook
  ;;           (push '("b" "Brain" plain (function org-brain-goto-end)
  ;;                   "* %i%?" :empty-lines 1)
  ;;                 org-capture-templates))
  (setq
   org-id-track-globally t
   org-id-locations-file "~/.emacs.d/.org-id-locations"
   org-brain-visualize-default-choices 'all
   org-brain-title-max-length 30
   org-brain-include-file-entries t
   org-brain-file-entries-use-title t
   org-brain-show-full-entry t)

  (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer))

(org-link-set-parameters
 "bz"
 :follow 'org-bugzilla-follow
 :face '(:foreground "red")
 :help-echo "Open bug in a browser.")

(defun org-bugzilla-follow (link)
  (message "Opening in browser")
  (shell-command (format "bugalert open %s" link)))

(setq org-agenda-exporter-settings
      '((ps-number-of-columns 2)
        (ps-landscape-mode t)
        (org-agenda-add-entry-text-maxlines 5)
        (htmlize-output-type 'css)
        (htmlize-pre-style t)))

(use-package org-protocol)
