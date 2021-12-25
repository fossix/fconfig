;;; Org-mode setup
;;; Some (most clock related) settings taken from doc.norang.ca/org-mode.html

(require 'notifications)

(use-package org
  :mode ("\\.org$" . org-mode)
  :commands (org
             org-capture
             org-mode
             org-store-link
             update-org-hours
             my-term-agenda
             dired-notes)
  :init
  (add-to-list 'org-modules 'org-habit 'drill)
  (setq
   org-directory (expand-file-name "org" notes-dir)
   org-default-notes-file (expand-file-name "notes" org-directory))
  :config
  (setq
   org-ditaa-jar-path ""
   org-babel-ditaa-java-cmd "/usr/bin/ditaa"
   org-ditaa-jar-option ""
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

   ;; Don't ask when I evaluate code
   org-confirm-babel-evaluate nil

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
  (use-package ob-ledger)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (go . t)
     (dot . t)
     (ditaa . t)
     (latex . t)
     (ledger .t)
     (shell . t)
     (rust . t)
     (scheme . t)
     (gnuplot . t)))

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

  (use-package org-agenda
    :bind (:map org-agenda-mode-map ([C-f9] . org-agenda-goto-today))
    :commands org-agenda
    :hook (org-agenda-mode . hl-line-mode)

    :init
    (setq org-refile-targets
	  '((nil :maxlevel . 5)
	    (org-agenda-files :maxlevel . 5)))

    :config
    (appt-activate t)

    ;; org appointments
    ;; Get appointments for today
    (defun ss/org-agenda-to-appt ()
      (interactive)
      (setq appt-time-msg-list nil)
      (let ((org-deadline-warning-days 0))
	(org-agenda-to-appt)))

    (defun ss/appt-disp-window (min-to-app new-time msg)
      (save-window-excursion (notifications-notify
			      :title "Appointment"
			      :body msg)))

    (setq appt-message-warning-time '30
	  appt-display-interval '5
	  appt-display-format 'window
	  appt-disp-window-function 'ss/appt-disp-window)

    (defadvice org-agenda-redo (after org-agenda-redo-add-appts)
      "Pressing `r' on the agenda will also add appointments."
      (progn
	(setq appt-time-msg-list nil)
	(org-agenda-to-appt)))

    (ad-activate 'org-agenda-redo)

    (add-hook 'org-finalize-agenda-hook 'ss/org-agenda-to-appt)
    (add-hook 'org-finalize-agenda-hook 'ss/notify-on-clocked-time)

    (run-at-time "24:01" nil 'ss/org-agenda-to-appt)

    (defun day-agenda-skip ()
      "Skip trees that are of priority A and has a meeting tag"
      (let ((subtree-end (save-excursion (org-end-of-subtree t)))
	    (pri-value (* 1000 (- org-lowest-priority ?A)))
	    (pri-current (org-get-priority (thing-at-point 'line t)))
	    (case-fold-search t))
	(if (or (re-search-forward ":meeting:" subtree-end t)
		(= pri-value pri-current))
	    subtree-end
	  nil)))

    (defun org-agenda-skip-if-blocked ()
      (let ((next-headline (save-excursion
			     (or (outline-next-heading) (point-max)))))
	(if (org-entry-blocked-p) next-headline)))

    ;; From here: http://doc.norang.ca/org-mode.html
    (defun bh/skip-habits ()
      "Skip habits"
      (save-restriction
	(widen)
	(let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
	  (if (org-is-habit-p)
	      next-headline
	    nil))))

    ;; (defun ss/org-skip-sunrise ()
    ;;   (if (and (not (equal date (calendar-current-date)))
    ;;            (string= (org-get-category) "Sunrise"))
    ;;       (org-end-of-subtree t)
    ;;     nil))

    ;; https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
    (defun ss/org-agenda-skip-subtree-if-priority (priority)
      "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
      (let ((subtree-end (save-excursion (org-end-of-subtree t)))
            (pri-value (* 1000 (- org-lowest-priority priority)))
            (pri-current (org-get-priority (thing-at-point 'line t))))
        (if (= pri-value pri-current)
            subtree-end
          nil)))

    ;; Easy basic searches. Get a quick view of next actions, etc
    (setq org-agenda-custom-commands
	  ;; We will have 5 blocks under the "Agenda and TODOs headline. The first
	  ;; block will show the scheduled items for today, deadlines, meetings
	  ;; etc.
	  ;;
	  ;; The second block shows high priority tasks (doesn't matter if a task
	  ;; is started or in the next state, if it's high priority it shows up
	  ;; here).
	  ;;
	  ;; The third block shows the "Next and Ongoing tasks", but
	  ;; it's skipped from being displayed if it's a scheduled entry, or
	  ;; blocked. This is so because we don't want to clutter the view. If a
	  ;; task is scheduled, then we know it's to be done sometime soon, so it
	  ;; a blocked talk, because the child will either be scheduled or be
	  ;; shown in the "Pending items" block.
	  ;;
	  ;; The next block shows all tasks that are due within 30 days, and
	  ;; finally "Pending items", to show the remaining tasks with effort
	  ;; estimate.
	  ;;
	  ;; NOTE: Since blocked items won't be shown, make sure the children are
	  ;; TODO items, if they are check boxes, set the NOBLOCKING property.
	  '(("A" "Agenda and TODOs"
	     ((agenda ""
		      ((org-agenda-overriding-header "Today's Agenda")
		       (org-agenda-span 'day)
                       (org-agenda-prefix-format " %i %?-12t% s")
                       (org-agenda-scheduled-leaders '("" "%2dx "))
                       (org-agenda-use-time-grid t)
		       (org-deadline-warning-days 7)
		       (org-agenda-skip-scheduled-if-deadline-is-shown 'not-today)
		       (org-agenda-skip-deadline-prewarning-if-scheduled 3)
		       (org-agenda-skip-scheduled-delay-if-deadline t)
		       (org-agenda-skip-timestamp-if-deadline-is-shown nil)
		       (org-agenda-sorting-strategy
			'(time-up todo-state-up priority-down))
		       (org-agenda-skip-function 'org-agenda-skip-if-blocked)))

	      (tags "PRIORITY=\"A\"/-DONE-CLOSED-FIXED-CANCELLED"
		    ((org-agenda-overriding-header "High priority")
		     ;; If priority inheritance work's the following could be
		     ;; uncommented, so only the next actionable child shows up.
		     ;; (org-agenda-dim-blocked-tasks 'invisible)
                     (org-agenda-prefix-format " %i % s")))

	      (tags "/NEXT|STARTED|WAITING|ASSIGNED|INPROGRESS"
		    ((org-agenda-sorting-strategy
		      '(priority-down effort-down todo-state-down))
                     (org-agenda-prefix-format " %i % s")
		     (org-agenda-skip-function
		      (progn
			'(or (org-agenda-skip-if-blocked)
			     (org-agenda-skip-entry-if 'scheduled)
                             (ss/org-agenda-skip-subtree-if-priority ?A))))
		     (org-agenda-overriding-header "Next and Ongoing tasks")))

	      (tags-todo "-bill&+DEADLINE>\"<today>\"+DEADLINE<\"<+30d>\"&TODO<>\"STARTED\""
			 ((org-agenda-overriding-header "Due Within a Month")
			  (org-agenda-skip-function
			   (progn
			     '(or
			       (org-agenda-skip-entry-if 'notdeadline))))))

	      (alltodo ""
		       ((org-agenda-overriding-header "Pending items")
			(org-agenda-prefix-format " %i [%e] ")
			(org-agenda-sorting-strategy
			 '(priority-down effort-up todo-state-down category-keep))
			(org-agenda-skip-function
			 (progn
			   '(or (org-agenda-skip-if-blocked)
				(org-agenda-skip-entry-if 'regexp "\\[#A\\]")
				(org-agenda-skip-subtree-if 'regexp ":someday:")
				(org-agenda-skip-if nil '(scheduled deadline timestamp))
				(org-agenda-skip-entry-if 'todo '("STARTED"
		       "NEXT" "WAITING" "MOVED")))))))))

	    ("r" "Tasks to be refiled" tags "refile"
	     ((org-agenda-files '("~/notes/org/dump.org" "~/notes/org/TODO"
				  "~/notes/org/work" "~/notes/org/journal.org"))))

	    ("W" "Work week review"
	     ((agenda ""
		      ((org-agenda-start-on-weekday 1)
		       (org-agenda-show-log t)
		       (org-agenda-time-grid nil)
		       (org-agenda-start-with-log-mode t)
		       (org-agenda-include-diary nil)
		       (org-agenda-log-mode-items '(state clock))
		       (org-agenda-files '("~/notes/org/work.org"))
		       (org-agenda-start-with-clockreport-mode t)
		       (org-agenda-span 'week)
		       (org-agenda-start-day "-7")
		       (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
		       (org-agenda-overriding-header "Work week in Review")))))

            ("T" "Todo week review"
	     ((agenda ""
		      ((org-agenda-start-on-weekday 1)
		       (org-agenda-show-log t)
		       (org-agenda-time-grid nil)
		       (org-agenda-start-with-log-mode t)
		       (org-agenda-include-diary nil)
		       (org-agenda-log-mode-items '(state clock))
		       (org-agenda-files '("~/notes/org/TODO"))
		       (org-agenda-start-with-clockreport-mode t)
		       (org-agenda-span 'week)
		       (org-agenda-start-day "-7")
		       (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
		       (org-agenda-overriding-header "Week in Review")))))

	    ("e" tags "EFFORT>\"0\""
	     ((org-agenda-overriding-header "All tasks with effort estimate set")
	      (org-agenda-prefix-format "[%e] ")
	      (org-agenda-sorting-strategy
	       '(priority-down todo-state-down effort-up category-keep))
	      (org-agenda-skip-function
	       (progn
		 '(or (org-agenda-skip-if-blocked) (day-agenda-skip)
		      (org-agenda-skip-entry-if 'scheduled 'deadline 'todo
	                                        '("DONE" "CANCELLED" "CLOSED"))
		      (bh/skip-habits))))
	      (org-agenda-files org-agenda-files)))

            ("X" agenda ""
                ((org-agenda-prefix-format " [ ] ")
                 (org-agenda-with-colors t)
                 (org-agenda-remove-tags t))
                ("~//tmp/agenda.html"))
            ))

    (setq org-agenda-time-grid '((daily today remove-match)
                                 (800 1000 1200 1400 1600 1800 2000)
                                 "......" "----------------"))

    ;; functions to remind me to stop working for the day
    (defun ss/org-clock-total-sum-today ()
      "Get the total clocked time today across all agenda files in minutes."
      (let ((files (org-agenda-files))
	    (total 0))
	(org-agenda-prepare-buffers files)
	(dolist (file files)
	  (with-current-buffer (find-buffer-visiting file)
	    (setq total (+ total (org-clock-sum-today)))))
	total))

    (defvar ss/clocked-notify-limit
      "The duration in hours, after which org-timeout should send notification")

    (defalias 'clocked-notify-ok-callback-fn nil
      "The callback function to be called when notification ok is clicked")

    (defalias 'clocked-notify-cancel-callback-fn nil
      "The callback function to be called when notification cancel is clicked")

    (setq ss/clocked-notify-limit 8)

    (defun ss/clocked-time-notify ()
      (if (>= (/ (ss/org-clock-total-sum-today) 60) ss/clocked-notify-limit)
	  (notifications-notify
	   :title "Time to leave"
	   :body "Clocked time exceeded."
	   :timeout -1)))
    ;; :actions '("Confirm" "OK" "Refuse" "Cancel")
    ;; :on-action 'clocked-notify-ok-callback-fn
    ;; :on-close 'clocked-notify-cancel-callback-fn)))

    (defun ss/notify-on-clocked-time ()
      "Notify if total clocked time exceeds `clocked-notify-limit`"
      (run-with-timer 0 1800 'ss/clocked-time-notify)))

  (use-package org-capture
    :requires org
    :commands org-capture
    :config
    (add-hook 'org-capture-mode-hook
	      (lambda ()
		(setq-local org-tag-alist (org-global-tags-completion-table))))

    ;; Some notes/todo capture templates
    (setq org-capture-templates
	  (append
	   '(("u" "Quotes" entry
	      (file+headline (concat notes-dir "/quotes") "Quotes")
	      "* %^{Quote} -- %^{Author}"))

	   '(("t" "Todo" entry
	      (file+headline "dump.org" "Tasks")
	      "* TODO %^{Do-what?} %^g\n  %i%?"))

	   '(("d" "Idea/Project dump" entry
	      (file+headline "dump.org" "Dump")
	      "* TODO %^{Do-what?} :refile:%^g\n  %i%?"))

	   '(("m" "Meetings" entry
	      (file+headline "todo.org" "Meetings/Appointments/Calls")
	      "* TODO %^{Meeting-Name} :meeting:%^g\n  %i%?"))

	   '(("n" "Today's Notes" entry
	      (file+olp+datetree "notes.org" "Unfiled")
	      "* %^{Title} %^g\n %i%?\n %a"))

	   '(("f" "Remote Capture" entry
	      (file+headline "notes.org" "Captured")
	      "* %^{Title} :browser:%^g\n %i%?\n"))

	   '(("p" "Phone call" entry
	      (file "journal.org" "Phone Calls")
	      "* PHONE %? :PHONE:%^g\n%U" :clock-in t :clock-resume t))

	   '(("r" "Random" entry
	      (file+headline "misc.org" "Random")
	      "* %u %?\n  %i"))

	   '(("j" "Journal entries")
	     ("jl" "General logging" entry
	      (file+olp+datetree "journal.org")
	      "** %U %^{Activity}  :log:%^g" :immediate-finish t)
	     ("jc" "Misc clockable entries" entry
	      (file+olp+datetree "journal.org")
	      "* %U %^{Activity} :time:%^g\n" :clock-in t :clock-resume t)
	     ;; A quick way to file breaks with running clock
	     ("jb" "Take a break!" entry
	      (file+olp+datetree "journal.org")
	      "* %U %^{Activity} :time:break:%^g\n" :clock-in t :clock-resume t))

	   '(("w" "Work related entries")
	     ("wm" "Misc clockable entries" entry
	      (file+olp+datetree "work.org")
	      "* %U %^{Activity} :time:%^g\n" :clock-in t :clock-resume t)
	     ("wM" "Work meetings" entry
	      (file+headline "work.org" "Meetings")
	      "* TODO %^{Meeting-Name} %^g\n  %^T\n  %i%?")
	     ("wp" "Calls from Work-phone calls" entry
	      (file "journal.org" "Phone Calls")
	      "* PHONE to: %^{Call to} :PHONE:%^g\n%U" :clock-in t :clock-resume t)
	     ("wt" "Todo" entry
	      (file+headline "work.org" "Tasks")
	      "* TODO %^{Do-what?} %^g\n    %?%i")
	     ("wd" "Task dump" entry
	      (file+headline "work.org" "Task dump")
	      "* %^{Do What?} :refile:%^g\n%?%i")
	     ("ws" "Standup Entries" entry
	      (file+olp+datetree "standup.org")
	      "* %^{Summary}\n  1. %i%?" :tree-type week))
	   org-capture-templates))

    ;; system wide org-capture
    ;; https://www.reddit.com/r/emacs/comments/74gkeq/system_wide_org_capture/
    (defadvice org-switch-to-buffer-other-window
	(after supress-window-splitting activate)
      "Delete the extra window if we're in a capture frame"
      (if (equal "capture" (frame-parameter nil 'name))
	  (delete-other-windows)))

    (defadvice org-capture-finalize
	(after delete-capture-frame activate)
      "Advise capture-finalize to close the frame"
      (when (and (equal "capture" (frame-parameter nil 'name))
		 (not (eq this-command 'org-capture-refile)))
	(delete-frame)))

    (defadvice org-capture-refile
	(after delete-capture-frame activate)
      "Advise org-refile to close the frame"
      (delete-frame))

    (defun activate-capture-frame ()
      "run org-capture in capture frame"
      (select-frame-by-name "capture")
      (switch-to-buffer (get-buffer-create "*scratch*"))
      (org-capture)))

  (use-package org-brain
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

  (use-package org-protocol))
