;;; Org-mode setup
;;; Some (most clock related) settings taken from doc.norang.ca/org-mode.html

(require 'notifications)

(set-face-attribute 'org-headline-done nil :strike-through t)


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
  "Switch a task from NEXT to in progress (PROG) when clocking in.
   Skips capture tasks, projects, and subprojects."
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (string= (org-get-todo-state) "NEXT")
           (not (is-project-p)))
      "PROG"))))

(setq org-clock-in-switch-to-state 'clock-in-to-next)

(defvar org-state)
(defvar org-last-state)

(defun org-clock-in-if-starting ()
  "Clock in when the task is marked STARTED."
  (when (and (string= org-state "PROG")
             (not (string= org-last-state org-state)))
    (org-clock-in)))

(defun org-clock-out-if-waiting ()
  "Clock out when the task is marked WAITING."
  (when (and (string= org-last-state "PROG")
             (string= org-state "TODO"))
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
