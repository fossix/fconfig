(use-package page-break-lines
  :defer t
  :diminish page-break-lines-mode)

(use-package dashboard
  :requires (org all-the-icons page-break-lines)
  :diminish dashboard-mode
  :commands dashboard-open
  :hook ((dashboard-mode . (lambda () (setq-local tab-width 1))))
  :bind (:map dashboard-mode-map
              ("<return>" . dashboard-return)
              ("<up>"           . widget-backward)
              ("p"           . widget-backward)
              ("n"         . widget-forward)
              ("<down>"         . widget-forward))

  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
				    (bookmarks . "book")))

  (setq
   dashboard-startup-banner "/home/santosh/Pictures/emacs_startup.png"
   initial-buffer-choice #'(lambda () (get-buffer "*dashboard*"))
   dashboard-banner-logo-title
   (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n")
   dashboard-center-content t
   dashboard-set-init-info nil
   dashboard-set-navigator t
   dashboard-items '((recents   . 7)
                      (bookmarks . 5)
                      (agenda . 5)
                      (projects . 5)))

  ;; command
  (defun dashboard-open ()
    (interactive)
    (dashboard-insert-startupify-lists)
    (switch-to-buffer "*dashboard*")
    (dashboard-mode))

  ;; Recent files
  (defun dashboard-insert-recents (list-size)
    "Add the list of LIST-SIZE items from recently edited files."
    (recentf-mode)
    (dashboard-insert-section
     "Recent Files:"
     recentf-list
     list-size
     "r"
     `(lambda (&rest ignore) (find-file-existing ,el))
     (abbreviate-file-name el)))

  ;; Bookmarks
  (defun dashboard-insert-bookmarks (list-size)
    "Add the list of LIST-SIZE items of bookmarks."
    (require 'bookmark)
    (dashboard-insert-section
     "Bookmarks:"
     (dashboard-subseq (bookmark-all-names)
                       0 list-size)
     list-size
     "m"
     `(lambda (&rest ignore) (bookmark-jump ,el))
     (let ((file (bookmark-get-filename el)))
       (if file
           (format "%s - %s" el (abbreviate-file-name file))
         el)))))

(setq dashboard-navigator-buttons
`(;; line1
  (
   ;; Open kernel source directory
   (,(all-the-icons-faicon "linux" :height 1.0 :v-adjust 0.0)
    "Kernel Source"
    "Linux kernel source"
    (lambda (&rest _) (dired "~/dev/repos/kernels/linux")))
   ;; flagged messages flagged search
   ("★ Starred Mails" nil "Show flags" (lambda (&rest _) (notmuch-search "tag:flagged")) error))
  (;; line 2
   (,(all-the-icons-faicon "home" :height 1.0 :v-adjust 0.0)
    "Homepage"
    "Browse homepage"
    (lambda (&rest _) (browse-url "http://fossix.org")))
   (,(all-the-icons-faicon "linkedin" :height 1.0 :v-adjust 0.0)
    "Linkedin"
    ""
    (lambda (&rest _) (browse-url "http://linkedin.com")))
   (,(all-the-icons-faicon "reddit" :height 1.0 :v-adjust 0.0)
    "Reddit"
    ""
    (lambda (&rest _) (browse-url "https://www.reddit.com"))))))
