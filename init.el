(setq config-dir (file-name-directory
                  (file-truename (or load-file-name (buffer-file-name)))))

(add-hook 'after-init-hook
          `(lambda ()
             (require 'org)
             ;; load up the starter kit
             (org-babel-load-file (expand-file-name "emacs-config.org"
                                                    config-dir))))
