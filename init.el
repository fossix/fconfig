(setq config-dir (file-name-directory
                  (file-truename (or load-file-name (buffer-file-name)))))

(add-hook 'after-init-hook
          `(lambda ()
             (require 'org)
             ;; load up the starter kit
             (org-babel-load-file (expand-file-name "emacs-config.org"
                                                    config-dir))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#d4d4d4" :background "#000000"))))
 '(variable-pitch ((t (:family "Noto Sans")))))
