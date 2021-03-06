;; Constants for our use
(defconst fconfig/1MB (* 1 1024 1024))

;; Variables that can be configured from outsite
(defvar fconfig-dir (file-name-directory load-file-name))
(defvar fconfig-default-theme nil)
(defvar fconfig-default-font nil)
(defvar fconfig-module-dir (concat fconfig-dir "/modules"))
(defvar fconfig-pkg-dir (concat fconfig-dir "/pkgs"))
(defvar fconfig-gc-threshold (* 500 fconfig/1MB))

(add-to-list 'load-path fconfig-pkg-dir)

(defmacro set-fconfig-modules-dir! (path)
  (let (module-dir)
    (if (stringp path)
	(setq module-dir path)
      (setq module-dir (eval path)))
    `(progn
       (setq fconfig-dir ,module-dir))))

(defmacro fconfig! (module)
  (let*
      ((module-name (symbol-name module))
       (config-file (concat (file-name-as-directory fconfig-module-dir)
			    (concat module-name ".el"))))
    `(progn (load-file ,config-file)
	    (provide (intern (concat "fconfig-" ,module-name))))))

(defmacro global-map! (key fn)
  `(general-def ,key ,fn))

(defmacro global-unmap! (key)
  `(general-unbind ,key))

(defun fconfig/init ()
  "Increases garbage collection threshold for faster
  startup. `fconfig/finish` must be called later"
  (setq gc-cons-threshold (* 1000 fconfig/1MB))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (message "Emacs loaded in %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done))))

(defun fconfig/finish ()
  ;; Should be called only if fconfig/init is called earlier
  (setq gc-cons-threshold fconfig-gc-threshold))

(defun fconfig/theme! (theme &optional custom-path)
  (when custom-path
    (add-to-list 'custom-theme-load-path custom-path))
  (setq fconfig-default-theme theme))

(defun fconfig/font! (font)
  (setq fconfig-default-font font))

(add-hook 'after-make-frame-functions 'create-frame-hook)
(add-hook 'emacs-startup-hook 'create-frame-hook)
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)


(provide 'fconfig)
