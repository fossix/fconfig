# Fconfig

Clone this repo into `.emacs.d`, or pont the `fconfig-dir` to the fconfig
directory.

## Example config

```emacs-lisp
;;; -*- mode:emacs-lisp -*-

(require 'fconfig)

(fconfig/theme! #'aanila "~/dev/repos/aanila")
(fconfig/font! "Monaco-11")

(fconfig/init)
(let ((file-name-handler-alist nil))
  (fconfig! core)
  (fconfig! utils)
  (fconfig! packages)
  (fconfig! buffer)
  (fconfig! org-config)
  (fconfig! solar)
  (fconfig! dashboard)
  (fconfig! vc)
  (fconfig! search)
  (fconfig! frame)
  (fconfig! lsp)
  (fconfig! mail)
  (fconfig! progmode)
  (fconfig! mm)
  ;; (fconfig! finance)
  ;; (fconfig! speak)
  ;; (fconfig! devanagari)
  (fconfig! bindings)                   ; Better if this comes last
  )

(setq custom-file "~/.emacs.d/custom.el")

;;; personal setup
(setq
 user-mail-address "user@example.com"
 calendar-latitude 12.971599
 calendar-longitude 77.594563)

(global-unmap! "C-x o")

(global-map! "C-<f1>" 'other-window)
(global-map! "C-<f2>" 'other-frame)

(fconfig/finish)
```
