(use-package magit
  :defer t
  :diminish
  :commands magit-get-top-dir
  :config
  (progn
    (setq magit-commit-signoff t)))

(use-package git-commit
  :defer t)

(use-package git-timemachine :defer 3)

(use-package helm-git-grep
  :after helm
  :config
  (progn
    (define-key isearch-mode-map (kbd "C-c s") 'helm-git-grep-from-isearch)
    (define-key helm-map (kbd "C-c s") 'helm-git-grep-from-helm)))

(use-package helm-ls-git :defer 3)

(use-package gist :defer 3)

(general-create-definer fconfig-vc-bind
  :prefix "C-c v"
  :name "Version control"
  "" '(:ignore t :which-key "Version control"))
