(use-package magit
  :ensure t
  :defer t
  :diminish
  :commands magit-get-top-dir
  :config
  (progn
    (setq magit-commit-signoff t)))

(use-package git-commit
  :ensure t
  :defer t)

(use-package git-timemachine :ensure t :defer 3)

(use-package helm-git-grep
  :ensure t
  :after helm
  :config
  (progn
    (define-key isearch-mode-map (kbd "C-c s") 'helm-git-grep-from-isearch)
    (define-key helm-map (kbd "C-c s") 'helm-git-grep-from-helm)))

(use-package helm-ls-git
  :ensure t
  :after helm
  :defer 3)

(use-package gist :ensure t :defer 3)

(general-create-definer fconfig-vc-bind
  :prefix "C-c v"
  :name "Version control"
  "" '(:ignore t :which-key "Version control"))
