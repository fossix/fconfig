;;; Multimedia

;;; controlling spotify client
(use-package spotify
  :commands
  (spotify-play spotify-pause spotify-playpause spotify-previous spotify-next)
  :config (spotify-enable-song-notifications))

;;; TODO add spotify search. helm-spotify-plus?

(general-create-definer fconfig-mm-bind
  :prefix "C-c m"
  :name "Music control"
  "" '(:ignore t :which-key "Music control"))
