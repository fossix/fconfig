(defvar speak/tts nil "text to speech process")

(defun tts-running ()
  (and (not (null speak/tts))
       (eq (process-status speak/tts) 'run)))

(defun start-tts ()
  (if (not (tts-running))
      (setq speak/tts
            (start-process "speak/tts"
                           "*tts-speak*"
                           "espeak" "-p70" "-s150" "-k20" "-v" "en-uk-rp+f1"))))

(defun end-tts ()
  (delete-process speak/tts)
  (setq speak/tts nil))

(defun speak-text (text)
  (start-tts)
  (process-send-string speak/tts (concat text "\n")))

(speak-text "Hello")

(defun speak (&optional text)
  "Speak word at point or region."
  (interactive)
  (unless text
    (setq text (if (use-region-p)
                   (buffer-substring
                    (region-beginning) (region-end))
                 (thing-at-point 'word))))
  (speak-text text))

;;; inspired by
;;; http://kitchingroup.cheme.cmu.edu/blog/2015/06/29/Getting-Emacs-to-read-to-me/
(defun say-word (&optional arg)
  "Speak word at point. With ARG, go forward ARG words."
  (speak (thing-at-point 'word)))

(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

(defun say-sentence ()
  "Speak sentence at point. With ARG, go forward ARG sentences."
  (interactive)
  (speak (replace-in-string "\n" " " (thing-at-point 'sentence))))

(defun say-paragraph ()
  "Speak paragraph at point. With ARG, go forward ARG paragraphs."
  (interactive)
  (speak (replace-in-string "\n" " " (thing-at-point 'paragraph))))

(end-tts)
