;;; From here: https://www.reddit.com/r/orgmode/comments/a1z26t/sunrise_sunset_as_separate_entries_on_agenda_view/
;; Sunrise (edits by Eph Zero)
;; Brady Trainor
;; http://stackoverflow.com/questions/22889036/custom-diary-sunrise-function-not-working-autoload-diary-emacs
(require 'solar)
(require 'cl-lib)

(defun solar-sunrise-string (date &optional nolocation)
  "String of *local* time of sunrise and daylight on Gregorian DATE."
  (let ((l (solar-sunrise-sunset date)))
    (format
     "%s"
     (if (car l)
         (concat "Sunrise " (apply 'solar-time-string (car l)))
       "no sunrise")
     (nth 2 l))))

;; To be called from diary-list-sexp-entries, where DATE is bound.
;;;###diary-autoload
(defun diary-sunrise ()
  "Local time of sunrise as a diary entry.
  Accurate to a few seconds."
  (or (and calendar-latitude calendar-longitude calendar-time-zone)
      (solar-setup))
  (solar-sunrise-string date))

;; Sunset
;; Brady Trainor
;; http://stackoverflow.com/questions/22889036/custom-diary-sunrise-function-not-working-autoload-diary-emacs

(defun solar-sunset-string (date &optional nolocation)
  "String of *local* time of sunset and daylight on Gregorian DATE."
  (let ((l (solar-sunrise-sunset date)))
    (format
     "%s (%s hours daylight)"
     (if (cadr l)
         (concat "Sunset " (apply 'solar-time-string (cadr l)))
       "no sunset")
     (nth 2 l))))

;; To be called from diary-list-sexp-entries, where DATE is bound.
;;;###diary-autoload
(defun diary-sunset ()
  "Local time of sunset as a diary entry.
  Accurate to a few seconds."
  (or (and calendar-latitude calendar-longitude calendar-time-zone)
      (solar-setup))
  (solar-sunset-string date))
