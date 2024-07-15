;;; config-calendar.el --- Description -*- lexical-binding: t; -*-

;; Prevent these holidays from appearing in the calendar
(setq holiday-bahai-holidays nil
      holiday-general-holidays nil
      holiday-hebrew-holidays nil
      holiday-islamic-holidays nil
      holiday-oriental-holidays nil
      holiday-solar-holidays nil
      )

;; I would prefer to use the following method, but unfortunately they
;; don't appear on Android Orgzly app.  So instead, I declare them in
;; a separate org file and comment this out, in case I get it to work
;; in the future on Android.

;; (setq holiday-local-holidays
;;       '((holiday-fixed 1 1 "Año nuevo")
;;         (holiday-float 2 1 2 "Carnaval")
;;         (holiday-float 2 2 2 "Carnaval")
;;         (holiday-fixed 3 24 "Día nacional de la Memoria por la Verdad y la Justicia")
;;         (holiday-fixed 4 2 "Día del Veterano y de los Caídos en la Guerra de Malvinas")
;;         (holiday-fixed 5 1 "Día del Trabajador")
;;         (holiday-fixed 5 25 "Día de la Revolución de Mayo")
;;         (holiday-fixed 6 20 "Paso a la Inmortalidad del General Manuel Belgrano (Día de la bandera)")
;;         (holiday-fixed 7 9 "Día de la Independencia")
;;         (holiday-fixed 12 8 "Inmaculada Concepción de María")
;;         ;; Navidad ya esta incluido en holiday-christian-holidays
;;         (holiday-fixed 12 25 "Día nacional de la Memoria por la Verdad y la Justicia")))

(setq org-agenda-include-diary t)


(provide 'config-calendar)
;;; config-calendar.el ends here
