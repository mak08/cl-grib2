;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-04-05 01:18:20>

(in-package cl-grib2)

(defmacro with-handle-from-index ((handlevar index) &body forms)
  (let ((resultvar (gensym "RESULT-")))
    `(let* ((,handlevar (grib-handle-new-from-index ,index))
            (,resultvar
             (progn ,@forms)))
       (grib-handle-delete ,handlevar)
       ,resultvar)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
