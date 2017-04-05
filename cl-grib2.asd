;;; -*- lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert
;;; Created        22/03/2000 11:15:16
;;; Last Modified  <michael 2017-04-06 01:15:50>

(defsystem "cl-grib2"
  :description "GRIB API bindings"
  :default-component-class cl-source-file.cl
  :depends-on ("cl-utilities" "zlib" "log2")
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "api")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

