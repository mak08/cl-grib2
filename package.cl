;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-04-05 00:53:02>

(defpackage "CL-GRIB2"
  (:use "COMMON-LISP" "LOCAL-TIME" "CFFI")
  (:export  "WITH-C-FILE"
            "FOPEN"
            "FCLOSE"

            "GRIB-COUNT-IN-FILE"

            "GRIB-INDEX-NEW-FROM-FILE"
            "GRIB-INDEX-NEW"
            "GRIB-INDEX-DELETE"
            "GRIB-INDEX-ADD-FILE"

            "GRIB-INDEX-GET-SIZE"

            "GRIB-INDEX-GET-LONG"
            "GRIB-INDEX-GET-DOUBLE"
            "GRIB-INDEX-GET-STRING"

            "GRIB-INDEX-SELECT-LONG"
            "GRIB-INDEX-SELECT-DOUBLE"
            "GRIB-INDEX-SELECT-STRING"

            "GRIB-HANDLE-NEW-FROM-FILE"
            "WITH-HANDLE-FROM-INDEX"
            "GRIB-HANDLE-NEW-FROM-INDEX"
            "GRIB-HANDLE-DELETE"

            "GRIB-GET-SIZE"

            "GRIB-GET-LONG"
            "GRIB-GET-BYTES"
            "GRIB-GET-STRING"
            "GRIB-GET-DOUBLE-ARRAY"

            "GRIB-ITERATOR-NEW"
            "GRIB-ITERATOR-NEXT"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
