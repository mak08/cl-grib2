;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description  libgrib_api bindings
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2017-04-06 00:28:39>

(in-package cl-grib2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GRIB_API bindings
;;;  https://software.ecmwf.int/wiki/display/GRIB/GRIB+API+installation
;;;  Install with ./configure --disable-jpeg

(define-foreign-library libgrib_api
  (:linux "libgrib_api.so"))

(use-foreign-library libgrib_api)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Handles

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grib_count_in_file

(defun grib-count-in-file (c-file)
  "Counts the messages contained in a file resource."
  (with-foreign-object
      (n '(:pointer :int))
    (let ((err
           (grib_count_in_file (null-pointer) c-file n)))
      (case err
        (0
         (mem-ref n :int))
        (otherwise
         (error "Error ~a" err))))))

(defcfun grib_count_in_file :int
  (context :pointer)
  (file :pointer)
  (n (:pointer :int)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grib_handle_new_from_file

(defun grib-handle-new-from-file (c-file)
  (with-foreign-object (err :int)
    (values (grib_handle_new_from_file (null-pointer) c-file err)
            (mem-ref err :int))))

(defcfun grib_handle_new_from_file
    :pointer
  (context :pointer)
  (file :pointer)
  (err (:pointer :int)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get next selected message handle in index.
;; Function is listed in the index module, not in the handle module.
;; https://software.ecmwf.int/wiki/display/GRIB/index.c

(defun grib-handle-new-from-index (index)
  (with-foreign-object (err :int)
    (let ((handle (grib_handle_new_from_index index err)))
      (if (= 0 (mem-ref err :int))
          handle
          (error "Failed to create handle from index ~a" index)))))

(defcfun grib_handle_new_from_index
    :pointer
  (index :pointer)
  (err (:pointer :int)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grib_handle_delete

(defun grib-handle-delete (handle)
  (grib_handle_delete handle))

(defcfun grib_handle_delete :int (handle :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indexes
;;;
;;; The grib_index is the structure giving indexed access to messages in a file.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grib_index_new

(defun grib-index-new (keys)
  (with-foreign-object (err :int)
    (let ((keys-string (format () "~{~a~^,~}" keys)))
      (values (grib_index_new (null-pointer)
                              keys-string
                              err)
              (mem-ref err :int)))))

(defcfun grib_index_new
    :pointer
  (context :pointer)
  (keys :string)
  (error (:pointer :int)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grib_index_new_from_file

(defun grib-index-new-from-file (filename keys)
  (with-foreign-object (err :int)
    (let* ((keys-string (format () "~{~a~^,~}" keys))
           (index (grib_index_new_from_file (null-pointer)
                                            filename
                                            keys-string
                                            err)))
      (if (= 0 (mem-ref err :int))
          index
          (error "Failed to open ~a, error: ~a" filename (mem-ref err :int))))))

(defcfun grib_index_new_from_file
    :pointer
  (context :pointer)
  (filename :string)
  (keys :string)
  (error (:pointer :int)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delerte index

(defcfun grib-index-delete :void (index :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add_file

(defcfun grib-index-add-file
    :int
  (index :pointer)
  (filename :string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; size of index wrt. key

(defun grib-index-get-size (index key)
  (with-foreign-object (size :int)
    (let ((retval (grib_index_get_size index key size)))
      (if (= retval 0)
          (mem-ref size :int)
          (values nil retval)))))

(defcfun grib_index_get_size
    :int
  (index :pointer)
  (key :string)
  (size :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get distinct values of key - long

(defun grib-index-get-long (index key
                            &optional (count (grib-index-get-size index key)))
  (with-foreign-objects
      ((values :long count)
       (size :int))
    (setf (mem-ref size :int) count)
    (let* ((res (grib_index_get_long index key values size))
           (keys (make-array count :element-type 'integer)))
      (case res
        (0
         (dotimes (k count keys)
           (setf (aref keys k) (mem-aref values :long k))))
        (otherwise
         (error "grib-index-get-long: ~a" res))))))

(defcfun grib_index_get_long
    :int
  (index :pointer)
  (key :string)
  (value :pointer)
  (size (:pointer :int)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get distinct values of key - double

(defun grib-index-get-double (index key
                            &optional (count (grib-index-get-size index key)))
  (with-foreign-objects
      ((values :double count)
       (size :int))
    (setf (mem-ref size :int) count)
    (let ((res (grib_index_get_double index key values size))
          (keys (make-array count :element-type 'double-float)))
    (case res
      (0
       (dotimes (k count keys)
         (setf (aref keys k) (mem-aref values :double k))))
      (otherwise
       (error "grib-index-get-double: ~a" res))))))

(defcfun grib_index_get_double
    :int
  (index :pointer)
  (key :string)
  (value :pointer)
  (size (:pointer :int)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get distinct values of key - string

(defun grib-index-get-string (index key
                            &optional (count (grib-index-get-size index key)))
  (with-foreign-objects
      ((values :pointer count)
       (size :int count))
    (setf (mem-ref size :int) count)
    (let ((res (grib_index_get_string index key values size))
          (keys (make-array count :element-type 'string)))
      (case res
        (0
         (dotimes (k count keys)
           (setf (aref keys k) (mem-aref values :string k))))
        (otherwise
         (error "grib-index-get-string: ~a" res))))))

(defcfun grib_index_get_string
    :int
  (index :pointer)
  (key :string)
  (value :pointer)
  (size (:pointer :int)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Select handles (messages) matching key

(defcfun grib-index-select-long
  :int
  (index :pointer)
  (key :string)
  (value :long))

(defcfun grib-index-select-double
  :int
  (index :pointer)
  (key :string)
  (value :double))

(defcfun grib-index-select-string
  :int
  (index :pointer)
  (key :string)
  (value :string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Iterators

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new

(defun grib-iterator-new (handle)
  (with-foreign-object (err '(:pointer :int)) 
    (values 
     (grib_iterator_new handle 0 err)
     (mem-ref err :int))))

(defcfun grib_iterator_new
  :pointer
  (handle :pointer)
  (flags :long)
  (err :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; next

(defun grib-iterator-next (iterator)
  (with-foreign-objects
      ((lat '(:pointer :double))
       (lon '(:pointer :double))
       (val '(:pointer :double)))
    (let ((res (grib_iterator_next iterator lat lon val)))
      (when (> res 0)
        (values (mem-ref lat :double)
                (mem-ref lon :double)
                (mem-ref val :double))))))

(defcfun grib_iterator_next :int
  (iterator :pointer)
  (lat :pointer)
  (lon :pointer)
  (val :pointer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; previous

(defun grib-iterator-previous (iterator)
  (with-foreign-objects
      ((lat '(:pointer :double))
       (lon '(:pointer :double))
       (val '(:pointer :double)))
    (let ((res (grib_iterator_previous iterator lat lon val)))
      (when (> res 0)
        (values (mem-ref lat :double)
                (mem-ref lon :double)
                (mem-ref val :double))))))

(defcfun grib_iterator_previous :int
  (iterator :pointer)
  (lat :pointer)
  (lon :pointer)
  (val :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reset

(defcfun grib-iterator-reset :int
  (iterator :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Get Data

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get the number of coded value from a key, if several keys of the same name are present, the total sum is returned. 

(defun grib-get-size (handle key)
  (with-foreign-object
      (value :long)
    (let ((retval (grib_get_size handle key value)))
      (if (= retval 0)
          (mem-ref value :long)
          (values nil retval)))))

(defcfun grib_get_size :int
  (handle :pointer)
  (key :string)
  (value (:pointer :long)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get raw bytes values from a key. 

(defun grib-get-bytes (handle key num)
  (with-foreign-objects
      ((value :int)
       (bytes :int8 num))
    (let ((retval (grib_get_bytes handle key value num)))
      (if (= retval 0)
          (loop :for k :below num :collect (mem-aref bytes :int8))
          (error "Failed to read ~a, error ~a" key retval)))))

(defcfun grib_get_bytes :int
  (handle :pointer)
  (key :string)
  (bytes :pointer)
  (length :int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get a long value from a key, if several keys of the same name are present, the last one is returned. 

(defun grib-get-long (handle key)
  (with-foreign-object
      (value :long)
    (let ((retval (grib_get_long handle key value)))
      (if (= retval 0)
          (mem-ref value :long)
          (error "Failed to read ~a, error ~a" key retval)))))

(defcfun grib_get_long :int
  (handle :pointer)
  (key :string)
  (value (:pointer :long)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get a double value from a key, if several keys of the same name are present, the last one is returned. 

(defun grib-get-double (handle key)
  (with-foreign-object
      (value :double)
    (let ((retval (grib_get_double handle key value)))
      (if (= retval 0)
          (mem-ref value :double)
          (values nil retval)))))

(defcfun grib_get_double :int
  (handle :pointer)
  (key :string)
  (value (:pointer :double)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get a string value from a key, if several keys of the same name are present, the last one is returned. 

(defun grib-get-string (handle key)
  (with-foreign-objects
      ((value :char 100)
       (length :int))
    (setf (mem-ref length :int) 100)
    (let ((retval (grib_get_string handle key value length)))
      (if (= retval 0)
          (foreign-string-to-lisp value)
          (error "Failed to get string value: ~a" retval)))))

(defcfun grib_get_string :int
  (handle :pointer)
  (key :string)
  (value (:pointer :string))
  (length (:pointer :int)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get double array values from a key. 

(defun grib-get-double-array (handle key &aux (length (grib-get-size handle key)))
  (with-foreign-objects
      ((value :double length)
       (size :int))
    (setf (mem-aref size :int) length)
    (let ((retval
           (grib_get_double_array handle key value size)))
      (if (= retval 0)
          (let* ((length-out (mem-ref size :int))
                 (result (make-array length-out :element-type 'double-float)))
            (prog1 
                (dotimes (k length-out result)
                  (setf (aref result k) (mem-ref value :double k)))
              ;; Try to work around bugs.launchpad.net/sbcl/+bug/1446962
              #+sbcl (sb-ext:gc :full t)))
          (values nil retval)))))

(defcfun grib_get_double_array :int
  (handle :pointer)
  (key :string)
  (value :pointer)
  (length (:pointer :int)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Additional bindings: C file access 

(defmacro with-c-file ((var path mode) &body forms)
  `(let ((,var (fopen ,path ,mode)))
     (unwind-protect
          (progn ,@forms)
       (unless (null-pointer-p ,var)
         (fclose ,var)))))

(defcfun fopen :pointer (fname :string) (mode :string))

(defcfun fclose :int (file :pointer))

(defcfun fprintf :int (file :pointer) (format :string) &rest)

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
