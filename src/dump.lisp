;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(in-package #:cl-jags)

(defun var-as-string (var)
  "Return a string that corresponds to a variable name. - is replaced by _,
the latter is not allowed in symbols that denote R/JAGS variables.."
  (check-type var symbol)
  (bind ((string (symbol-name var))
         ((:flet invalid-error (cause))
          (error "Symbol name ~A is not valid as an R/JAGS variable name (~A)."
                 string cause))
         (string (map 'string 
                      (lambda (char)
                        (unless (standard-char-p char)
                          (invalid-error "contains non-standard characters"))
                        (cond
                          ((or (alphanumericp char) (char= char #\.)) char)
                          ((char= char #\-) #\_)
                          ((char= char #\_)
                           (invalid-error 
                            "contains an underline, use a hyphen instead"))
                          (t (invalid-error 
                              "characters outside a-z, A-Z, . or -"))))
                      string)))
    string))

(defgeneric dump (stream object)
  (:documentation "Dump OBJECT to STREAM, in a format readable by R and JAGS.
  The object is not named, use DUMP-NAMED for that."))

(defun dump-named (stream name object)
  "Dump OBJECT to STREAM (using the generic function DUMP), in a format readable
 by R and JAGS.  The object is named, ie assigned to NAME."
  (format stream "`~a` <-~%" (var-as-string name))
  (dump stream object)
  (terpri stream))

(defmethod dump (stream (number real))
  (labels ((dump-float (float)
             (let ((*read-default-float-format* 'double-float))
               (format stream "~,,,,,,'ee" float))))
    (etypecase number
      (integer (format stream "~d" number))
      (float (dump-float number))
      (rational (dump-float (coerce number 'double-float))))))

(defmethod dump (stream (vector vector))
  (princ "c(" stream)
  (iter
    (for element :in-vector vector)
    (unless (first-iteration-p) (princ #\, stream))
    (dump stream element))
  (princ ")" stream))

(defmethod dump (stream (array array))
  (let* ((dimensions (coerce (array-dimensions array) '(simple-array fixnum (*)))))
    (assert (plusp (array-rank array)))
    (princ "structure(c(" stream)
    (cl-num-utils:with-indexing* (dimensions index next-index :column-major? t)
      (iter
        (unless (first-iteration-p ) (princ #\, stream))
        (dump stream (row-major-aref array index))
        (until (next-index))))
    (princ "), .Dim=" stream)
    (dump stream dimensions)
    (princ ")" stream)))
