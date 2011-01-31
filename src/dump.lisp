;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(in-package #:cl-jags)

(defgeneric dump (stream object)
  (:documentation "Dump OBJECT to STREAM, in a format readable by R and JAGS.
  The object is not named, use DUMP-NAMED for that."))

(defun dump-named (stream object name)
  "Dump OBJECT to STREAM (using the generic function DUMP), in a format readable
 by R and JAGS.  The object is named, ie assigned to NAME."
  (format stream "`~a` <-~%"
          (etypecase name
            (string name)
            (symbol (symbol-name name))))
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
    (cl-num-utils:with-indexing* (dimensions next-index :column-major? t)
      (dotimes (element-counter (array-total-size array))
        (unless (zerop element-counter) (princ #\, stream))
        (dump stream (row-major-aref array (next-index)))))
    (princ "), .Dim=" stream)
    (dump stream dimensions)
    (princ ")" stream)))
