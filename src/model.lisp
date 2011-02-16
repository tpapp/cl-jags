;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-jags)

(defparameter *indent* 0)

(defun out (expression)
  "Write symbolic expression corresponding to the model to *STANDARD-OUTPUT* for
parsing by R/JAGS.  *INDENT* controls indentation."
  (if (atom expression)
      (out-atom expression)
      (out-cons (car expression) (cdr expression))))

(defgeneric out-cons (first rest)
  (:documentation "Write a cons (usually a list), used by OUT."))

(defgeneric out-atom (atom)
  (:documentation "Write an atom, used by OUT."))

(defmethod out-atom ((symbol symbol))
  (princ (var-as-string symbol)))

(defmethod out-atom ((real real))
  (dump *standard-output* real))

(defun out-funcall (function &rest arguments)
  "Helper function for writing infix-style funcalls."
  (check-type function string)
  (princ function)
  (iter
    (for argument :in arguments)
    (princ (if (first-iteration-p) #\( #\,))
    (out argument))
  (princ #\)))

(defun indent ()
  "Write the required number of spaces."
  (iter
    (repeat *indent*)
    (princ #\space)))

(defmacro with-line (&body body)
  "Line with indentation."
  `(progn
     (indent)
     ,@body
     (format *standard-output* ";~%")))

(defun line (object)
  "Line with indentation."
  (with-line (out object)))

(defmacro with-braces (&body body)
  "Wrap output of BODY in braces, taking care of indentation."
  `(progn
     (format *standard-output* " {~%")
     (let ((*indent* (+ 2 *indent*)))
       ,@body)
     (indent)
     (format *standard-output* "}~%")))

(defmacro with-parens (&body body)
  "Put output of BODY in parentheses."
  `(progn
     (princ #\()
     ,@body
     (princ #\))))

(defmacro define-notation ((name &rest arguments) &key out)
  ""
  (with-unique-names (rest)
    `(progn
       (defmacro ,name ,arguments
         (declare (ignorable ,@(destructured-variables arguments)))
         (error "macro should never be called, purely as notation aid"))
       (defmethod out-cons ((first (eql ',name)) ,rest)
         (destructuring-bind ,arguments ,rest
           ,out)))))

;;; simple notation

(define-notation (each (index array &optional (dimension 1)) &body body)
    ;; ?? from-to syntax?
    :out (progn
           (indent)
           (princ "for (")
           (out index)
           (princ " in 1:")
           (out `(ref (dim ,array) ,dimension))
           (princ #\))
           (with-braces
             (mapc #'out body))))

(define-notation (~ expression distribution)
    :out (with-line
           (out expression)
           (princ " ~ ")
           (out distribution)))

(define-notation (@ expression value)
    :out (with-line
           (out expression)
           (princ " <- ")
           (out value)))

(define-notation (ref array &rest indexes)
    :out (progn
           (out array)
           (iter
             (for index :in indexes)
             (princ (if (first-iteration-p) #\[ #\,))
             (out index))
           (princ #\])))

;;; various infix operations

(defmacro define-infix ((name &rest arguments) out)
  (with-unique-names (rest)
    `(defmethod out-cons ((first (eql ',name)) ,rest)
       (destructuring-bind ,arguments ,rest
         ,out))))

(define-infix (normal mean sd)
    (out-funcall "dnorm" mean `(expt ,sd -2)))

(define-infix (uniform left right)
    (out-funcall "dunif" left right))

(define-infix (+ &rest arguments)
    (iter
      (for argument :in arguments)
      (unless (first-iteration-p) (princ #\+))
      (with-parens (out argument))))

(define-infix (length vector)
    (out-funcall "length" vector))

(define-infix (dim array)
    (out-funcall "dim" array))

(define-infix (expt base power)
    (progn
      (with-parens (out base))
      (princ #\^)
      (with-parens (out power))))

;;; model

(defclass model ()
  ((definition :accessor definition :initarg :definition)
   ))

(defmacro with-new-file ((stream filespec) &body body)
  `(with-open-file (,stream ,filespec :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
     ,@body))

(defun write-model (model filespec)
  "Write model to FILESPEC."
  (with-new-file (*standard-output* filespec)
    (princ "model ")
    (with-braces
      (mapc #'out (definition model)))))
