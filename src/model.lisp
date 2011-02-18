;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-jags)

;;; expansion

(defclass expansion-table ()
  ((arrays :accessor arrays :initarg :arrays :initform nil)))

(defparameter *expansion-table* (make-instance 'expansion-table)
  "Default expansion table.")

(defgeneric expand-cons (key form)
  (:documentation "Expand FORM, dispatching on KEY.  If the form is not to be
  expanded further, the second value returned is T.")
  (:method (key form)
    (aif (find key (arrays *expansion-table*))
         (cons 'aref form)
         (values form t))))

(defun expand-1 (form)
  (cond
    ((consp form) (expand-cons (car form) form))
    (t (values form t))))

(defun expand (form)
  (bind (((:values expansion final?) (expand-1 form)))
    (if final?
        (if (listp expansion)
            (mapcar #'expand-1 expansion)
            (expand-1 expansion))
        (expand expansion))))

;;; output to R/JAGS syntax

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

(defmacro define-expansion (name (&rest arguments) &body body)
  (with-unique-names (form)
    `(defmethod expand-cons ((first (eql ',name)) ,form)
       (destructuring-bind ,arguments (cdr ,form)
         ,@body))))

(defmacro define-primitive (name (&rest arguments) &body body)
  (with-unique-names (rest)
    `(defmethod out-cons ((first (eql ',name)) ,rest)
       (destructuring-bind ,arguments ,rest
         ,@body))))

(defmacro define-primitive* (name (&rest arguments) &body body)
  `(progn
     (defun ,name ,arguments
       (declare (ignorable ,@arguments))
       (error "This function is provided as a syntactic aid and is not meant 
               to be used as CL code."))
     (define-primitive ,name ,arguments ,@body)))

(defmacro define-expansion* (name (&rest arguments) &body body)
  `(progn
     (defmacro ,name ,arguments
       (declare (ignorable ,@(destructured-variables arguments)))
       (error "This macro is provided as a syntactic aid and is not meant 
               to be used as CL code."))
     (define-expansion ,name ,arguments ,@body)))

;;; some primitives

(define-primitive for% (index from to &body body)
  (indent)
  (princ "for (")
  (out index)
  (princ " in ")
  (out from)
  (princ ":")
  (out to)
  (princ #\))
  (with-braces
    (mapc #'out body)))

(define-primitive normal (mean sd)
  (out-funcall "dnorm" mean `(expt ,sd -2)))

(define-primitive uniform (left right)
  (out-funcall "dunif" left right))

(define-primitive + (&rest arguments)
  (iter
    (for argument :in arguments)
    (unless (first-iteration-p) (princ #\+))
    (with-parens (out argument))))

(define-primitive length (vector)
  (out-funcall "length" vector))

(define-primitive expt (base power)
  (with-parens (out base))
  (princ #\^)
  (with-parens (out power)))

(define-primitive aref (array &rest indexes)
  (out array)
  (iter
    (for index :in indexes)
    (princ (if (first-iteration-p) #\[ #\,))
    (out index))
  (princ #\]))

;;; these primitives will be provided as empty functions for syntactic
;;; convenience

(define-primitive* dim (array)
  (out-funcall "dim" array))

(define-primitive* ~ (expression distribution)
    (with-line
      (out expression)
      (princ " ~ ")
      (out distribution)))

;;; expansions

(define-expansion* each ((array index &optional (dimension 1)) &body body)
  `(for% ,index 1 (aref (dim ,array) ,dimension) ,@body))

(define-expansion* each~ ((array index) distribution
                        &body body)
  `(each (,array ,index)
     (~ (aref ,array ,index) ,distribution)
     ,@body))

(define-expansion* for-index ((index to-or-from &optional (to nil to?)) &body body)
  `(for% ,index ,(if to? to-or-from 1) ,(if to? to to-or-from) ,@body))

;;; model

(defclass model ()
  ((definition :accessor definition :initarg :definition)
   (expansion-table :accessor expansion-table :initarg :expansion-table
                    :initform (make-instance 'expansion-table))))

(defmacro with-new-file ((stream filespec) &body body)
  `(with-open-file (,stream ,filespec :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
     ,@body))

(defun write-model (model filespec)
  "Write model to FILESPEC."
  (bind (((:slots-r/o definition expansion-table) model)
         (*expansion-table* expansion-table)
         (expanded (mapcar #'expand definition)))
    (with-new-file (*standard-output* filespec)
      (princ "model ")
      (with-braces
        (mapc #'out expanded)))))
