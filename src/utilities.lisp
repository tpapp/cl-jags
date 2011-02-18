;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-jags)

(defun destructured-variables (macro-lambda-list)
    "List all bound variables in a macro lambda list, assuming that the latter
is syntactically correct."
  (let (variables
        key-or-optional?
        (special-keywords '((&optional t)
                            (&rest nil)
                            (&key t)
                            (&allow-other-keys)
                            (&aux)
                            (&whole)
                            (&body)
                            (&environment))))
    (flet ((process (var)
             (acond
               ((find var special-keywords :key #'first)
                (awhen (second it)      ; switch mode when applicable
                  (setf key-or-optional? it)))
               ((listp var)
                (if key-or-optional?
                    (progn
                      (push (first var) variables)
                      (awhen (third var) ; supplied-p
                        (push it variables)))
                    (appendf variables (destructured-variables var))))
               (t (push var variables)))))
      (let ((last (last macro-lambda-list)))
        (aif (cdr last)                 ; dotted list
             (progn
               (push it variables)
               (mapc #'process (butlast macro-lambda-list)))
             (mapc #'process macro-lambda-list)))
      variables)))
