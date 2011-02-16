;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-jags)

(defun destructured-variables (lambda-list)
    "List all variables in lambda-list."
    ;; !! includes INIT-FORMs if they are symbols.  This is good enough for
    ;; !! creating an IGNORABLE declaration, but not for other applications.
    (remove-if (lambda (var)
                 (typep var '(or null
                              (not symbol)
                              (member &body &rest &key &optional))))
               (flatten lambda-list)))
