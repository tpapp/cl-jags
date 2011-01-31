;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-jags-tests)

(deftestsuite dump-tests (cl-jags-tests) ()
  (:equality-test #'equal))

(defun dump* (object)
  "Dump OBJECT to a string.  For testing."
  (with-output-to-string (stream)
    (dump stream object)))

(addtest (dump-tests)
  dump-number
  (ensure-same (dump* 1) "1")
  (ensure-same (dump* 3.14) "3.14e+0")
  (ensure-same (dump* 1/2) "5.e-1"))

(addtest (dump-tests)
  dump-vector
  (ensure-same (dump* #(1 2 3)) "c(1,2,3)"))

(addtest (dump-tests)
  dump-array
  (ensure-same (dump* #2A((1 2 3) (4 5 6)))
               "structure(c(1,4,2,5,3,6), .Dim=c(2,3))")
  (ensure-same (dump* #3A(((1) (2))
                          ((3) (4))
                          ((5) (6))))
               "structure(c(1,3,5,2,4,6), .Dim=c(3,2,1))"))

(addtest (dump-tests)
  named-dump
  (ensure-same (with-output-to-string (stream)
                 (dump-named stream 2 "x"))
               "`x` <-
2
"))
