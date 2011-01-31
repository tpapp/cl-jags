;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-jags-tests)

(deftestsuite cl-jags-tests () ()
  (:equality-test #'equal))

;; EXTERNAL

(defun run-cl-jags-tests ()
  "Run all the tests for CL-JAGS."
  (run-tests :suite 'cl-jags-tests))
