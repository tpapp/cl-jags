;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-jags)

(defparameter *mu* 2d0)
(defparameter *sigma* 3d0)
(defparameter *y* (rs:replicate 1000 (normal *mu* *sigma*)))

(defparameter *data* `((y ,*y*)))

(defparameter *session* (make-instance 'session :model *model* :data *data* :dir #P"/tmp/"
                                       :monitors '(MU SIGMA) :n-chains 3
                                       :n-draws 10 :prefix "simple_"))

(run-session *session*)

(cl-num-utils:map-columns #'alexandria:mean (posterior-matrix *session*))

