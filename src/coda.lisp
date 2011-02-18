;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-jags)

(defun parse-indexes (index-designator &optional (start 0) end)
  "Parse array indexes."
  (mapcar #'parse-integer
          (ppcre:split ",\s*" index-designator :start start :end end)))

;; (parse-indexes "1,2,3")                 ; OK
;; (parse-indexes "   1 , 2, 3   ")        ; OK
;; (parse-indexes "   1 , 2, 3   x")       ; junk
;; (parse-indexes "1")
;; (parse-indexes " 1 ")

(defun parse-node (node-string monitor-varnames)
  "Return (values NAME INDEXES), latter may be NIL."
  (bind (((:values name indexes)
          (aif (position #\[ node-string)
               (let ((end (1- (length node-string))))
                 (assert (char= #\] (aref node-string end)) ()
                         "malformed array index in ~A" node-string)
                 (values (subseq node-string 0 it)
                         (parse-indexes node-string (1+ it) end)))
               node-string))
         (var (gethash name monitor-varnames)))
    (assert var () "Variable ~A was not monitored." name)
    (if indexes
        (cons var (mapcar #'1- indexes))
        var)))

;; (parse-node "MU" (monitor-varnames *session*))
;; (parse-node "MU[1,2,3]" (monitor-varnames *session*))
;; (parse-node "PHI[1,2,3]" (monitor-varnames *session*))

(defstruct (index-line (:constructor make-index-line (node start end)))
  "A parsed line from a CODA index file.  START and END are from 0, in the CL
  convention."
  node start end)

(defun parse-index-line (line monitor-varnames)
  "Parse a line in a CODA index."
  (bind (((node-string start-string end-string)
          (ppcre:split "\\s+" line :sharedp t))
         (start (1- (parse-integer start-string))) ; CL, indexing from 0
         (end (parse-integer end-string)))
    (assert (and (<= 0 start) (< start end)))
    (make-index-line (parse-node node-string monitor-varnames)
                     start end)))

(defun parse-coda-index (filespec monitor-varnames)
  "Parse a CODA index file, checking for contiguity."
  (let* ((index-lines
          (with-open-file (stream filespec :direction :input)
            (iter
              (for line := (read-line stream nil nil nil))
              (while line)
              ;; ?? check for empty lines if that causes problems
              (collecting (parse-index-line line monitor-varnames)
                          :result-type vector))))
         (index-lines (sort index-lines #'< :key #'index-line-start)))
    ;; check contiguity
    (iter
      (for index-line :in-vector index-lines)
      (for start := (index-line-start index-line))
      (for end := (index-line-end index-line))
      (for previous-end :previous end :initially 0)
      (assert (= previous-end start) () "CODA index not contiguous."))
    index-lines))

(defun uniform-n-draws? (index-lines n-draws)
  "Test if number of draws is equal to n-draws in index-lines."
  (bind (((:flet n-draws (index-line))
          (- (index-line-end index-line)
             (index-line-start index-line))))
    (every (lambda (index-line)
             (= (n-draws index-line) n-draws)) index-lines)))
          
(defun read-coda-matrix (filespec matrix n-draws n-nodes)
  "Read a CODA table file into a matrix of DOUBLE-FLOATs."
  (let ((*read-default-float-format* 'double-float))
      (with-open-file (stream filespec :direction :input)
        (dotimes (node-index n-nodes)
          (dotimes (draw-index n-draws)
            (bind ((line (read-line stream))
                   ((:values nil position) (read-from-string line))
                   (value (read-from-string line t nil :start position)))
              ;; (assert (= index (1+ draw-index)))
              (check-type value double-float)
              (setf (aref matrix draw-index node-index) value)))))))
