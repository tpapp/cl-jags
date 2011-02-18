;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-jags)

(defparameter *jags-executable* "jags"
  "JAGS executable, with or without path, will be called by a shell.")

(defparameter *minimum-burn-in* 1000
  "Minimum BURN-IN when not given.")

(defclass session ()
  ((prefix :accessor prefix
           :documentation "prependend to file names"
           :initform "")
   (dir :accessor dir :initarg :dir :documentation "working directory")
   (script-filename :accessor script-filename :initarg :script-filename
                    :initform "jags.cmd")
   (model :accessor model :initarg :model)
   (model-filename :accessor model-filename :initarg :model-filename
                  :initform "model.bug")
   (data :accessor data :initarg :data)
   (data-filename :accessor data-filename :initarg :data-filename
                  :initform "data.R")
   (monitors :accessor monitors :initarg :monitors :initform nil)
   (monitor-varnames :accessor monitor-varnames :documentation
                     "A hash table with R/JAGS variable names for monitors.")
   (coda-stem :accessor coda-stem :initarg :coda-stem
              :initform "CODA")   
   (modules :accessor modules :initarg :modules :initform nil
            :documentation "list of modules to load (strings)")
   (n-chains :accessor n-chains :initarg :n-chains :initform 1
             :documentation "number of chains")
   (burn-in :accessor burn-in :initarg :burn-in
            :documentation "burn-in period, calculated from N-DRAWS if not given")
   (n-draws :accessor n-draws :initarg :n-draws :initform 1000
            :documentation "number of draws (to keep)")
   (posterior-draws :accessor posterior-draws
                    :documentation "Matrix with posterior draws, stacked by
                    chains.")
   (node-table :accessor node-table
               :documentation "Node->index mapping for posterior draws.")))

(defmethod initialize-instance :after ((session session) &key &allow-other-keys)
  (assert (cl-fad:directory-pathname-p (dir session)))
  ;; set up mapping for monitors
  (assert (monitors session) () "No monitors specified.")
  (iter
    (with table := (make-hash-table :test #'equal))
    (for monitor :in (monitors session))
    (setf (gethash (var-as-string monitor) table) monitor)
    (finally
     (setf (monitor-varnames session) table))))

(defmethod slot-unbound (class (session session) (slot-name (eql 'burn-in)))
  (max *minimum-burn-in* (n-draws session)))

(defun session-pathname (session component &rest rest)
  "Generate pathnames for various filenames (components) using the given
session.  See code for exact specifications.  All other functions should use
this one for resolving component names."
  (with-slots (coda-stem) session
    (merge-pathnames 
     (concatenate 'string (prefix session)
                  (ecase component
                    (model (model-filename session))
                    (data (data-filename session))
                    (script (script-filename session))
                    (coda-index (format nil "~Aindex.txt" coda-stem))
                    (coda-chain (bind (((chain-index) rest))
                                  (check-type chain-index (integer 0))
                                  (format nil "~Achain~D.txt" 
                                          coda-stem (1+ chain-index))))))
     (dir session))))

(defun write-data (data filespec)
  "Write data for session."
  (with-new-file (stream filespec)
    (iter
      (for (name value) :in data)
      (dump-named stream name value))))

(defun write-script (session)
  "Write script for session."
  (with-new-file (*standard-output* (session-pathname session 'script))
    (flet ((command (control-string &rest format-arguments)
             (apply #'format *standard-output* control-string format-arguments)
             (terpri)))
      (iter
        (for module :in (modules session))
        (command "load ~A~%" (aetypecase module
                                (string it)
                                (symbol (var-as-string it)))))
      (command "cd \"~a\"" (namestring (dir session)))
      (command "model in \"~a\"" (model-filename session))
      (command "data in \"~a\"" (data-filename session))
      (command "compile, nchains(~a)" (n-chains session))
      ;; !! read init parameters, eg 'parameters in "line-inits.R"'
      (command  "initialize")
      (command "update ~A" (burn-in session))
      (iter
        (for (nil varname) :in-hashtable (monitor-varnames session))
        ;; !! add thinning, also parameter to SESSION class
        (command "monitor ~A" varname))
      (command "update ~A" (n-draws session))
      (command "coda *, stem(\"~A\")" (concatenate 'string (prefix session)
                                                   (coda-stem session)))
      (command "exit"))))

(defun prepare-session (session)
  "Prepare session for running JAGS by writing out all files."
  (bind (((:slots-r/o data model) session)
         ((:flet path (component)) (session-pathname session component)))
    (write-data data (path 'data))
    (write-model model (path 'model))
    (write-script session)))

(defun run-jags (session &key (jags-executable *jags-executable*))
  "Run JAGS on SESSION, using JAGS-EXECUTABLE."
  (bind (((:values output error status)
          (trivial-shell:shell-command
           (format nil "~A ~A" jags-executable
                   (namestring (session-pathname session 'script))))))
    (unless (zerop status)
      (error "JAGS failed with output:~%~A~%error message:~%~A" output error))))

(defun posterior-matrix (session &optional chain-index)
  "Return a matrix for the given chain index, or all chains pooled (if
applicable).  When CHAIN-INDEX is ARRAY, return a rank-3 array, indexed by the
chains along its first dimension."
  (bind (((:slots-r/o posterior-draws n-draws n-chains) session)
         (n-nodes (array-dimension posterior-draws 1)))
    (etypecase chain-index
      ((integer 0)
         (cl-num-utils:displace-array posterior-draws (list n-draws n-nodes)
                                      (* chain-index n-draws n-nodes)))
      (null posterior-draws)
      ((eql 'array)
         (cl-num-utils:displace-array posterior-draws
                                      (list n-chains n-draws n-nodes))))))

(defun read-posterior-draws (session)
  "Create matrix for posterior draws and read them."
  (bind ((index-lines (parse-coda-index (session-pathname session 'coda-index) 
                                        (monitor-varnames session)))
         ((:slots n-draws node-table posterior-draws n-chains) session)
         (n-nodes (length index-lines)))
    (assert (uniform-n-draws? index-lines n-draws))
    (setf node-table (make-hashed-index (map 'vector 
                                             #'index-line-node index-lines))
          posterior-draws (make-array (list (* n-chains n-draws) n-nodes)
                                      :element-type 'double-float))
    (dotimes (chain-index n-chains)
      (read-coda-matrix (session-pathname session 'coda-chain chain-index)
                        (posterior-matrix session chain-index)
                        n-draws n-nodes))))

(defun run-session (session)
  "Run session: write files, run JAGS, read posterior draws."
  (prepare-session session)
  (run-jags session)
  (read-posterior-draws session)
  session)

