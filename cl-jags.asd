;;;; cl-jags.asd

(asdf:defsystem #:cl-jags
  :description "Common Lisp interface for JAGS."
  :author "Tamas K Papp <tkpapp@gmail.com>"
  :license "Boost"
  :serial t
  :depends-on (#:iterate
               #:alexandria
               #:metabang-bind
               #:anaphora
               #:cl-num-utils
               #:cl-random
               #:cl-fad
               #:cl-ppcre
               #:random-sample
               #:trivial-shell
               #:cl-num-utils)
  :components
  ((:module 
    "package-init"
    :pathname #P"src/"
    :components
    ((:file "package")))
   (:module 
    "main"
    :pathname #P"src/"
    :serial t
    :components
    ((:file "utilities")
     (:file "dump")
     (:file "model")
     (:file "coda")
     (:file "session")))))
