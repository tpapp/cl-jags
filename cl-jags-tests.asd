;;;; cl-jags.asd

(asdf:defsystem #:cl-jags-tests
  :description "Unit tests for CL-JAGS."
  :author "Tamas K Papp <tkpapp@gmail.com>"
  :license "Same as CL-JAGS -- this is part of the latter."
  :serial t
  :depends-on (#:lift
               #:iterate
               ;; #:alexandria
               #:metabang-bind
               #:anaphora)
  :components 
  ((:module
    "package-init"
    :pathname #P"tests/"
    :components
    ((:file "package")))
   (:module
    "setup"
    :pathname #P"tests/"
    :components
    ((:file "setup")))
   (:module
    "tests"
    :pathname #P"tests/"
    :components
    ((:file "test-dump")))))
