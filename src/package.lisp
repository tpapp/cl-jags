;;;; package.lisp

(defpackage #:cl-jags
  (:use #:cl #:iterate #:metabang-bind #:anaphora #:alexandria #:cl-random
        #:cl-num-utils)
  (:shadowing-import-from #:rv #:mean #:sd #:variance)
  (:nicknames #:jags)
  (:export
   
   ;; utilities -- nothing is exported
   
   ;; dump

   #:dump #:dump-named

   ;; model -- nothing is exported

   ;; coda -- nothing is exported

   ;; session

   #:*jags-executable* #:*minimum-burn-in*  #:session #:prepare-session 
   #:run-jags #:posterior-matrix #:posterior-matrix #:run-session

   ))
