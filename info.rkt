#lang info
(define collection "memo")
(define deps '("base"
               "finalizer" "nested-hash"))
(define build-deps '("sandbox-lib" "scribble-lib" "racket-doc" "rackunit-lib" "thread-utils"))
(define scribblings '(("scribblings/memo.scrbl" ())))
(define pkg-desc "Memoization with finalizers and cleanup")
(define version "0.1.0")
(define pkg-authors '("Kevin Robert Stravers"))
