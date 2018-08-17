#lang scribble/manual
@require[@for-label[memo
                    racket/base]
          racket/sandbox
          scribble/example]

@(define evaluator (parameterize ([sandbox-output 'string]
                                  [sandbox-error-output 'string]
                                  [sandbox-memory-limit 500])
                     (make-evaluator 'racket '(require memo))))

@title{memo: Memoization with cache and finalization control}
@author{Kevin Robert Stravers}

@defmodule[memo]

This package provides macros for defining memoized functions. A memoized function stores its results in a @racket[hasheq] table. Multiple arguments invoke nested @racket[hasheq] tables.

It also provides manners to finalize or destroy memoized values.

@defform*[((memoize (param ...+) body ...+)
           (memoize (param ...+) #:finalize finalizer body ...+))]{
  Creates a memoized @racket[lambda]. It is the @racket[lambda] that holds the memoized values internally. If the @racket[lambda] goes out of scope, then so do the associated memoized values.
  A finalizer can be specified using the @racket[#:finalize] keyword after the parameter list. The finalizer runs whenever a value has been removed from the cache, but no guarantee is made as to when this will happen as finalization depends on the racket garbage collector.
}

@defform*[((define/memoize (name param ...+) body ...+)
           (define/memoize (name param ...+) #:finalize finalizer body ...+))]{
  Same as @racket[memoize] but defines a @racket[name] in the surrounding scope.
}

@examples[#:eval evaluator
  (define/memoize (fib n)
    (if (< n 2)
      1
      (+ (fib (sub1 n)) (fib (- n 2)))))
  (fib 100)
]

Accessing the cache is done by calling the function without any arguments. So it can be reset by doing the following:

@examples[#:eval evaluator #:label #f
  (set-box! (fib) (hasheq))
]

One can also simply remove the desired entries inside @racket[(fib)], and then use @racket[set-box!] to store it back to the function. Finalization occurs if a finalizer is specified and the GC happens to collect your removed value.

For multiple arguments the @racket[hash] becomes nested with respect to the parameters:

@examples[#:eval evaluator
(define/memoize (f a b c) (+ a b c))
(f 1 2 3)
(f)
]

@defform*[((memoize-zero body ...+))]{
  Creates a memoized @racket[lambda] that takes zero arguments. It runs the body once and only once when called for the first time. To access the content of the cache and first-time flag, give the function one argument.
}

@defform*[((define/memoize-zero name body ...+))]{
  Same as @racket[memoize-zero] but defines the name.
}

@examples[#:eval evaluator
  (define/memoize-zero example
    (writeln "This runs once and only once")
    'value)
  (example)
  (example)
]

Access to the zero version is granted by providing a dummy argument. Here we use @racket['get-cache] for clarity.

@examples[#:eval evaluator
  (define/memoize-zero example
    (writeln "This runs once and only once")
    'value)
  (example 'get-cache)
  (example)
  (example 'get-cache)
]

Two values are returned; the cache itself (inside a @racket[box]), as well as the @racket[first-time?] flag, also in a @racket[box]. This flag indicates whether or not the cache should computed.
