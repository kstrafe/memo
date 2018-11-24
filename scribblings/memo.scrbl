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
           (memoize (param ...+) #:hash hsh body ...+)
           (memoize (param ...+) #:finalize finalizer body ...+))]{
  Creates a memoized @racket[lambda]. It is the @racket[lambda] that holds the memoized values internally. If the @racket[lambda] goes out of scope, then so do the associated memoized values.
  @racket[#:hash] specifies a hash table to use for the memoized data. The default is @racket[hasheq].
  A finalizer can be specified using the @racket[#:finalize] keyword after the parameter list. The finalizer runs whenever a value has been removed from the cache, but no guarantee is made as to when this will happen as finalization depends on the racket garbage collector.
}

@defform*[((define/memoize (name param ...+) body ...+)
           (define/memoize (name param ...+) #:hash hsh #:finalize finalizer body ...+))]{
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
  Memoize-zero has no finalizer.
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

Sometimes we wish to write partially memoized functions, for instance, when we compute a side-effect and we want to cache some important result before doing the side-effect. A good use-case is OpenGL, where we may need to generate a texture or load a @racket[glProgram].


@defform*[((memoize-partial (memoized-param ...)
                            (live-param ...)
                            #:hash hsh
                            #:finalize finalizer
                            (memoized-body ...)
                            (live-body ...+)))]{
  Creates a memoized function that memoizes @racket[memoized-body] using @racket[memoized-param], but will apply the remaining
  arguments to the @racket[live-body]. Similarly to other memoizations, one can use empty arguments to get the cached table.
  If @racket[live-param] is empty, calling the memoized function with just the @racket[memoized-param] will run the @racket[live-body]. Otherwise, it will return a function taking @racket[live-param].
  If @racket[memoized-param] is empty, then the function will run the memoized body once during the first invocation.
}

@defform*[((define/memoize-partial name
                                   (memoized-param ...)
                                   (live-param ...)
                                   #:hash hsh
                                   #:finalize finalizer
                                   (memoized-body ...)
                                   (live-body ...+)))]{
  Same as @racket[memoize-partial] but defines the name.
}

@examples[#:eval evaluator
  (define/memoize-partial partial (x y) (a)
    ((writeln "Runs once for each unique x and y")
     (define N (+ x y)))
    ((* N a)))
  (partial 1 2 3)
  (partial)
  (partial 1 2 4)

  (partial 0 0 3)
  (partial)
  (partial 0 0 0)
]

@examples[#:eval evaluator
  (define/memoize-partial f (x y) ()
    ((writeln "Runs once for each unique x and y")
     (define N (+ x y)))
    ((* N 10)))
  (f 1 2)
  (f)
  (f 1 2)
]

If @racket[hash] is desired, it can be specified:
@examples[#:eval evaluator
  (define/memoize (memoize-with-hash a str) #:hash hash
    str)
  (memoize-with-hash 1 "A string")
  (memoize-with-hash 1 (string-append "A " "string"))

  ; The table only contains one entry
  (memoize-with-hash)
]
