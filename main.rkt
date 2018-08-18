#lang racket/base

(provide define/memoize         memoize
         define/memoize-zero    memoize-zero
         define/memoize-partial memoize-partial)

(require racket/function syntax/parse/define
         (for-syntax racket/base racket/list)
         finalizer nested-hash)

(define-syntax-parser define/memoize
  ([_ (name:id formals:id ...+)
      (~optional (~seq #:finalize fin:expr) #:defaults [(fin #'(lambda x x))])
      body:expr ...+]
   #'(define name (memoize (formals ...) #:finalize fin body ...))))

(define-syntax-parser memoize
  ([_ (param:id ...)
      (~optional (~seq #:finalize fin:expr) #:defaults [(fin #'(lambda x x))])
      body:expr ...+]
   #'(let ([cache (box (hasheq))])
       (case-lambda
         [() cache]
         [(param ...)
           (let ([result (nested-hash-ref (unbox cache) param ... #:default #f)])
             (if result
               result
               (let ([value ((lambda () body ...))])
                 (set-box! cache (nested-hash-set (unbox cache) param ... value))
                 (register-finalizer value fin)
                 value)))]))))

(define-syntax-parser define/memoize-zero
  ([_ name:id body:expr ...+]
   #'(define name (memoize-zero body ...))))

(define-syntax-parser memoize-zero
  ([_ body:expr ...+]
   #'(let ([cache (box (void))] [first-time? (box #t)])
       (case-lambda
         [() (if (unbox first-time?)
               (let ([result ((lambda () body ...))])
                 (set-box! cache result)
                 (set-box! first-time? #f)
                 result)
               (unbox cache))]
         [(dummy)
           (values cache first-time?)]))))

(define-syntax-parser define/memoize-partial
  ([_ name:id
      (param:id ...+)
      (further:id ...)
      (~optional (~seq #:finalize fin:expr) #:defaults [(fin #'(lambda x x))])
      (body:expr ...+)
      (every:expr ...+)]
   #'(define name (memoize-partial (param ...) (further ...) (body ...) (every ...)))))

(define-syntax-parser memoize-partial
  ([_ (param:id ...+)
      ()
      (~optional (~seq #:finalize fin:expr) #:defaults [(fin #'(lambda x x))])
      (body:expr ...)
      (every:expr ...+)]
   #'(let ([memo (memoize (param ...) #:finalize fin
                   body ...
                   (lambda ()
                     every ...))])
         (case-lambda
           [()                      (memo)]
           [(param ...)             ((memo param ...))])
       ))
  ([_ (param:id ...+)
      (further:id ...)
      (~optional (~seq #:finalize fin:expr) #:defaults [(fin #'(lambda x x))])
      (body:expr ...)
      (every:expr ...+)]
   #'(let ([memo (memoize (param ...) #:finalize fin
                   body ...
                   (lambda (further ...)
                     every ...))])
         (case-lambda
           [()                      (memo)]
           [(param ...)             (curry memo param ...)]
           [(param ... further ...) ((memo param ...) further ...)])
       )))


(module+ test
  (require racket/function racket/port
           rackunit thread-utils)
  (define/memoize (fib/memo       n) (if (< n 2) 1 (+ (fib/memo (sub1 n))       (fib/memo (- n 2)))))
  (define         (fib            n) (if (< n 2) 1 (+ (fib (sub1 n))            (fib (- n 2)))))
  (define         flag #f)
  (define/memoize (fib/memo/final n) #:finalize (lambda x (set! flag #t))
                                     (if (< n 2) 1 (+ (fib/memo/final (sub1 n)) (fib/memo/final (- n 2)))))
  (define/memoize-zero example 1)
  (define/memoize-partial partial (a b) (x y)
    ((define N (+ a b)))
    ((+ x y N)))

  (define partial*
    (memoize-partial (x y) (w)
      ((define N (- x y)))
      ((+ w N))))

  (test-case "speed"
    (check-false     (until-timeout (thunk (fib      1000)) 1 (const #f)))
    (check-not-false (until-timeout (thunk (fib/memo 1000)) 1 (const #f))))
  (test-case "cache reset"
    (check-equal? (hash-count (unbox (fib/memo))) 1001)  ; Contains keys 0 - 1000
    (set-box! (fib/memo) (hasheq))
    (check-equal? (hash-count (unbox (fib/memo))) 0))
  (test-case "memoize-zero"
    (check-equal? (with-output-to-string (lambda ()
                                         (define x (memoize-zero (display "test")))
                                         (x)
                                         (x)
                                         (x)))
                  "test"))
  (test-case "memoize-zero-cache"
    (define-values (cache first-time?) (example 'get-cache))
    (check-equal? (unbox cache)        (void))
    (check-equal? (unbox first-time?)  #t)
    (example)
    (check-not-equal? (unbox cache)        (void))
    (check-equal?     (unbox first-time?)  #f))
  (test-case "memoize-partial"
    (check-equal? (partial 1 2 3 4) 10)
    (check-equal? (partial* 1 2 3) 2)
    (check-equal? ((partial 1 2) 3 4) 10)))
