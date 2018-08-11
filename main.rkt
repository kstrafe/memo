#lang racket/base

(provide define/memoize memoize)

(require syntax/parse/define
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
               (let ([value (begin body ...)])
                 (set-box! cache (nested-hash-set (unbox cache) param ... value))
                 (register-finalizer value fin)
                 value)))]))))


(module+ test
  (require racket/function racket/port
           rackunit thread-utils)
  (define/memoize (fib/memo       n) (if (< n 2) 1 (+ (fib/memo (sub1 n))       (fib/memo (- n 2)))))
  (define         (fib            n) (if (< n 2) 1 (+ (fib (sub1 n))            (fib (- n 2)))))
  (define         flag #f)
  (define/memoize (fib/memo/final n) #:finalize (lambda x (set! flag #t))
                                     (if (< n 2) 1 (+ (fib/memo/final (sub1 n)) (fib/memo/final (- n 2)))))

  (test-case "speed"
    (check-false     (until-timeout (thunk (fib      1000)) 1 (const #f)))
    (check-not-false (until-timeout (thunk (fib/memo 1000)) 1 (const #f))))
  (test-case "cache reset"
    (check-equal? (hash-count (unbox (fib/memo))) 1001)  ; Contains keys 0 - 1000
    (set-box! (fib/memo) (hasheq))
    (check-equal? (hash-count (unbox (fib/memo))) 0))
  )
