#lang racket

(require data/functor)
(require data/maybe)
(require data/either)
(require data/monad)
(require data/applicative)

(define (id x) x)

(define (incr n)
  (if (< n 10) (success (+ n 1)) (failure "too big!")))

(chain incr (chain incr (success 9)))

(do
  (define x 8)
  [y <- (incr x)]
  [z <- (incr y)]
  (pure z))


(map
  (lambda (x) (+ x 1))
  (success 2))

(map
  (lambda (x) (+ x 1))
  (failure "ize"))

(id 12)

(+ 
  1
  2)

; Regular recursive fib
;; (define (fib n)
;;   (cond
;;     [(eq? n 1) 1]
;;     [(eq? n 2) 1]
;;     [else (+ (fib (- n 1)) (fib (- n 2)))]
;;   )
;; )

; Tail-recursive fib
(define (fib n)
  (define (go n acc1 acc2) 
    (cond [(eq? n 1) acc1]
      [(eq? n 2) acc2]
      [else (go (- n 1) acc2 (+ acc1 acc2))])
    )
  (go n 1 1))


(map fib (range 1 9))
