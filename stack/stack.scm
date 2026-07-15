(use-modules (ice-9 match))

;; Basics
(define (empty) '())

(define (push stack elem)
  (cons elem stack))

(define (pop stack)
  (cons (cdr stack) (car stack)))

(define (peek stack)
  (car stack))

;; Arithmetics
(define (operate stack op)
  (let* ((s1 (pop stack))
         (s2 (pop (car s1)))
         (n1 (cdr s1))
         (n2 (cdr s2)))
    (push (car s2) (op n1 n2))))

(define (mul stack)
  (operate stack *))

(define (add stack)
  (operate stack +))

(define (sub stack)
  (operate stack -))

(define ex1 (sub (push (add (push (push (empty) 3) 3)) 8)))
