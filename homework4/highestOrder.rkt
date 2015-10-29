#lang racket

(define (f p g h)
  (lambda (x) (and (p (g x)) (p (h x)))))

(define (inc x) (+ x 1))

(define square (lambda (x) (* x x)))

(define (prime? n)
  (define (prime-iter i)
    (cond [(> i (sqrt n)) #t]
          [(= (remainder n i) 0) #f]
          [else (prime-iter (+ i 1))])
  )
  (if (= n 1) #f
       (prime-iter 2)))
