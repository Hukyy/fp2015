#lang racket
(define (series a b n)
  (define (series-iter a b i)
    (if (= n 1) a
        (if (> i n) b
            (series-iter b (+ a b) (+ i 1)))))
  (series-iter a b 3)
)

(define (fibonacci n)
  (series 1 1 n)
)

(define (lucas n)
  (series 2 1 n)
)

(define (summed-member n)
  (+ (fibonacci n) (lucas n))
)

(define (nth-fibonacci-sum n)
  (define (nth-fib-iter result i)
    (if (> i n) result
        (nth-fib-iter (+ result (fibonacci i)) (+ i 1))))
  (nth-fib-iter 0 1)
)

(define (nth-lucas-sum n)
  (define (nth-luc-iter result i)
    (if (> i n) result
        (nth-luc-iter (+ result (lucas i)) (+ i 1))))
  (nth-luc-iter 0 1)
)

(define (lucas-fib-diff n)
  (- (lucas n ) (fibonacci n))
)
