#lang racket
(define (string-repeat str n)
  (define (helper result i)
    (if (< n 1) ""
    (if (> i n) result
        (helper (string-append result str) (+ i 1)))))
  (helper "" 1)
)

(define (fence n)
  
    (if (= n 0) "{>0<}"
        (string-append "{" (string-repeat "-" (round (+ 1 (log n)))) ">" (~a n) "<" (string-repeat "-" (round (+ 1 (log n)))) "}")
   
))  
