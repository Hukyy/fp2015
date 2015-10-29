#lang racket
(define (string-reverse str)
  (define (helper result i)
    (cond
      [(= (string-length str) 0) ""]
      [(= i 0) (string-append result (~a (string-ref str 0)))]
      [else (helper (string-append result (~a (string-ref str i))) (- i 1))]
     )
   )
  (helper "" (- (string-length str) 1))
)


(define (to-binary-string n) 
 (define (dec->bin n)
  (cond
    [(=  n 0) 0]
    [else (+ (* 10 (dec->bin (quotient n 2))) (remainder n 2))]))
 (~a (dec->bin n)))


(define (from-binary-string binary-str)
  (define (iter newN result i)
    (cond
      [(= newN 0) result]
      [else (iter (quotient newN 10) (+ result (* (remainder newN 10) (expt 2 i))) (+ i 1))]))
  (iter (string->number binary-str) 0 0))
   
