#lang racket
(define (repeat count what glue)
  (define (iter result newc)
    (cond [(= newc 0) result]
          [else (iter (string-append result glue what) (- newc 1))]))
  (iter what (- count 1)))

(define (repeater str)
  (lambda (count glue)
    (define (iter result newc)
      (cond [(= newc 0) result]
            [else (iter (string-append result glue str) (- newc 1))]))
    (iter str (- count 1))))

(define (prime? n)
   (define (prime-iter i)
    (cond [(>= i n) #t]
          [(= (remainder n i) 0) #f]
          [else (prime-iter (+ i 1))])
  )
 (if (= n 1) #f (prime-iter 2)))  

(define (truncatable-prime? x)
  (cond [(and (< x 10) (prime? x)) #t]
        [(not (prime? x)) #f]
        [else (truncatable-prime? (quotient x 10))]))

(define (where list-elements list-predicates)
  (define (iter result curelem curpred)
    (cond [(empty? curelem) result]
          [(empty? curpred) (iter (cons (first curelem) result) (rest curelem) list-predicates)]
          [(not ((first curpred) (first curelem))) (iter result (rest curelem) list-predicates)]
          [else (iter result curelem (rest curpred))]))
   (reverse (iter '() list-elements list-predicates)))

;(define (zero matrix)
  ;(define (findpos pos result mat)
   ; (cond [(empty? mat) result]
       ;   [(empty? (first mat)) (findpos 0 result (rest mat))]
        ;  [(= 0 (first (first mat))) (findpos (+ 1 pos) (cons pos result) (rest (
         ; [(empty? (first mat)) (findpos 0 result (rest mat))]
         ; [(= 0 (caar mat)) (findpos (+ 1 pos) (cons pos result) (rest mat))]
         ; [else (findpos (+ 1 pos) result (rest mat))])
   ; (findpos 0 '() matrix)))
