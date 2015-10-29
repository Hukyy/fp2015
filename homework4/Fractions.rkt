#lang racket
(define (fst pair)
  (car pair))

(define (snd pair)
  (cdr pair))

(define a (cons 1 0))
(define b (cons 66 3))

(define (this-is-private-function f frac1 frac2)
(define result (cons (f (* (fst frac1) (snd frac2)) (* (snd frac1) (fst frac2))) (* (snd frac1) (snd frac2))))
  result)


(define (add-frac frac1 frac2)
  (define result (this-is-private-function + frac1 frac2))
 ( if (= (snd result) 0) "Undefined"
  result))

(define (subtract-frac frac1 frac2)
  (define result (this-is-private-function - frac1 frac2))
  (if (= (snd result) 0) "Undefined"
  result))


(define (mult-frac frac1 frac2)
   (define result (cons (* (fst frac1) (fst frac2)) (* (snd frac1) (snd frac2))))
  (if (= (snd result) 0) "Undefined"
  result))

(define (simplify-frac frac)
  (define divisor (gcd (fst frac) (snd frac)))
  (cond [(= (fst frac) 0) (define result (cons 0 1)) result]
        [(= (snd frac) 0) (define result (cons 0 1)) result] ;;thinking of "Undefined" but it's probably not a good idea
        [(and (< (fst frac) 0) (< (snd frac) 0)) (define result (cons (* (/ (fst frac) divisor) -1) (* (/ (snd frac) divisor)  -1))) result]
        [else (define result (cons (/ (fst frac) divisor) (/ (snd frac) divisor)))result])
  
  )
                          
