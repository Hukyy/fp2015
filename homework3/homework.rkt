#lang racket
(require "fence.rkt")
(require "binary.rkt")

(define (nth-beast-number n)
  (define (iter result i)
    (cond [(= i 0) result]
          [else (iter (string-append result "666") (- i 1))]
          ))
    (string->number (iter "" n)))

(define (palindrome? n)
  (= n (string->number (string-reverse (number->string n)))))

(define (occurrences a n)
  (define (occ-iter n result)
    (cond [(= n 0) result]
          [(= (remainder n 10) a) (occ-iter (quotient n 10) (+ result 1))]
          [else (occ-iter (quotient n 10) result)]))
  (occ-iter n 0))

(define (next-hack n)
  (define (hack-iter i)
    (cond [(and (odd? (occurrences 1 (string->number (to-binary-string i)))) (palindrome? (string->number (to-binary-string i)))) i]
          [else (hack-iter (+ i 1))]))
  (hack-iter (+ n 1)))

(define (p-score n)
    (cond [(palindrome? n) 1]
          [else (+ 1 (p-score (+ n (string->number (string-reverse(number->string n))))))]))
