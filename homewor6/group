#lang racket
(define (take-while p items)
  (cond [(empty? items) '()]
        [(not (p (first items))) `()]
        [else (cons (first items) (take-while p (rest items)))]))

(define (drop-while p items)
  (cond [(empty? items) `()]
        [(not (p (first items))) items]
        [else (drop-while p (rest items))]))
;;/////////////////////////////////////////////////////////////////////////////////

(define (group lst)
  (cond [(empty? lst) '()]
        [else (cons (take-while (lambda (x) (equal? x (first lst))) lst)
                    (group (drop-while (lambda (x) (equal? x (first lst))) lst)))]))

