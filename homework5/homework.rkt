#lang racket
(define (sum numbers)
  (cond [(empty? numbers) 0]
        [else (+ (first numbers) (sum (rest numbers)))]))

(define (member? x items)
  (cond [(empty? items) #f]
        [(equal? x (first items)) #t]
        [else (member? x (rest items))]))

(define (length2 items)
  (cond [(empty? items) 0]
        [else (+ 1 (length2 (rest items)))]))

(define (list-ref2 items n)
  (define (iter pos xs)
    (cond [(empty? xs) "Position not found"]
          [(= n pos) (first xs)]
          [else (iter (+ 1 pos) (rest xs))]))
  (iter 0 items))

(define (range2 a b)
  (cond [(= a b) `()]
        [else (cons a (range (+ 1 a) b))]))

(define (build-list2 n f)
  (map f (range2 0 n)))

(define (append2 l1 l2)
  (cond [(empty? l1) l2]
        [else (cons (first l1) (append2 (rest l1) l2))]))

(define (reverse2 items)
  (cond [(empty? items) `()]
        [else  (append2 (reverse2 (rest items))  (list (first items)))]))

(define (take2 n items)
  (define (iter cur xs)
    (cond [(empty? xs) `()]
          [(= n cur) `()]
          [else (cons (first xs) (iter (+ 1 cur ) (rest xs)))]))
  (iter 0 items))

(define (drop2 n items)
  (cond [(= n 0) items]
        [else (drop2 (- n 1) (rest items))]))

(define (take-while p items)
  (cond [(not (p (first items))) `()]
        [else (cons (first items) (take-while p (rest items)))]))

(define (drop-while p items)
  (cond [(empty? items) `()]
        [(not (p (first items))) items]
        [else (drop-while p (rest items))]))

(define (number->list n)
  (define (iter acc newn)
    (cond [(zero? newn) acc]
          [else (append2 (iter acc (quotient newn 10)) (list (remainder newn 10)))]))
  (if (= n 0) (list 0)
      (iter `() n)))



(define (list->number xs)
  (define (iter cur x)
  (cond [(empty? x) cur]
        [else (iter (+ (* 10 cur) (first x)) (rest x))]))
  (iter 0 xs))
        
  

