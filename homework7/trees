#lang racket
(define (make-tree node left right)
  (list node left right))

(define (make-leaf node)
  (make-tree node '() '()))

(define (empty-tree? tree)
  (null? tree))

(define (root tree)
  (first tree))

(define (left tree)
  (first (rest tree)))

(define (right tree)
  (first (rest (rest tree))))

(define (count-nodes tree)
  (cond
    [(empty? tree) 0]
    [else (+ 1 (count-nodes (left tree)) (count-nodes (right tree)))]))


(define (height tree)
 (cond  [(empty-tree? tree) 0]
        [else (+ 1 (max (height (left tree)) (height (right tree))))]))

(define t
  (make-tree 10
    (make-tree 4
      (make-leaf 1)
      (make-leaf 6))
    (make-leaf 15)))

(define (leaf? leaf)
  (and (empty? (left leaf)) (empty? (right leaf))))

(define (tree-level level tree)
  (cond [(empty? tree) `()]
        [(= level 1)  (list (root tree))]
        [else (append (tree-level (- level 1) (left tree)) (tree-level (- level 1) (right tree)))]))


(define (tree-levels tree)
  (define (iter level result)
    (cond [(= level 0) result]
          [else (iter (- level 1) (cons (tree-level level tree) result))]))
  (iter (height tree) `()))

(define (tree-map f tree)
  (cond [(leaf? tree) (make-leaf (f (root tree)))]
        [else (cons (f (root tree)) (append  (list (tree-map f (left tree))) (list (tree-map f (right tree)))))]))

(define (bst-insert x tree)
  (cond [(empty-tree? tree) (make-leaf x)]
        [(= x (root tree)) tree]
        [(> x (root tree)) (make-tree (root tree) (left tree) (bst-insert x (right tree)))]
        [else (make-tree (root tree) (bst-insert x (left tree)) (right tree))]))

(define (bst-element? x tree)
  (cond [(empty-tree? tree) #f]
        [(= x (root tree)) #t]
        [(> x (root tree)) (bst-element? x (right tree))]
        [else (bst-element? x (left tree))]))

(define (bst->list tree)
  (cond [(empty? tree) '()]
        [else (append (bst->list (left tree)) (list (root tree)) (bst->list (right tree)))]))

(define (bst? tree)
  (cond [(leaf? tree) #t]
        [(or (> (root tree) (root (right tree))) (< (root tree) (root (left tree)))) #f]
        [else (and (bst? (left tree)) (bst? (right tree)))]))
  
