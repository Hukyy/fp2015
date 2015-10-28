(define (product-digit n)
  (if (< n 10) n
      (* (remainder n 10) (product-digit (quotient n 10)))))
