(define (repeated f n)
  (if (= n 1)
      f
      (lambda(x) (f ((repeated f (- n 1)) x)))))

((repeated square 2) 2)
