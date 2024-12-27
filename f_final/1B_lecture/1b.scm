(define (repeated f n)
  (if (< n 2)
      f
      (lambda(x) (f ((repeated f (- n 1)) x)))))

((repeated square 2) 2)

(define (sum start end term increment)
  (define (iter s e t i result)
    (if (> s e)
	result
	(iter (i s) e t i (+ result (term s)))))
  (iter start end term increment 0))

(sum 2 4 square 1+)

(define (rec-sum start end term increment)
  (if (> start end)
      0
      (+ (term start) (rec-sum (increment start) end term increment))))

(rec-sum 2 4 square 1+)

(define (sos x y)
  (+ (square x) (square y)))

(sos 2 3)

(define (toh n)
  (if (< n 1)
      0
      (+ (* 2 (toh (- n 1))) 1)))


