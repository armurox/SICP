(define (sum start end term increment)
  (define (iter s e t i result)
    (if (> s e)
	result
	(iter (increment s) e t i (+ result (term s)))))
  (iter start end term increment 0))

(define (sum-int a b)
  (sum a b (lambda(x) x) 1+))

(sum-int 1 5)

(define (pi-sum a b)
  (sum a b (lambda(i) (/ 1 (* i (+ i 2)))) (lambda(x) (+ x 4))))

(pi-sum 1 300)
