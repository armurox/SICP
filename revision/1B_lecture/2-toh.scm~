(define (toh n)
  (if (= n 1)
      n
      (+ (* 2 (toh (- n 1))) 1)))

(toh 4)

(define (toh-1 n)
  (- (expt 2 n) 1))

(toh 4)

(define (fib n)
  (define (help a b d)
    (if (= d n)
	b
	(help (+ a b) a (+ 1 d))))
  (help 1 0 0))

(fib 10)

(define (fib-1 n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
	 (fib (- n 2)))))

(fib 1)
