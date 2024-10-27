(define (+ a b)
  (if (= b 0)
      a
      (+ (1+ a) (-1+ b)))
  )

(+ 2 4)

(define (rec-+ a b)
  (if (= b 0)
      a
      (1+ (rec-+ a (-1+ b)))))

(rec-+ 1 2) ;; -> 3

(define (fib n)
  (cond ((< n 1) 0)
	((= n 1) 1)
	((> n 1) (+ (fib (- n 1)) (fib (- n 2))))))

(fib 6) ;; -> 8

(define (fib-1 n)
  (define (iter curr prev count)
    (if (= count n)
	curr
	(iter (+ prev curr) curr (+ count 1))))
  (iter 1 0 0))

(fib-1 5)

(define (gcd a b)
  (let ((r (remainder a b)))
  (if (= r 0)
      b
      (gcd b r))))

(gcd 119 544) ;; -> 17

(define (sos a b)
  (define (square c)
    (* c c))
  (+ (square a) (square b)))

(sos 2 3) ;; -> 13

(define (rec-toh n)
  (if (= n 1)
      1
  (1+ (* 2 (rec-toh (-1+ n))))))

(rec-toh 3) ;; -> 7

(define (form-toh n)
  (-1+ (expt 2 n)))

(form-toh 3)

(define (move from to spare n)
  (if (= n 0) "done"
      ((move (-1+ n) from spare to)
       (print-move 1)
       (move (-1+ n) space to from))))


