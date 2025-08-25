(define (fixed-f f)
  (define (try old new)
    (if (< (abs (- old new)) 0.00001)
	new
	(try new (f new))))
  (try 1 (f 1)))

(define (avg a b)
  (/ (+ a b) 2))

(define (sqrt x)
  (fixed-f (lambda(g) (avg g (/ x g)))))

(sqrt 4)

(define (deriv f)
  (define df 0.0000001)
  (lambda(x) (/ (- (f (+ x df)) (f x)) df)))

(define (newton f)
  (lambda(x) (- x (/ (f x) ((deriv f) x)))))

(define (sqrt-1 x)
  (fixed-f (newton (lambda(y) (- (* y y) x)))))

(sqrt-1 64)


(define (+ x y)
  (if (= y 0)
      x
      (+ (1+ x) (-1+ y))))

(+ 1 8)

(define (fib n)
  (define (helper a b c)
    (if (= c n)
	b
	(helper (+ a b) a (1+ c))))
  (helper 1 0 -1))

(fib 5)

(define (rec-fib n)
  (if (= n 1)
      0
      (if (= n 2)
	  1
	  (+ (rec-fib (- n 1)) (rec-fib (- n 2))))))
