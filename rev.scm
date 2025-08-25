(define (sqrt x)
  (define (try guess)
    (if (good-enough? guess)
	guess
	(try (avg guess (/ x guess)))
	))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.0001))
  (define (avg a b)
    (/ (+ a b) 2))
  (try 1))

(sqrt 4)

(define (fixed-f func guess)
  (define (try old new)
    (if (< (abs (- old new)) 0.0001)
	new
	(try new (func new))))
  (try guess (func guess)))


(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-1 x)
  (fixed-f (lambda(g) (average g (/ x g))) 1))

(sqrt-1 4)

(define (sqrt-2 x)
  (define (average-damp func)
    (lambda(i)
      (average i (func i))))
  (fixed-f (average-damp (lambda(g) (/ x g))) 1))

(sqrt-2 4)

(define (deriv f)
  (let ((dx 0.0001))
    (lambda(x)
    (/ (- (f (+ x dx)) (f x)) dx))))

((deriv square) 1)

(define (newton f)
  (fixed-f (lambda(x) (- x (/ (f x) ((deriv f) x)))) 1))

(define (sqrt-3 x)
  (newton
   (lambda(i) (- (square i) x))))

(sqrt-3 2)

(define (add a b)
  (if (= a 0)
      b
      (add (-1+ a) (1+ b))))

(add 1 2)

(define (add-1 a b)
  (if (= a 0)
      b
      (1+ (add-1 (-1+ a) b))))

(add-1 1 2)

(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else
	 (+ (fib (- n 1)) (fib (- n 2))))))

(fib 6)

(define (fib-1 n)
  (define (helper a b count)
    (if (or (= count n) (> count n))
	a
	(helper (+ a b) a (+ count 1))))
  (helper 0 1 0))

(fib-1 6)
