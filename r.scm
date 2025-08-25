(define (sqrt x)
  (define (avg a b)
    (/ (+ a b) 2))
  (define (try guess)
    (if (good-enough? guess)
	guess
	(try (avg guess (/ x guess)))))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.0001))
  (try 1))

(sqrt 4)

(define (fixed-point f)
  (lambda(x)
    (define (try old new)
      (if (< (abs (- old new)) 0.0001)
	  new
	  (try new (f new))))
    (try x (f x))))

(define (sqrt-1 x)
  (define (avg a b)
    (/ (+ a b) 2))
  ((fixed-point (lambda(g) (avg g (/ x g)))) 1))

(sqrt-1 2)

(define (deriv f)
  (let ((dx 0.0001))
  (lambda(x)
    (/ (- (f (+ x dx)) (f x)) dx))))

(define (newton f)
  (lambda(g)
    ((fixed-point (lambda(x) (- x (/ (f x) ((deriv f) x))))) g)))

(define (sqrt-2 x)
  ((newton (lambda(y) (- (square y) x))) 1))

(sqrt-2 2)
