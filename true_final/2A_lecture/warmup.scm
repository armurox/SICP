(define (fixed-point f g)
  (define tolerance 0.00001)
  (define (abs x)
    (if (< x 0)
	(- x)
	x))
  (define (close-enuf? a b)
    (< (abs (- a b)) tolerance))
  (define (iter old new)
    (if (close-enuf? old new)
	new
	(iter new (f new))))
  (iter g (f g)))

(define (deriv f)
  (define h 0.00001)
  (lambda(x)
    (/ (- (f (+ x h)) (f x)) h)))

(define (newton f g)
  (define df (deriv f))
  (fixed-point (lambda(y) (- y (/ (f y) (df y)))) g))

(define (sqrt x)
  (define (square a) (* a a))
  (newton (lambda(y) (- (square y) x)) 1))

(sqrt 2)
