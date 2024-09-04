(define (fixed-point f)
  (define (try old new)
    (if (good-enough? old new)
	new
	(try new (f new))))
  (define (abs a)
    (if (< a 0)
	(- a)
	a))
  (define (good-enough? a b)
    (< (abs (- a b)) 0.00001))
  (try 0 1))

(define (deriv f)
  (lambda (x)
  (let ((df 0.00001))
    (/ (- (f (+ x df)) (f x)) df))))

(define (newton f)
  (fixed-point
   (lambda(x)
     (- x (/ (f x) ((deriv f) x))))))

(define (sqrt-1 x)
  (newton (lambda(y) (- (square y) x))))

(sqrt-1 2)
