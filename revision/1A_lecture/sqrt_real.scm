(define (sqrt_1 x)
  (define (avg a b)
    (/ (+ a b) 2))
  (define (improve guess)
    (avg guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.000001))
  (define (try guess)
    (if (good-enough? guess)
	guess
	(try (improve guess))))
  (try 1))

(sqrt_1 2)
