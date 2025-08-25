(define (fixed-f f g)
  (define (abs x)
    (if (< x 0)
	(- x)
	x))
  (if (< (abs (- g (f g))) 0.001)
      g
      (fixed-f f (f g))))

(define (sqrt x)
  (define (avg a b)
    (/ (+ a b) 2))
  (fixed-f (lambda (i) (avg i (/ x i))) 1))

(sqrt 2)


