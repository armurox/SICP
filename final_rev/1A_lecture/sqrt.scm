(define (sqrt x)
  (define (try guess)
    (if (good-enough? guess)
	guess
	(try (improve guess))))
  (define (abs a)
    (if (> a 0)
	a
	(- a)))
  (define (avg a b)
    (/ (+ a b) 2))
  (define (square a)
    (* a a))
  (define (good-enough? guess)
    (< (abs (- x (square guess))) 0.0001))
  (define (improve guess)
    (avg x (/ x guess)))
  (try 1))

(sqrt 2)

(define (fixed-point f)
    (define (try old new)
      (if (good-enough? old new)
	  new
	  (try new (f new))))
    (define (abs a)
      (if (> a 0)
	  a
	  (- a)))
    (define (avg a b)
      (/ (+ a b) 2))
    (define (good-enough? old new)
      (< (abs (- old new)) 0.0001))
    (try 0 1))

(define (avg a b)
  (/ (+ a b) 2))

(define (sqrt-1 x)
  (fixed-point (lambda (g) (avg g (/ x g)))))

(sqrt-1 1)

	  
