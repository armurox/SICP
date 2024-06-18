(define (sqrt-1 x)
  (define (good-enough? g)
    (< (abs (- (square g) x)) 0.0001))
  (define (square a)
    (* a a))
  (define (abs a)
    (if (< a 0)
	(- a)
	a))
  (define (avg a b)
    (/ (+ a b) 2))
  (define (try guess)
    (if (good-enough? guess)
	guess
	(try (avg guess (/ x guess)))))
  (try 1))

(sqrt-1 4)

(define (fixed-point f)
  (define (try g)
    (if (< (abs (- g (f g))) 0.0001)
	g
	(try (f g))))
  (lambda(g)
    (try g)))

(define (average-damp f)
  (define (avg a b)
    (/ (+ a b) 2))
  (lambda(x)
    (avg x (f x))))

(define (sqrt-2 x)
  ((fixed-point (average-damp (lambda(g) (/ x g)))) 1))

(sqrt-2 4)

(define (deriv f)
  (define h 0.0001)
  (lambda(x)
    (/ (- (f (+ x h)) (f x)) h)))

(define (newton f)
  (lambda(x)
    (- x (/ (f x) ((deriv f) x)))))

(define (sqrt-3 x)
  (define (square a)
    (* a a))
  ((fixed-point (newton (lambda(y) (- (square y) x)))) 1))

(sqrt-3 2)
