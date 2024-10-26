(define (sqrt x)
  (define (avg a b)
    (/ (+ a b) 2))
  (define (abs a)
    (if (< a 0)
	(- a)
	a))
  (define (square a)
    (* a a))
  (define (good-enough? g)
    (< (abs (- x (square g))) 0.0001))
  (define (improve g)
    (avg g (/ x g)))
  (define (try g)
    (if (good-enough? g)
	g
	(try (improve g))))
  (try 1))

(sqrt 4)


(define (fixed-point f)
  (define (avg a b)
    (/ (+ a b) 2))
  (define (abs a)
    (if (< a 0)
	(- a)
	a))
  (define (good-enough? g)
    (< (abs (- g (f g))) 0.0001))
  (define (try g)
    (if (good-enough? g)
	g
	(try (f g))))
  (try 1))

(define (sqrt-1 x)
  (define (avg a b)
    (/ (+ a b) 2))
  (fixed-point (lambda(g) (avg g (/ x g)))))

(sqrt-1 2)

(define (deriv f)
  (let ((dx 0.0001))
    (lambda(x)
      (/ (- (f (+ x dx)) (f x)) dx))))

(define (newton f)
  (fixed-point (lambda(x)
		 (- x (/ (f x) ((deriv f) x))))))

(define (sqrt-2 x)
  (define (square a)
    (* a a))
  (newton (lambda(y)
	    (- (square y) x))))

(sqrt-2 100)

(define (repeated f n)
  (if (= n 1)
      f
      (lambda(x) (f ((repeated f (- n 1)) x)))))

((repeated (lambda(a) (* a a)) 2) 2) ;; -> 16
