(define (sqrt x)
  (define (avg a b)
    (/ (+ a b) 2))
  (define (square a)
    (* a a))
  (define (abs a)
    (if (< a 0)
	(- a)
	a))
  (define (good-enough? a)
    (< (abs (- (square a) x)) 0.0001))
  (define (try g)
    (if (good-enough? g)
	g
	(try (avg g (/ x g)))))
  (try 1))

(sqrt 1) ;; -> 1
(sqrt 4) ;; -> 21523361 / 10761680
(sqrt 2) ;; -> 577/408

(define (fixed-point f)
  (lambda(g)
    (define (abs a)
      (if (< a 0)
	  (- a)
	  a))
    (define (good-enough? old new)
      (< (abs (- old new)) 0.00001))
    (define (iter old new)
      (if (good-enough? old new)
	  new
	  (iter new (f new))))
    (iter g (f g))))

(define (avg a b)
  (/ (+ a b) 2))

(define (sqrt x)
  ((fixed-point (lambda(g) (avg g (/ x g)))) 1))

(sqrt 1) ;; -> 1
(sqrt 4) ;; -> 926510094425921/463255047212960

(define (avg-damp f)
  (lambda(x)
    (avg x (f x))))

((avg-damp square) 2) ;; -> 3

(define (sqrt x)
  ((fixed-point (avg-damp (lambda(g) (/ x g)))) 1))

(sqrt 4) ;; -> 926510094425921/463255047212960

(define (deriv f)
  (lambda(x)
    (let ((dx 0.00001))
      (/ (- (f (+ x dx)) (f x)) dx))))

(define (newton f)
  (lambda(g)
    (let ((df (deriv f)))
      ((fixed-point (lambda(a) (- a (/ (f a) (df a))))) g))))

(define (sqrt x)
  ((newton (lambda(a) (- (square a) x))) 1))

(sqrt 4) ;; -> 2.000
(sqrt 2) ;; -> 1.414



