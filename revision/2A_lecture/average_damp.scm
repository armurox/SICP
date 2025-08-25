(define (fixed-point f g)
  (if (< (abs (- g (f g))) 0.00001)
      g
      (fixed-point f (f g))))

(define (avg a b)
  (/ (+ a b) 2))

(define (average-damp func)
  (lambda(y) (avg y (func y))))

(define (sqrt x)
  (fixed-point (average-damp (lambda(y) (/ x y))) 1))

(sqrt 4)


(define (make-rat a b)
  (define div (gcd a b))
  (cons (/ a div) (/ b div)))

(define (rat+ a b)
  (make-rat  (+ (* (car a) (cdr b)) (* (cdr a) (car b))) (* (cdr a) (cdr b))))

(define m
  (make-rat 1 2))

(define n
  (make-rat 2 4))

(rat+ m n)

(define (my-cons x y)
  (lambda(a)
    (if (= a 1)
	x
	y)))

(define (my-car a)
  (a 1))

(define (my-cdr a)
  (a 2))

(my-cons 1 2)

(my-car (my-cons 1 2))
(my-cdr (my-cons 2 4))
