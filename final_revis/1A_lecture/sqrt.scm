(define (square x)
  (* x x))

(square 2) ;; -> 4

(define square
  (lambda (x) (* x x)))

(square 2) ;; -> 4

(define (average-damp f)
  (lambda (x)
    (/ (+ x (f x)) 2)))

(define (sqrt x)
  (define (try g)
    (if (good-enough? g)
	g
	(try (improve g))))
  (define (good-enough? g)
    (< (abs (- (square g) x)) 0.0001))
  (define (improve g)
    ((average-damp (lambda(a) (/ x a))) g))
  (try 1))

(sqrt 1) ;; -> 1
(sqrt 4) ;; -> 21523361/10761680 (approx 2)

(define (fixed-point f initial-guess)
  (define (try g)
    (if (good-enough? g)
	g
	(try (f g))))
  (define (good-enough? g)
    (< (abs (- g (f g))) 0.00001))
  (try initial-guess))


(define (sqrt x)
  (fixed-point (average-damp (lambda(a) (/ x a))) 1))

(sqrt 1) ;; -> 1
(sqrt 4) ;; -> 21523361/10761680


(define (deriv f)
  (lambda (x)
    (let ((h 0.000001))
      (/ (- (f (+ x h)) (f x)) h))))

(define (newton f initial-guess)
  (fixed-point (lambda(x) (- x (/ (f x) ((deriv f) x)))) initial-guess))


(define (sqrt x)
  (newton (lambda(y) (- (square y) x)) 1))

(sqrt 1);; -> 1
(sqrt 4) ;; -> 2.00
(sqrt 2) ;; -> 1.4142

(+ 3 17.4 5) ;; -> 25.4

(+ 3 (* 5 6) 8 2) ;; -> 43

(define A (* 5 5))
(* A A) ;; -> 625
(define B (+ A (* 5 a)))
b ;; -> 150
(+ A (/ B 5)) ;; -> 55

(define (square x)
  (* x x))

(square 10) ;; -> 100

(define square
  (lambda(x) (* x x)))

(square 10) ;; -> 100
(square 1001.7) ;; -> 1003402.8900000001

(define (average x y)
  (/ (+ x y) 2))

(define (mean-square x y)
  (average (square x) (square y)))

(mean-square 2 4) ;; -> 10
(mean-square 2 3) ;; -> 13/2


(define (abs x)
  (cond ((< x 0) (- x))
	((= x 0) x)
	((> x 0) x)))

(abs 2) ;; -> 2
(abs 0) ;; -> 0
(abs -9) ;; -> 9

;; Alterantive definition of absolute value with if
(define (abs x)
  (if (< x 0)
      (- x)
      x))

(abs 2) ;; -> 2
(abs 0) ;; -> 0
(abs -3) ;; -> 3
(abs -9) ;; -> 9
