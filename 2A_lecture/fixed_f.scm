(define (fixed good-enough guess improve a)
  (if (good-enough guess)
      guess
      (fixed good-enough (improve guess) improve a)))

(define (sqrt a)
  (define (square x) (* x x))
  (define (average x y) (/ (+ x y) 2))
  (fixed (lambda(i)(< (abs (- (square i) a)) 0.001)) 1 (lambda(i)(average i (/ a i))) a))

(define (fixed-f func guess)
  (define (abs x)
    (if (< x 0)
	(- x)
	x))
  (if (< (abs (- (func guess) guess)) 0.001)
      (func guess)
      (fixed-f func (func guess))))

;; Square root definition based on fixed-f
(define (sqrt_1 x)
  (define (average x y) (/ (+ x y) 2))
  (fixed-f (lambda (i)(average i (/ x i))) 1))

;; Fixed point function with internal function definition
(define (func_f func guess)
  (define (abs x)
    (if (< x 0) (- x)
	x))
  (define (iter old new)
    (if (< (abs (- old new)) 0.001)
	new
	(iter new (func new))))
  (iter guess (func guess)))

(define (sqrt_2 x)
  (define (average x y ) (/ (+ x y) 2))
  (func_f (lambda(i) (average i (/ x i))) 1))

;; Square root with average damping definition
(define (sqrt_3 x)
  (define (average x y ) (/ (+ x y) 2))
  (define (average_damp f) (lambda(a) (average a (f a))))
  (func_f (average_damp (lambda(i) (/ x i ))) 1))


(define (newton f guess)
  (define (deriv func)
    (define df 0.00000001)
    (lambda (x) ( / (- (func (+ x df)) (func x)) df)))
  (func_f (lambda (y)(- y (/ (f y) ((deriv f) y)))) guess))

;; Square root with newton-rhapson method
(define (sqrt_5 x)
  (define (square y) (* y y))
  (newton (lambda(y) (- (square y) x)) 1))

(sqrt_5 16)
(sqrt 4)
(sqrt_1 4)
(sqrt_2 4)
(sqrt_3 4)

