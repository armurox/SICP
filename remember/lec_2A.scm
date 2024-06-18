(define (sum a b)
  (define (sum-up ans i)
    (if (> i b)
	ans
	(sum-up (+ ans i) (+ i 1))))
  (sum-up 0 a)) ;; Iterative process

(sum 1 5)

(define (sum-int a b)
  (if (> a b)
      0
      (+ a (sum-int (1+ a) b)))) ;; Recursive process

(sum-int 1 5)

;; Generalize
(define (sum a b f next)
  (if (> a b)
      0
      (+ (f a) (sum (next a) b f next)))) ;; recursive version

(sum 1 2 square (lambda(x) (1+ x))) ;; -> 5

(define (sum a b f increment)
  (define (sum-up ans i)
    (if (> i b)
	ans
	(sum-up (+ ans (f i)) (increment i))))
  (sum-up 0 a)) ;; iterative version

(sum 1 4 square (lambda(x) (1+ x))) ;; -> 30
(sum 1 4 (lambda(x) (/ 1 (* x (+ x 2)))) (lambda(x) (+ x 4))) ;; -> 1 / 3

(define (sum-int a b)
  (sum a b (lambda(x) x) 1+))

(sum-int 1 5)

(define (sum-square a b)
  (sum a b square 1+))

(sum-square 1 2)

(define (pi-sum a b)
  (sum a b (lambda(x) (/ 1 (* x (+ x 2)))) (lambda(x) (+ x 4))))

(pi-sum 1 4)

(define (sqrt x)
  (define tolerance 0.000001)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) tolerance))
  (define (avg a b)
    (/ (+ a b) 2))
  (define (improve guess)
    (avg (/ x guess) guess))
  (define (try guess)
    (if (good-enough? guess)
	guess
	(try (improve guess))))
  (try 1)) ;; Heron of Alexandria's method

(sqrt 4)

(define (fixed-point f g)
  (define tolerance 0.000001)
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  (define (improve guess)
    (f guess))
  (define (try guess)
    (if (good-enough? guess)
	guess
	(try (improve guess))))
  (try g))

(define (sqrt x)
  (define (avg a b)
    (/ (+ a b) 2))
  (fixed-point (lambda(a) (avg a (/ x a))) 1))

(sqrt 4)

(define (avg a b)
    (/ (+ a b) 2))

(define (average-damp f)
  (lambda(x)
    (avg x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda(a) (/ x a))) 1)) ;; Using average damp

(sqrt 4)

(define (deriv f)
  (define h 0.000001)
  (lambda(x)
    (/ (- (f (+ x h)) (f x)) h)))

(define (newton f guess)
  (define df (deriv f))
  (fixed-point (lambda(x)
		 (- x (/ (f x) (df x))))
	       guess))

(define (sqrt x)
  (newton (lambda(y)
	    (- (square y) x))
	  1)) ;; Square root with newton-rhapson method

(sqrt 2) ;; -> 1.4142135623754424
  
    
