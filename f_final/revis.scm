(define (square a)
  (* a a))

(define (abs a)
  (if (< a 0)
      (- a)
      a))

(define (avg a b)
  (/ (+ a b) 2))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.00001))
  (define (improve guess)
    (avg guess (/ x guess)))
  (define (try guess)
    (if (good-enough? guess)
	guess
	(try (improve guess))))
  (try 1))

(sqrt 1)
(sqrt 4)
(sqrt 2)

(define (fixed-point f)
  (lambda(g)
    (let ((min_diff 0.00001))
      (define (good-enough? a b)
	(< (abs (- a b)) min_diff))
      (define (iter old new)
	(if (good-enough? old new)
	    new
	    (iter new (f new))))
      (iter g (f g)))))

(define (sqrt x)
  ((fixed-point (lambda(guess) (avg guess (/ x guess)))) 1))

(sqrt 2)

(define (sqrt x)
  (define (average-damp f)
    (lambda(a)
      (avg a (f a))))
  ((fixed-point (lambda(guess) ((average-damp (lambda(g) (/ x g))) guess))) 1))

(sqrt 4)

(define (newton func)
  (define (deriv f)
    (let ((h 0.00001))
      (lambda(x)
	(/ (- (f (+ x h)) (f x)) h))))
  (let ((df (deriv func)))
    (fixed-point (lambda(x) (- x (/ (func x) (df x)))))))

(define (sqrt x)
  ((newton (lambda(y) (- (square y) x))) 1))

(sqrt 2)
(sqrt 16)

(define (+ a b)
  (define (iter x y)
    (if (= x 0)
	y
	(iter (-1+ x) (1+ y))))
  (iter a b))

(+ 3 4)

(define (+ a b)
  (if (= a 0)
      b
      (+ (-1+ a) (1+ b))))

(+ 4 5)

(define (+ a b)
  (if (= a 0)
      b
      (1+ (+ (-1+ a) b))))

(+ 4 5)

(define (fib n)
  (define (iter count prev curr)
    (if (> count (- n 1))
	prev
	(iter (+ count 1) curr (+ curr prev))))
  (iter 0 0 1))

(fib 6)

(define (fib n)
  (cond ((< n 1) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1)) (fib (- n 2))))))

(fib 6)
