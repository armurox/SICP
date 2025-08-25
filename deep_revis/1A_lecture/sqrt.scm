(+ 1 2)

(define (square x) (* x x))

(square 2)

(define (abs a)
  (if (< a 0)
      (- a)
      a))

(abs -1)
(abs 1)
(abs 0)

(define (avg a b)
  (/ (+ a b) 2))

(avg 1 2)

(avg 4 5)

;; Iterative implementation of a square root where we repeatedly apply the function average(guess + x / guess)
(define (sqrt x)
  (define (improve guess)
    (avg guess (/ x guess)))
  (define (is-good? guess old-guess)
    (< (abs (- guess old-guess)) 0.0001))
  (define (try guess)
    (define (iter old new)
      (if (is-good? new old)
	  new
	  (iter new (improve new))))
    (iter guess (improve guess)))
  (try 1))

(sqrt 1) ;; -> 1
(sqrt 4)

;; A recursive implementation of the square root, using the same function as mentioned above
(define (sqrt-recur x)
  (define (improve guess)
    (avg guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- guess (improve guess))) 0.00001))
  (define (try guess)
    (if (good-enough? guess)
	(improve guess)
	(try (improve guess))))
  (try 1))

(sqrt-recur 4)

;; An alternative construction is to extract out the idea of repeatedly applying a function until it spits out a single value, which is the fixed-point procedure below
(define (fixed-point func)
  (lambda(x)
    (define (good-enough? guess old-guess)
      (< (abs (- guess (func guess))) 0.00001))
    (define (try old new)
      (if (good-enough? new old)
	  new
	  (try new (func new))))
    (try x (func x))))


(define (sqrt x)
  ((fixed-point
    (lambda(g) (avg g (/ x g)))) x))

(sqrt 4)

;; We can further abstract away the idea of taking the average of a function's output and its input, and account for that within average damping
(define (avg-damp func)
  (lambda(g)
    (avg g (func g))))

(define (sqrt x)
  ((fixed-point (avg-damp
		 (lambda(g) (/ x g)))) x))

(sqrt 4)

;; Now, we will build up to the Newton-Rhapson Method of computing square roots.
;; The rough formulation involves two parts, first, the actual Newton-Rhapson method applies to roots of a function in general
;; It states that to find a root x, we start with some guess x_0 and repeatedly apply x_(n+1) = x_n - f(x_n) / f'(x_n) starting with n = 0

;; Now, the idea of repetedly applying until we get some guess is remniscent of fixed-points, so we now need to simply construct the actual function
;; To do that, we need one preliminary, which is the actual function for computing a derivitive, which I will demonstrate below

;; The idea is quite simple, we will approximate the derivitive with its definition of lim h -> 0(f(x + h) - f(x)) / h
;; And to approximate the idea of the limit, we simply let h be a very small constant
(define (deriv f)
  (lambda(x)
    (let ((h 0.00001))
      (/ (- (f (+ x h)) (f x)) h))))



;; Now, on to the newton rhapson method, this should be a procedure that takes in a function, and computes it root.
;; Then, the final definition of a square root is simply the function, where, if we assume we are computing the square root of y, then it is  f(y) = y^2 - x.
(define (newton f)
  (fixed-point
   (let ((df (deriv f)))
     (lambda(x)
       (- x (/ (f x) (df x)))))))

(define (sqrt x)
  ((newton (lambda(y) (- (square y) x))) x))

(sqrt 2)
