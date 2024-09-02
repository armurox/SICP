(define (fixed-point f)
  (define (try old new)
    (if (good-enough? old new)
	new
	(try new (f new))))
  (define (avg a b)
    (/ (+ a b) 2))
  (define (abs a)
    (if (> a 0)
	a
	(- a)))
  (define (good-enough? old new)
    (< (abs (- old new)) 0.00001))
  (try 0 1))

(define (avg a b)
  (/ (+ a b) 2))

(define (sqrt x)
  (fixed-point (lambda(g) (avg g (/ x g)))))

(define (fib n)
  (cond ((< n 1) 0)
      ((= n 1) 1)
      (else
       (+ (fib (- n 1)) (fib (- n 2))))))

(fib 10)

(define (fib n)
  (define (helper a b count)
    (if (= count n)
	a
	(helper b (+ a b) (1+ count))))
  (helper 0 1 0))

(fib 10)

(define (average-damp f)
  (lambda(x) (avg x (f x))))

(define (sqrt-1 x)
  (fixed-point (average-damp (lambda(g) (/ x g)))))

(sqrt-1 2)

(define (deriv f)
  (let ((h 0.00001))
  (lambda(x)
    (/ (- (f (+ x h)) (f x)) h))))

(define (newton f)
  (fixed-point (lambda(x)
    (- x (/ (f x) ((deriv f) x))))))

(define (sqrt-2 x)
  (newton (lambda (y) (- (square y) x))))

(sqrt-2 2)
  
	
