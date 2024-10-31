(define (average-damp f)
  (define (avg a b)
    (/ (+ a b) 2))
  (lambda(x) (avg x (f x))))

(define (sqrt-1 x)
  (define (abs a)
    (if (< a 0)
	(- a)
	a))
  (define (square a)
    (* a a))
  (define (good-enough? g)
    (< (abs (- x (square g))) 0.0001))
  (define (improve g)
    ((average-damp (lambda(a) (/ x a))) g))
  (define (try g)
    (if (good-enough? g)
	g
	(try (improve g))))
  (try 1))

(sqrt-1 4)

(define (sum a b)
  (define (iter x y count)
    (if (> x y)
	count
	(iter (1+ x) y (+ count x))))
  (iter a b 0))

(sum 0 5)

(define (generic-sum a b term next)
  (define (iter x y val)
    (if (> x y)
	val
	(iter (next x) y (+ val (term x)))))
  (iter a b 0))

(define (sum-int a b)
  (generic-sum a b (lambda(x) x) (lambda(x) (1+ x))))

(sum-int 0 5) ;; -> 15

(define (sum-sq a b)
  (define (square a)
    (* a a))
  (generic-sum a b square (lambda(x) (1+ x))))

(sum-sq 0 5) ;; -> 55
      
(sum-sq 1 8) ;; -> 204

(define (pi-sum a b)
  (generic-sum a b (lambda(x) (/ 1 (* x (+ x 2)))) (lambda(x) (+ x 4))))

(pi-sum 1 100)


(define (recursive-generic-sum a b term next)
  (if (> a b)
      0
      (+ (term a)
	 (recursive-generic-sum (next a) b term next))))

(define (recursive-sum-square a b)
  (define (square a) (* a a))
  (recursive-generic-sum a b square 1+))

(recursive-sum-square 1 4) ;; -> 30

(define (compute-num-ways num-steps)
  (define (iter curr next count)
    (if (= count num-steps)
	curr
	(iter next (+ curr next) (1+ count))))
  (iter 1 1 0))

(compute-num-ways 10) ;; DP Problem  


(define (fixed-point f g)
  (define tolerance 0.0001)
  (define (abs a)
    (if (< a 0)
	(- a)
	a))
  (define (close-enuf? old new)
    (< (abs (- old new)) tolerance))
  (define (iter old new)
    (if (close-enuf? old new)
	new
	(iter new (f new))))
  (iter g (f g)))

(define (sqrt-1 x)
  (fixed-point (average-damp (lambda(y) (/ x y))) 1))

(sqrt-1 4) 

(define (deriv f)
  (let ((h 0.00001))
    (lambda(x)
      (/ (- (f (+ x h)) (f x)) h))))
    

(define (newton f g)
  (fixed-point (lambda(x) (- x (/ (f x) ((deriv f) x)))) g))

(define (sqrt-2 x)
  (define (square a) (* a a))
  (newton (lambda(a) (- (square a) x)) 1))

(sqrt-2 16)
