A;; Iterative implementAAAation of the sum from a to b
(define (sum a b)
  (define (iter final-sum i)
    (if (> i b)
	final-sum
	(iter (+ final-sum i) (1+ i))))
  (iter 0 a))

(sum 1 2) ;; -> 3
(sum 1 10) ;; -> 2

;; A more generic version of sum, which takes, as its input any function, and computes a sum through those terms, using that function
(define (generic-sum a b f increment)
  (define (iter final-sum i)
    (if (> i b)
	final-sum
	(iter (+ final-sum (f i)) (increment i))))
  (iter 0 a))


(define (sum a b)
  (generic-sum a b (lambda(a) a) 1+))

(sum 1 2) ;; -> 3
(sum 1 10) ;; -> 55

(define (square a)
  (* a a))

(define (square-sum a b)
  (generic-sum a b square 1+))

(square-sum 1 2) ;; -> 5
(square-sum 1 10) ;; -> 385

(define (pi-sum a b)
  (generic-sum a b (lambda(i) (/ 1 (* i (+ i 2)))) (lambda(x) (+ x 4))))

(pi-sum 1 10) ;; -> 1289/3465
(pi-sum 1 1) ;; -> 1/3

;; Recursive implementation
(define (sum-int a b)
  (if (> a b)
      0
      (+ a (sum-int (1+ a) b))))

(sum-int 1 10) ;; -> 55

;; Recursive implementation of the generic sum
(define (generic-sum-int a b f increment)
  (if (> a b)
      0
      (+ (f a) (generic-sum-int (increment a) b f increment))))

(define (sum-int a b)
  (generic-sum-int a b (lambda(a) a) 1+))

(sum-int 1 10) ;; -> 55

(define (square-sum-int a b)
  (generic-sum-int a b square 1+))

(square-sum-int 1 10) ;; -> 385

;; Fixed-point now

(define (avg a b)
  (/ (+ a b) 2))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (fixed-point f g)
  (let ((tolerance 0.0000001))
    (define (good-enough? old new)
      (< (abs (- old new)) tolerance))
    (define (iter old new)
      (if (good-enough? old new)
	  new
	  (iter new (f new))))
    (iter g (f g))))

(define (avg-damp f)
  (lambda(a) (avg a (f a))))

(define (sqrt x)
  (fixed-point (avg-damp (lambda(g) (/ x g))) 1))

(sqrt 1) ;; -> 1
(sqrt 2)


;; Sqrt with newton's method

;; First let us define our derivative
(define (deriv f)
  (let ((h 0.000001))
    (lambda(x)
      (/ (- (f (+ x h)) (f x)) h))))

;; Now we can define the Newton-Rhapson procedure for finding square roots, given a guess. The formula boils down x_n = x_(n - 1) + f(x_(n - 1)) / f'(x_(n - 1)) being applied repeatedly, until x_n and x_(n-1) get close together, i.e. a clear example of using our fixed-point method to implement this

(define (newton-rhapson f g)
  (let ((df (deriv f)))
    (fixed-point (lambda(x) (- x (/ (f x) (df x)))) g)))


;; Finally, we implement square root using our defined newton-rhapson procedure, starting our guess at 1, with our input procedure being the one which is precisely a^2 - x, since we need to find the root of that to get the a that is the square root
(define (sqrt x)
  (newton-rhapson (lambda(a) (- (square a) x)) 1))

(sqrt 1) ;; -> 1
(sqrt 4) ;; -> 2
(sqrt 2) ;; -> 1.4242 yay:)



  
  



