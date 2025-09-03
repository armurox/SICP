(define a (cons 2 4))

(car a) ;; -> 2
(cdr a) ;; -> 4

;; Here we are using the Euclidean algorithm for the gcd
(define (gcd a b)
  (if (= a 0)
      b
      (gcd (modulo b a) a)))

(gcd 4 8) ;; -> 4
(gcd 20 30) ;; -> 10
(gcd 40 30) ;; -> 10

;; Construction of a rational number
(define (make-rat a b)
  (let ((greatest-common-divisor (gcd a b)))
    (cons (/ a greatest-common-divisor) (/ b greatest-common-divisor))))

;; Selection of the numerator and denominator
(define numer car)
(define denom cdr)

;; Some operations
(define (rat+ x y)
  (make-rat
   (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (rat- x y)
  (make-rat
   (- (* (numer x) (denom y)) (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (rat* x y)
  (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))

;; Testing
(define a (make-rat 4 8))
(define b (make-rat 8 10))
(numer a) ;; 1
(denom a) ;; -> 2

(rat+ a b) ;; -> (13 . 10)
(rat- a b) ;; -> (3 . -10)
(rat* a b) ;; -> (2 . 5)

;; composing
(rat* b (rat+ a b)) ;; -> (26 . 25)



(let ((a 2))
  (* a a)) ;; -> 4

;; Let's apply our knowledge now of connecting pairs to constructing vectors, and building up to computing the distance of a line
(define make-vector cons)
(define xcoord car)
(define ycoord cdr)


;; Note that a line is made up of two points
(define make-line cons)
(define first-point car)
(define second-point cdr)

;; Now, we can define a method for computing Euclidean distance, assuming we get a line as input
(define (distance l)
  (let ((x1 (xcoord (first-point l)))
	(x2 (xcoord (second-point l)))
	(y1 (ycoord (first-point l)))
	(y2 (ycoord (second-point l))))
    (sqrt (+ (square (- x2 x1)) (square (- y2 y1))))))

;; We can also compute the midpoint of a line segment, for which we will also define an average procedure first
(define (average a b)
  (/ (+ a b) 2))

(define (midpoint l)
  (let ((x1 (xcoord (first-point l)))
	(x2 (xcoord (second-point l)))
	(y1 (ycoord (first-point l)))
	(y2 (ycoord (second-point l))))
    (make-vector (average x1 x2) (average y1 y2))))

;; Now lets use the above definitions to construct a line
(define p1 (make-vector 1 2))
(define p2 (make-vector 4 6))
(define l1 (make-line p_1 p_2))

(distance l1) ;; -> 5
(midpoint l1) ;; -> (5/2 . 4)



;; Lets now start to blur the lines between data and procedures, looking at possible implementation of cons, car and cdr
(define (cons a b)
  (lambda(pick)
    (if (= pick 1)
	a
	b)))

(define (car x)
  (x 1))

(define (cdr x)
  (x 2))

(define m (cons 1 2)) ;; -> m, i.e. simple creation of a variable
(car m) ;; -> 1
(cdr m) ;; -> 2

;; In other words, the above implementations satisfy the basic contract for a pair with cons, car and cdr implemented, but cons is simply a procedure that selects the data, i.e., the data itself is being constructed from a procedure.
