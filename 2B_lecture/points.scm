(define (make-point x y)
  (cons x y))

(define (x-coord a)
  (car a))

(define (y-coord a)
  (cdr a))

(define (vector+ a b)
  (make-point (+ (car a) (car b)) (+ (cdr a) (cdr b))))

(define (vector- a b)
  (make-point (- (car a) (car b)) (- (cdr a) (cdr b))))

(define (make-seg x y)
  (cons x y))

(define (seg-start s)
  (car s))

(define (seg-end s)
  (cdr s))

(define (midpoint s)
  (let ((a (seg-start s))
	(b (seg-end s)))
    (make-point (/ (+ (x-coord a) (x-coord b)) 2)  (/ (+ (y-coord a) (y-coord b)) 2))))


(define (square x)
  (* x x))

(define (fixed-f func guess)
  (define (abs x)
    (if (< x 0)
	(- x)
	x))
  (define (iter old new)
    (if (< (abs (- old new)) 0.001)
	new
	(iter new (func new))))
  (iter guess (func guess)))

(define (sqrt x)
  (define (newton func guess)
    (define dx 0.0000001)
    (define (derivative f)
      (lambda(m) (/ (- (f (+ m dx)) (f m)) dx)))
    (fixed-f (lambda (y)(- y (/ (func y) ((derivative func) y)))) guess))
  (newton (lambda (a) (- (square a) x)) 1))


(define (length s)
  (let ((a (seg-start s))
	(b (seg-end s)))
    (let ((dx (- (x-coord a) (x-coord b)))
	  (dy (- (y-coord a) (y-coord b))))
      (sqrt (+ (square dx) (square dy))))))
	

(vector+ (make-point 1 2) (make-point 1 4))
(make-seg (make-point 1 2) (make-point 2 3))
(midpoint (make-seg (make-point 1 2) (make-point 2 4)))
(sqrt 16)
(length (make-seg (make-point 1 2) (make-point 5 5)))
