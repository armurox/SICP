(define (make-vector a b)
  (cons a b))

(define (x-cor v)
  (car v))

(define (y-cor v)
  (cdr v))

(define (distance a b)
  (sqrt (+ (square (- (x-cor a) (x-cor b))) (square (- (y-cor a) (y-cor b))))))

(distance (make-vector 3 4) (make-vector 0 0))

(define (make-segment p q)
  (cons p q))

(define (segment-s a)
  (car a))

(define (segment-e a)
  (cdr a))

(define (midpoint s)
  (define (average a b)
    (/ (+ a b) 2))
  (let ((a (segment-s s))
	(b (segment-e s)))
    (make-vector
     (average (x-cor a) (x-cor b))
     (average (y-cor a) (y-cor b)))))

(define l1
  (make-segment (make-vector 1 2) (make-vector 3 4)))

(midpoint l1)
