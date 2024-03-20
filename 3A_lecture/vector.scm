(define (make-vector a b)
  (cons a b))

(define (x-coord a)
  (car a))

(define (y-coord a)
  (cdr a))

(define (vector+ a b)
  (make-vector (+ (x-coord a) (x-coord b))
	       (+ (y-coord a) (y-coord b))))
(define (scale s v)
  (make-vector (* s (x-coord v))
	       (* s (y-coord v))))

(scale 2 (make-vector 3 4))
(vector+ (cons 2 3) (cons 4 7))

