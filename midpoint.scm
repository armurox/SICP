(define make-vector cons)

(define x-coord car)
(define y-coord cdr)

(define (vect+ a b)
  (make-vector
   (+ (x-coord a) (x-coord b))
   (+ (y-coord a) (y-coord b))))

(define (scale s v)
  (make-vector
   (* s (x-coord v))
   (* s (y-coord v))))

(define make-line-segment cons)

(define start car)
(define end cdr)

(define (midpoint l)
  (scale 0.5 (vect+ (start l) (end l))))

(define s (make-vector 1 2))

(define e (make-vector 3 6))

(midpoint (make-line-segment s e))
