(define make-vector cons)
(define x-coord car)
(define y-coord cdr)
(define make-line-segment cons)
(define start car)
(define end cdr)

(define (length l)
  (let ((dx (- (x-coord (start l)) (x-coord (end l))))
	(dy (- (y-coord (start l)) (y-coord (end l)))))
    (sqrt (+ (square dx) (square dy)))))

(define p1 (make-vector 1 2))
(define p2 (make-vector 5 5))

(length (make-line-segment p1 p2))
