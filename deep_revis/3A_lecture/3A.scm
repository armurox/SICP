;; Lets start with a construction of vectors using the previous lecture's example
(define make-vector cons)
(define xcoord car)
(define ycoord cdr)

;; We can implement addition and subtraction of vectors also,as well as scaling of vectors
(define (+vec v1 v2)
  (let ((x1 (xcoord v1))
	(x2 (xcoord v2))
	(y1 (ycoord v1))
	(y2 (ycoord v2)))
    (make-vector
     (+ x1 x2) (+ y1 y2))))


(define (-vec v1 v2)
  (let ((x1 (xcoord v1))
	(x2 (xcoord v2))
	(y1 (ycoord v1))
	(y2 (ycoord v2)))
    (make-vector
     (- x1 x2) (- y1 y2))))

(define (scale s v)
  (let ((x (xcoord v))
	(y (ycoord v)))
    (make-vector
     (* s x) (* s y))))


(define v1 (make-vector 2 4))
(define v2 (make-vector 4 8))

(+vec v1 v2) ;; -> (6 . 12)
(-vec v1 v2) ;; -> (-2, -4)
(scale 2 v1) ;; -> (4 . 8)
(scale 2 (+vec v1 v2)) ;; -> (12 . 24)
