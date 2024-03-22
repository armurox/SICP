(define make-vect cons)
(define x-coord car)
(define y-coord cdr)

(define make-seg cons)
(define start car)
(define end cdr)

(define (make-rect origin horizontal vertical)
  (list origin horizontal vertical))

(define orig car)
(define horiz (lambda(i) (car (cdr i))))
(define vert (lambda(i) (car (cdr (cdr i)))))

(define base-square (make-rect (make-vect 0 0) (make-vect 1 0) (make-vect 0 1)))

(vert base-square)

(define (scale rect)
  (lambda(point)
    (make-vect
     (+
      (x-coord (orig rect))
      (* (x-coord point) (x-coord (horiz rect)))
      (* (y-coord point) (x-coord (vert rect))))
     (+
      (y-coord (orig rect))
      (* (x-coord point) (y-coord (horiz rect)))
      (* (y-coord point) (y-coord (vert rect)))))))

((scale base-square) (make-vect 0 1))

(make-seg (make-vect 0 0) (make-vect 0 0.5))
(define (make-picture rect segs)
  (map
   (lambda(seg)
     (let ((scale-now (scale rect)))
       (make-seg (scale-now (start seg))
		 (scale-now (end seg)))))
   segs))


(define segments
  (list (make-seg (make-vect 0 0) (make-vect 0 0.5)) (make-seg (make-vect 0 0) (make-vect 1 0))))

(make-picture base-square segments)

(define (+vect a b)
  (make-vect
   (+ (x-coord a) (x-coord b))
   (+ (y-coord a) (y-coord b))))

(define (scale s v)
  (make-vect
   (* s (x-coord v))
   (* s (y-coord v))))

(define (coord-map rect)
  (lambda(point)
    (+vect
     (+vect
      (scale (x-coord point) (horiz rect))
      (scale (y-coord point) (vert rect)))
     (orig rect))))

(define (make-picture seglist)
  (lambda(rect)
    (for-each
     (lambda(seg)
       (drawline
	((coord-map rect) (start seg))
	((coord-map rect) (end seg))))
     seglist)))

((make-picture segments) base-square)

(define p1 (lambda(i) "A"))
(define p2 (lambda(i) "B"))

(define (beside p1 p2 s)
  (lambda(rect)
    (let ((split-point (scale s (horiz rect))))
      (p1 (make-rect (orig rect) split-point (vert rect)))
      (p2 (make-rect (+vect (orig rect) split-point) (scale (- 1 s) (horiz rect)) (vert rect))))))

((beside p1 p2 0.5) (make-rect (make-vect 1 2) (make-vect 4 5) (make-vect 7 8)))

(define (flip p)
  (lambda(rect)
    (p (make-rect (horiz rect) (orig rect) (vert rect)))))

((flip p1) (make-rect (make-vect 0 0) (make-vect 1 0) (make-vect 0 1)))

