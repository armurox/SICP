(define null '())
(define (my-map f a)
  (if (null? a)
      a
      (cons (f (car a)) (my-map f (cdr a)))))

(my-map square (list 1 4 6))

(define (iter-map f a)
  (define (iter store-temp next)
    (if (null? next)
	(cons store-temp ())
	(iter (cons store-temp (f (car next))) (cdr next))))
  (iter () a))

(iter-map square (list 1 2))

(define (my-reduce f initial a)
  (define (iter l result)
    (if (null? l)
	result
	(iter (cdr l) (f result (car l)))))
  (iter a initial))

(my-reduce + 0 (list 1 2 3 4)) ;; -> 10
(reduce + 0 (list 1 2 3 4)) ;; -> 10

(filter (lambda(a) (= a 4)) (list 1 2 3 4)) ;; -> 4

(define (my-filter f a)
  (define (iter l new-l)
    (cond ((null? l) (cons new-l ()))
	  ((f (car l)) (if (null? new-l) (iter (cdr l) (cons (car l) ())) (iter (cdr l) (cons new-l (car l)))))
	  (else
	   (iter (cdr l) new-l))))
  (iter a ()))

(my-filter (lambda(a) (= a 4)) (list 1 2 3 4 4 4))
(list 1 2 3 4)
(cons 1 (cons 2 (cons 3 (cons 4 null))))

(define 1-to-4 (list 1 2 3 4))

(define (my-for-each f l)
  (cond ((null? l)
      (display "done"))
	(else (f (car l)) (my-for-each f (cdr l)))))

(map display 1-to-4)
(my-for-each display 1-to-4)

;; Henderson-Escher Example
(define make-vec cons)
(define x-coor car)
(define y-coor cdr)
(define (+vec a b)
  (make-vec (+ (x-coor a) (x-coor b)) (+ (y-coor a) (y-coor b))))
(define (scale a b) (make-vec (* a (x-coor b)) (* a (y-coor b))))

(define (make-rect a b c) (cons a (cons b (cons c ()))))
(define origin car)
(define (horiz a) (car (cdr a)))
(define (vert a) (car (cdr (cdr a))))

(define (coord-map rect)
  (lambda(point)
    (+vec
     (origin rect)
     (+vec
      (scale (x-coor point) (horiz rect))
      (scale (y-coor point) (vert rect))
      )
     )
    )
  )

(define rect-1 (make-rect (make-vec 0 0) (make-vec 2 2) (make-vec 2 2)))
(define rect-2 (make-rect (make-vec 0 0) (make-vec 150 0) (make-vec 0 150)))

((coord-map rect-1) (make-vec 1 1)) ;; -> (4 . 4)
((coord-map rect-1) (make-vec 1 0)) ;; -> (2 . 2)

(map (coord-map rect-1) (list (make-vec 1 1) (make-vec 1 0) (make-vec 0 1))) ;; -> ((4 . 4) (2 . 2) (2 . 2))

(define make-segment cons)
(define seg-start car)
(define seg-end cdr)

(define (drawline v1 v2)
  (newline)
  (display "ctx.beginPath();")
  (display (string-append "ctx.moveTo(" (number->string (x-coor v1)) "," (number->string (y-coor v1)) ");"))
  (display (string-append "ctx.lineTo(" (number->string (x-coor v2)) "," (number->string (y-coor v2)) ");"))
  (display "ctx.stroke();"))
  

(define (make-picture seglist)
  (lambda(rect)
    (for-each (lambda(s)
		(drawline ((coord-map rect) (seg-start s)) ((coord-map rect) (seg-end s))))
	      seglist)))

(define p1 (make-picture (list (make-segment (make-vec 0 0) (make-vec 1 0)) (make-segment (make-vec 0 1) (make-vec 1 1)))))
(p1 rect-2)

(define (flip pic)
  (lambda(rec)
    (pic (make-rect (+vec (origin rec) (horiz rec)) (scale -1 (horiz rec)) (vert rec)))))

(define p2 (make-picture (list (make-segment (make-vec 0 0) (make-vec 1 0)) (make-segment (make-vec 0 1) (make-vec 1 1))
			       (make-segment (make-vec 0 0) (make-vec 1 1)))))

(p2 rect-2)
(define p3 (flip p2))

(p3 rect-2)

(define (beside pic-1 pic-2 a)
  (lambda(rect)
    (pic-1 (make-rect (origin rect) (scale a (horiz rect)) (vert rect)))
    (pic-2 (make-rect (+vec (origin rect) (scale a (horiz rect))) (scale (- 1 a) (horiz rect)) (vert rect)))))

(define p4 (beside p3 p2 0.5))
(p4 rect-2)

(define (right-push pic-1 a n)
  (if (< n 2)
      pic-1
      (beside pic-1 (right-push pic-1 a (- n 1)) a)))

(define p5 (right-push p4 0.5 4))

(p5 rect-2)

(define (repeated f n)
  (if (< n 2)
      f
      (lambda(x) (f ((repeated f (- n 1)) x)))))

((repeated square 2) 2) ;; -> 16

(define (push comb)
  (lambda(pic a n)
    ((repeated (lambda(p) (comb pic p a)) n) pic)))

(define right-push (push beside))

(define p6 (right-push p4 0.5 4))

(p6 rect-2)
