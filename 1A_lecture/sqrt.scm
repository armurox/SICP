(define (average x y) (/ (+ x y) 2))
(define (abs x) (if (< x 0) (- x)
		    x))
(define (try x g) (if (< (abs (- g (average g (/ x g)))) 0.001) g
		       (try x
			     (average g (/ x g)))))

(define (sqrt x) (try x 1))

(sqrt 4)
