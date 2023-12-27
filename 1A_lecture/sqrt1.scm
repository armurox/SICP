(define (average x y) (/
		       (+ x y) 2))
(define (abs x) (if (< x 0) (- x)
		    x))


(define (sqrt x) 
		  (define (good-enough? guess) (< (abs (- (square guess) x)) 0.001))
		  (define (improve guess) (average guess (/ x guess)))
		  (define (try guess) (if (good-enough? guess) guess
					  (try (improve guess))))
		  (try 1)
						   
		  )

(sqrt 4)
