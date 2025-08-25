(define (position a v u t)
  (+ u (* v t) (* 0.5 a (* t t))))

(position 0 0 0 0)

(position 0 0 20 0)

(position 0 5 10 10)

(position 2 2 2 2) ;; -> 10

(position 5 5 5 5) ;; -> 92.5

(define (root-1 a b c)
	   (define disc (- (square b) (* 4 a c)))
	     (if (< disc 0)
		 #f
		 (/ (+ (- b) (sqrt disc)) (* 2 a))))
