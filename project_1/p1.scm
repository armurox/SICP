(define position
  (lambda (a v u t)
    (+ (/ (* a (* t t)) 2) (* v t) u)))

(position 0 0 0 0)

(position 0 0 20 0)

(position 0 5 10 10)

(position 2 2 2 2)

(position 5 5 5 5)


(define root1
  (lambda (a b c)
    (define (disc a b c)
      (- (square b) (* 4 a c)))
    (if (< (disc a b c) 0)
	(> (disc a b c) 0)
	(/ (- (- b) (sqrt (disc a b c))) (* 2 a)))))

(root1 5 3 6)

(define time-to-impact
  (lambda (vertical-velocity elevation)
    (root1 (- 4.9) vertical-velocity elevation)))

(time-to-impact 4.9 0)

(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    (time-to-impact vertical-velocity (- elevation target-elevation))))

(time-to-height 9.8 0 4.9)

(define travel-distance-simple
  (lambda (elevation velocity angle)
    (* (* velocity (cos angle))
       (time-to-impact (* velocity (sin angle)) elevation))))

(define pi 3.14159265358979323)
(travel-distance-simple 1 45 (/ pi 2))

(define find-best-angle
  (lambda (velocity elevation)
    (define (max-angle angle max)
      (if (< (- (/ pi 2) angle) 0.001)
	  max
	  (if (> (travel-distance-simple elevation velocity angle) max)
	      (max-angle (+ angle 0.01) angle)
	      (max-angle (+ angle 0.01) max))))
    (max-angle 0 0)))

(find-best-angle 45 1)
	  
