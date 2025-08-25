(+ 3 17.4 5)

(+ 3 (* 5 6) 8 2)

(+ 3 4 8)

(+ (* 3 (+ 7 19.5)) 4)

(+ (* 3 5)
   (* 47
      (- 20 6.8))
   12)


(define A (* 5 5))

(* A A)

(define B (+ A (* 5 A)))

B

(define square
  (lambda (x) (* x x)))

(square 3)

(+ A (/ B 5))

(define (absolute x)
  (cond ((< x 0) (- x))
	((= x 0) 0)
	((> x 0) x)))

(absolute -3)

(define (abs_1 x)
  (if (< x 0)
      (- x)
      x))

(abs_1 -1)
