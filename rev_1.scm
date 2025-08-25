(define (num_moves n)
  (if (= n 1)
      1
      (+ (* 2 (num_moves (- n 1))) 1)))

(num_moves 2)

(define (make-rat a b)
  (let ((div (gcd a b)))
    (cons (/ a div) (/ b div))))

(define numerator car)

(define denominator cdr)

(make-rat 2 4)

(numerator (make-rat 2 4))

(define (repeat f n)
  (if (= n 1)
      f
      (lambda(y) (f ((repeat f (- n 1)) y)))))

((repeat square 3) 2)
