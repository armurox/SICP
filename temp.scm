(define (planar x y)
  (cons x y))

(define (real c)
  (car c))

(define (imaginary c)
  (cdr c))

(define (polar r a)
  (cons (* r (cos a)) (* r (sin a))))

(define (radius c)
  (sqrt (square (+ (real c) (imaginary c)))))

(define (angle c)
  (atan (imaginary c) (real c)))

(define (add c1 c2)
  (planar
   (+ (real c1) (real c2))
   (+ (imaginary c1) (imaginary c2))))

(define (multiply c1 c2)
  (polar
   (* (radius c1) (radius c2))
   (+ (angle c1) (angle c2))))
