(define (cons a b)
  (lambda(x)
    (if (= x 0)
	a
	b)))

(define (car x)
  (x 0))

(define (cdr x)
  (x 1))

(cons 1 2)

(cdr (cons 1 2)) ;; -> 2

(define (gcd a b)
  (if (= a 0)
      b
      (gcd (modulo b a) a)))

(define (make-rat a b)
  (define common-factor (gcd a b))
  (cons (/ a common-factor) (/ b common-factor)))

(define numer car)

(define denom cdr)

(define (rat+ x y)
  (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))

(denom (rat+ (make-rat 1 2) (make-rat 1 2))) ;; -> 1


