(define (own-cons a b)
  (lambda(x)
    (if (= x 0)
	a
	b)))

(define (own-car x)
  (x 0))

(define (own-cdr x)
  (x 1))

(own-cons 1 2)

(own-car (own-cons 1 2))

(own-cdr (own-cons 1 2))

(define (make-rat a b)
  (let ((save-gcd (gcd a b)))
    (cons (/ a save-gcd) (/ b save-gcd))))

(define numer car)
(define denom cdr)

(define (rat+ a b)
  (make-rat (+ (* (numer a) (denom b)) (* (numer b) (denom a))) (* (denom a) (denom b))))

(define (rat* a b)
  (make-rat (* (numer a) (numer b)) (* (denom a) (denom b))))

(define (rat- a b)
  (make-rat (- (* (numer a) (denom b)) (* (numer b) (denom a))) (* (denom a) (denom b))))



(define a (make-rat 2 4))
(define b (make-rat 3 4))

(rat+ a b) ;; -> (5 . 4)
(rat* a b) ;; -> (3 . 8)


