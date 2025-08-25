(define (my-cons a b)
  (lambda (x)
    (if (= x 1)
	a
	b)))

(define (my-car a)
  (a 1))

(define (my-cdr b)
  (b 2))

(define (make-rat a b)
  (define com (gcd a b))
  (my-cons (/ a com) (/ b com)))

(define (rat+ m n)
  (make-rat (+ (* (my-car m) (my-cdr n)) (* (my-cdr m) (my-car n))) (* (my-cdr m) (my-cdr n))))

(my-cdr (rat+ (make-rat 1 2) (make-rat 6 8)))

