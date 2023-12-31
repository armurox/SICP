;; Define rational number constructors and selectors

(define (make-rat n d)
  (define g (gcd n d))
  (cons (/ n g) (/ d g)))

(define (numerator p)
  (car p))

(define (denominator p)
  (cdr p))

;; Define additiona of rational numbers
(define (rat+ x y)
  (make-rat (+ (* (numerator x) (denominator y)) (* (numerator y) (denominator x)))
	    (* (denominator x) (denominator y))))

;; Define subtraction of rational numbers
(define (rat- x y)
  (make-rat (- (* (numerator x) (denominator y)) (* (numerator y) (denominator x)))
     (* (denominator x) (denominator y))))

;; Define multiplication of rationals
(define (rat* x y)
  (make-rat (* (numerator x) (numerator y)) (* (denominator x) (denominator y))))


(rat+ (make-rat 1 2) (make-rat 1 4))  
(rat- (make-rat 1 2) (make-rat 1 4))
