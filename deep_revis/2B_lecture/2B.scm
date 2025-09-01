(define a (cons 2 4))

(car a) ;; -> 2
(cdr a) ;; -> 4

;; Here we are using the Euclidean algorithm for the gcd
(define (gcd a b)
  (if (= a 0)
      b
      (gcd (modulo b a) a)))

(gcd 4 8) ;; -> 4
(gcd 20 30) ;; -> 10
(gcd 40 30) ;; -> 10

;; Construction of a rational number
(define (make-rat a b)
  (let ((greatest-common-divisor (gcd a b)))
    (cons (/ a greatest-common-divisor) (/ b greatest-common-divisor))))

;; Selection of the numerator and denominator
(define numer car)
(define denom cdr)

;; Some operations
(define (rat+ x y)
  (make-rat
   (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (rat- x y)
  (make-rat
   (- (* (numer x) (denom y)) (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (rat* x y)
  (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))

;; Testing
(define a (make-rat 4 8))
(define b (make-rat 8 10))
(numer a) ;; 1
(denom a) ;; -> 2

(rat+ a b) ;; -> (13 . 10)
(rat- a b) ;; -> (3 . -10)
(rat* a b) ;; -> (2 . 5)

;; composing
(rat* b (rat+ a b)) ;; -> (26 . 25)



