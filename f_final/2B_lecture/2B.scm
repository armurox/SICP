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


(define make-vec cons)

(define xcoor car)

(define ycoor cdr)

(define (+vec v u)
  (make-vec (+ (xcoor v) (xcoor u)) (+ (ycoor v) (ycoor u))))

(define make-line cons)

(define p1 car)
(define p2 cdr)

(define (average a b)
  (/ (+ a b) 2))

(define (length l)
  (sqrt (+ (square (- (xcoor (p1 l)) (xcoor (p2 l)))) (square (- (ycoor (p1 l)) (ycoor (p2 l)))))))

(define (midpoint l)
  (let ((a (p1 l)) (b (p2 l)))
    (make-vec (average (xcoor a) (xcoor b)) (average (ycoor a) (ycoor b)))))
      
(define line-1 (make-line (make-vec 1 2) (make-vec 4 6)))

(length line-1) ;; -> 5
(midpoint line-1) ;; -> (5/2 . 4)


