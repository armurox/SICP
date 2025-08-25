(define (sqrt x)
  (define (avg m n)
    (/ (+ m n) 2))
  (define (improve a g)
    (if (< (abs (- g (avg g (/ a g)))) 0.001)
	g
	(improve a (avg g (/ a g)))))
  (improve x 1))

(sqrt 4)

(define (sqrt_1 x)
  (define (avg m n)
    (/ (+ m n) 2))
  (define (fixed-f func guess)
    (if (< (abs (- guess (func guess))) 0.001)
	guess
	(fixed-f func (func guess))))
  (fixed-f (lambda(i) (avg i (/ x i))) 1))


(sqrt_1 4)

(define (avg m n)
  (/ (+ m n) 2))

(define (fixed-f func guess)
  (if (< (abs (- guess (func guess))) 0.001)
      guess
      (fixed-f func (func guess))))


(define (newton func guess)
  (define (deriv f)
    (define df 0.000000001)
    (lambda(i) (/ (- (f (+ i df)) (f i)) df)))
  (fixed-f (lambda(x) (- x (/ (func x) ((deriv func) x)))) guess))

(define (sqrt_2 x)
  (newton (lambda(i) (- (* i i) x)) 1))

(sqrt_2 16)
