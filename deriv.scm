(define (deriv exp var)
  (cond ((constant? exp var) 0)
	((same-var? exp var) 1)
	((sum? exp) (make-sum (deriv (a1 exp) var) (deriv (a2 exp) var)))
	((product? exp) (make-sum
			 (make-prod (m1 exp) (deriv (m2 exp) var))
			 (make-prod (m2 exp) (deriv (m1 exp) var))))))

(define (constant? exp var)
  (and (atom? exp)
       (not (eq? exp var))))

(define (atom? exp)
  (and (not (list? exp))
       (not (null? exp))))

(define (same-var? exp var)
  (and (atom? exp)
       (eq? exp var)))

(define (sum? exp)
  (and (not (atom? exp))
       (eq? (car exp) '+)))

(define (make-sum a b)
  (cond ((and (number? a)
	       (number? b)) (+ a b))
	 ((and (number? a)
	       (= a 0)) b)
	 ((and (number? b)
	      (= b 0)) a)
	 (else
	  (list '+ a b))))

(define (a1 s)
  (cadr s))

(define (a2 s)
  (caddr s))


(define (product? exp)
  (and (not (atom? exp))
       (eq? (car exp) '*)))


(define (make-prod a b)
  (cond ((and (number? a)
	      (number? b)) (* a b))
	((and (number? a) (= a 1)) b)
	((and (number? b) (= b 1)) a)
	(else
	 (list '* a b))))

(define (m1 p)
  (cadr p))

(define (m2 p)
  (caddr p))

(define x 'x)

(define expression
  (list '+
	(list '* 'a (list '* 'x 'x))
	(list '+ (list '* 'b 'x) 'c)))
expression

(deriv expression x)
