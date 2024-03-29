(define (deriv exp var)
  (cond ((constant? exp var) 0)
	((same-var? exp var) 1)
	((sum? exp) (make-sum (deriv (a1 exp) var)
			      (deriv (a2 exp) var)))
	((product? exp) (make-sum
			 (make-prod (m1 exp) (deriv (m2 exp) var))
			 (make-prod (m2 exp) (deriv (m1 exp) var))))))

(define (constant? exp var)
  (and (atom? exp)
       (not (eq? exp var))))

(define (same-var? exp var)
  (and (atom? exp)
       (eq? exp var)))

(define (sum? exp)
  (and (not (atom? exp))
       (eq? (car exp) '+)))

(define (atom? exp)
  (and (not (list? exp))
       (not (null? exp))))

(define (make-sum a1 a2)
  (cond ((and (number? a1)
	   (number? a2))
	 (+ a1 a2))
	((and (number? a1) (= a1 0)) a2)
	((and (number? a2) (= a2 0)) a1)
	(else
	 (list '+ a1 a2))))

(define a1 cadr)
(define a2 caddr)

(define (product? exp)
  (and (not (atom? exp))
       (eq? (car exp) '*)))

(define (make-prod m1 m2)
  (cond ((and (number? m1)
	      (number? m2))
	 (* m1 m2))
	((and (number? m1) (= m1 1)) m2)
	((and (number? m2) (= m2 1)) m1)
	(else
	 (list '* m1 m2))))

(define m1 cadr)
(define m2 caddr)

(define x 1)
(define a 2)
(define b 4)
(define c 3)
(define foo
  (list '+ (list '* 'a (list '* 'x 'x))
     (list '+ (list '* 'b 'x)
	'c)))
(deriv foo 'x)

(deriv foo 'x)
(deriv (a2 foo) 'x)
(a2 foo)
(deriv (list '* 'b 'x) 'x)
(make-sum (deriv (list '* 'b 'x) 'x) (deriv 'c 'x))
(deriv 'c 'x)
