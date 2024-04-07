(define (deriv exp var)
  (cond ((constant? exp var) 0)
	((variable? exp var) 1)
	((sum? exp) (make-sum (deriv (a1 exp) var) (deriv (a2 exp) var)))
	((product? exp) (make-sum
			 (make-product
			  (m1 exp) (deriv (m2 exp) var))
			 (make-product
			  (m2 exp) (deriv (m1 exp) var))))))

(define (atom? exp)
  (and (not (list? exp)) (not (null? exp))))

(define (constant? ex va)
  (and (atom? ex) (not (eq? ex va))))

(define (variable? exp var)
  (and (atom? exp) (eq? exp var)))

(define (sum? exp)
  (and (not (atom? exp)) (eq? (car exp) '+)))

(define (make-sum a b)
  (cond ((and (number? a) (number? b)) (+ a b))
	((and (number? a) (= 0 a)) b)
	((and (number? b) (= 0 b)) a)
	(else
	 (list '+ a b))))

(define (product? exp)
  (and (not (atom? exp)) (eq? (car exp) '*)))

(define (make-product a b)
  (cond ((and (number? a) (number? b)) (* a b))
	((and (number? a) (= 1 a)) b)
	((and (number? b) (= 1 b)) a)
	((and (number? a) (= 0 a)) 0)
	((and (number? b) (= 0 b)) 0)
	(else
	 (list '* a b))))

(define a1 cadr)
(define m1 cadr)
(define a2 caddr)
(define m2 caddr)

(define foo
  (list '+ (list '* 'a (list '* 'x 'x))
	(list '+ (list '* 'b 'x)
	      'c)))

(deriv foo 'x)
	 


