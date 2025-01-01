(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define (deriv exp var)
  (define (cons? a)
    (and (atom? a) (not (eq? a var))))
  (define (var? a)
    (and (atom? a) (eq? a var)))
  (define (sum? a)
    (and (not (atom? a)) (eq? (car a) '+)))
  (define (product? a)
    (and (not (atom? a)) (eq? (car a) '*)))
  (define (make-sum a b)
    (cond ((and (number? a) (number? b)) (+ a b))
	   ((eq? a '0) b)
	  ((eq? b '0) a)
	  ((eq? a b) (list '* 2 a))
	  (else 
    (list '+ a b))))
  (define (make-product a b)
    (cond ((and (number? a) (number? b)) (* a b))
          ((eq? a '1) b)
	  ((eq? b '1) a)
	  ((or (eq? a '0) (eq? b '0)) '0)
	  (else 
    (list '* a b))))
  (cond ((cons? exp) 0)
	((var? exp) 1)
	((sum? exp) (make-sum (deriv (cadr exp) var) (deriv (caddr exp) var)))
	((product? exp) (make-sum (make-product (cadr exp) (deriv (caddr exp) var)) (make-product (caddr exp) (deriv (cadr exp) var))))))

(define foo '(+ (* x x) (+ (* b x) c)))
(deriv foo 'x) ;; -> (+ (* 2 x) b)
(deriv foo 'b) ;; -> x
(deriv foo 'c) ;; -> 1
(deriv '(* x x) 'x) ;; -> (* 2 x)
(deriv 'c 'x) ;; -> 0

