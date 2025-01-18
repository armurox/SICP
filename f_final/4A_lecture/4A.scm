(define deriv-rules
  '(
    ((dd (?c c) (? v)) 0)
    ((dd (?v v) (? v)) 1)
    ((dd (?v u) (? v)) 0)
    ((dd (+ (? x1) (? x2)) (? v)) (+ (dd (: x1) (: v)) (dd (: x2) (: v))))
    ((dd (* (? x1) (? x2)) (? v)) (+ (* (: x1) (dd (: x2) (: v))) (* (: x2) (dd (: x1) (: v)))))
    ((dd (** (? x) (?c n)) (? v)) (* (* (: n) (** (: x) (- (: n) 1))) (dd (: x) (: v))))
    ))

;; Goal
;;(define d-simp
  ;;(simplifier deriv-rules))


(define (atom? a)
  (and (not (pair? a)) (not (null? a))))

;; One possible implementation of extend-dict (assuming a dict comes as either `fail or a list of pairs )
(define (in? a b)
  (cond ((null? b) #f)
	((equal? a (car b)) #t)
	(else
	 (in? a (cdr b)))))

(define (extend-dict pattern expression dict)
  (let ((curr-match (cons (cdr pattern) expression)))
  (if (in? curr-match dict)
      `fail
      (cons curr-match dict))))
      

;; Matcher
(define (match p e dict)
  (define (arbitrary-constant? pattern)
    (equal? (car pattern) '?c))
  (define (arbitrary-variable pattern)
    (equal? (car pattern) '?v))
  (define (arbitrary-expression? pattern)
    (equal? (car pattern) '?))
  (define constant? atom?)
  (define variable atom?)
  (cond ((equal? dict 'fail) 'fail)
	((or (and (null? p) (not (null? e))) (and (null? e) (not (null? p)))) 'fail)
	((and (null? p) (null? e)) dict)
	((atom? p)
	 (if (atom? e)
	     (if (equal? p e)
		 dict
		 'fail)
	     'fail))
	((arbitrary-constant? p)
	 (if (constant? e)
	     (extend-dict p e dict)
	     'fail))
	((arbitrary-variable p)
	 (if (variable? e)
	     (extend-dict p e dict)
	     'fail))
	((arbitrary-expression? p)
	 (extend-dict p e dict))
	((atom? e) 'fail)
	(else
	 (match
	   (cdr p)
	   (cdr e)
	   (match
	     (car p)
	     (car e)
	     dict)))))

(match '((? v)) '((dd a) b) '()) ;; -> fail
(match '((? v) (? c)) '((dd a) b) '()) ;; -> (((c) . b) ((v) dd a))


;; Possible implementations of skeleton-evaluation?, eval-exp and evaluate until more detail is given
(define (skeleton-evaluation? s)
  (equal? (car s) ':))

(define (eval-exp s)
  (cdr s))

(define (evaluate a dict)
  (define (loop d)
    (cond ((null? d) #f)
	((equal? (car d) (car a)) (cdr d))
	(else
	 (let ((eval-result (loop (car d))))
	   (if eval-result
	       eval-result
	       (loop (cdr d))
	     )))))
  (loop dict))

;;Instantiator
(define (instantiate skeleton dict)
  (define (loop s)
    (cond ((null? s) s)
          ((atom? s) s)
	  ((skeleton-evaluation? s) (evaluate (eval-exp s) dict))
	  (else
	   (cons (loop (car s)) (loop (cdr s))))))
  (loop skeleton))


(instantiate '((: x) = (: x)) (cons (cons 'x 1) '())) ;; -> (1 = 1)
(instantiate '(foo (: x) = (: x)) (cons (cons 'x 1) '())) ;; -> (foo 1 = 1)

;; True-eval (will be explained later)
(define (true-evaluate form dict)
  (if (atom? form)
      (lookup form dict) ;; Essentially the evaluate I wrote above, I guess
      (apply
       (eval (lookup (car form) dict) user-initial-environment)
       (mapcar (lambda(x) (lookup v dict)) (cdr form)))))
