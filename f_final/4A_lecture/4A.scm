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
  (if (null? b)
      #f
      (let ((curr-dict (car b)))
  (cond ((null? b) #f)
	((and (equal? (car a) (car curr-dict)) (not (equal? (cdr a) (cdr curr-dict)))) #t)
	(else
	 (in? a (cdr b)))))))

(define (extend-dict pattern expression dict)
  (let ((curr-match (cons (cadr pattern) expression)))
  (if (in? curr-match dict)
      `fail
      (cons curr-match dict))))



;; Matcher
(define (match p e dict)
  (define (arbitrary-constant? pattern)
    (equal? (car pattern) '?c))
  (define (arbitrary-variable? pattern)
    (equal? (car pattern) '?v))
  (define (arbitrary-expression? pattern)
    (equal? (car pattern) '?))
  (define constant? number?)
  (define variable? atom?)
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
	((arbitrary-variable? p)
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
(match '((? v) (? c)) '((dd a) b) '()) ;; -> ((c . b) ((v dd a))


;; Possible implementations of skeleton-evaluation?, eval-exp and evaluate until more detail is given
(define (skeleton-evaluation? s)
  (equal? (car s) ':))

(define (eval-exp s)
  (cdr s))

(define (evaluate a dict)
  (define (loop d)
    (cond ((null? d) #f)
	  ((atom? d) #f)
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
(instantiate '((: y) + foo (: x) = bar) (cons (cons 'x 1) (cons 'y 2))) ;; -> (2 + foo 1 = bar)
(instantiate '((: y) + foo (: x) - hello + (: z) = 3) (list (cons 'x 1) (cons 'y 2) (cons 'z 3))) ;; -> (2 + foo 1 - hello + 3 = 3)
;; True-eval (will be explained later)
(define (true-evaluate form dict)
  (if (atom? form)
      (lookup form dict) ;; Essentially the evaluate I wrote above, I guess
      (apply
       (eval (lookup (car form) dict) user-initial-environment)
       (mapcar (lambda(x) (lookup v dict)) (cdr form)))))

;; Test
(instantiate '((: c) + 1 + (: v) = foo) (match '((? v) (? c)) '((dd a) b) '())) ;; -> (b + 1 + (dd a) = foo)
(match '((? v) (?c c)) '((dd a) (b c)) '()) ;; -> fail

(define (compound? a) (not (atom? a)))

;; Test compound?
(compound? (cons 1 2)) ;; -> #t

;; GIGO Simplifier
(define (simplifier the-rules)
  (define pattern car)
  (define skeleton cdr)
  (define empty-dictionary `())
  (define (simplify-exp exp)
    (try-rules
     (if (compound? exp)
	 (map simplify-exp exp)
	 exp)))
  (define (try-rules exp)
    (define (scan-rules rules)
      (if (null? rules)
	  exp
	  (let ((dict (match (pattern (car rules)) exp empty-dictionary)))
	    (if (equal? dict 'fail)
		(scan-rules (cdr rules))
		(simplify-exp (instantiate (skeleton (car rules)) dict))))))
    (scan-rules the-rules))
  simplify-exp)

((simplifier '((((? v) + (? c)) ((: c) and (: v))))) '((a b) + c)) ; ((c and (a b))) 

(define d-simp
  (simplifier deriv-rules))

(d-simp '(dd (+ y x) y)) ;; -> ((+ (1) (0)))
(d-simp '(dd (+ x y) y));; -> ((+ (1) (0)))
