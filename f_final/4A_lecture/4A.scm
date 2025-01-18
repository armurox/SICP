(define deriv-rulesA
  '(
    ((dd (?c c) (? v)) 0)
    ((dd (?v v) (? v)) 1)
    ((dd (?v u) (? v)) 0)
    ((dd (+ (? x1) (? x2)) (? v)) (+ (dd (: x1) (: v)) (dd (: x2) (: v))))
    ((dd (* (? x1) (? x2)) (? v)) (+ (* (: x1) (dd (: x2) (: v))) (* (: x2) (dd (: x1) (: v)))))
    ((dd (** (? x) (?c n)) (? v)) (* (* (: n) (** (: x) (- (: n) 1))) (dd (: x) (: v))))
    ))

;; Goal
(define d-simp
  (simplifier deriv-rules))


(define (atom? a)
  (and (not (pair? a)) (not (null? a))))

;; One possible implementation of extend-dict (assuming a dict comes as either `fail or a list of pairs )
(define (in? a b)
  (cond ((null? b) #f)
	((equal? a (car v)) #t)
	(else
	 (in? a (cdr b)))))

(define (extend-dict pattern expression dict)
  (let ((curr-match (cons (cdr pattern) expression)))
  (if (in? curr-match dict)
      `fail
      (cons curr-match dict))))
      

;; Matcher
(define (match p e dict)
  (cond ((equal? dict `fail) `fail)
	((atom? p)
	 (if (atom? e)
	     (if (equal? p e)
		 dict
		 `fail)
	     `fail))
	((arbitrary-constant? p)
	 (if (constant? e)
	     (extend-dict p e dict)
	     `fail))
	((arbitrary-variable p)
	 (if (variable? e)
	     (extend-dict p e dict)
	     `fail))
	((arbitrary-expression? p)
	 (extend-dict p e dict))
	(else
	 (match
	   (cdr p)
	   (cdr e)
	   (match
	     (car p)
	     (car e)
	     dict)))))
	     
	   
	     
      
