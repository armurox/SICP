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

;;Instantiator

	     
      
