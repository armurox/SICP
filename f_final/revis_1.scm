(define (sqrt x)
  (define (avg a b)
    (/ (+ a b) 2))
  (define (square a)
    (* a a))
  (define (abs a)
    (if (< a 0)
	(- a)
	a))
  (define (good-enough? a)
    (< (abs (- (square a) x)) 0.0001))
  (define (try g)
    (if (good-enough? g)
	g
	(try (avg g (/ x g)))))
  (try 1))

(sqrt 1) ;; -> 1
(sqrt 4) ;; -> 21523361 / 10761680
(sqrt 2) ;; -> 577/408

(define (fixed-point f)
  (lambda(g)
    (define (abs a)
      (if (< a 0)
	  (- a)
	  a))
    (define (good-enough? old new)
      (< (abs (- old new)) 0.00001))
    (define (iter old new)
      (if (good-enough? old new)
	  new
	  (iter new (f new))))
    (iter g (f g))))

(define (avg a b)
  (/ (+ a b) 2))

(define (sqrt x)
  ((fixed-point (lambda(g) (avg g (/ x g)))) 1))

(sqrt 1) ;; -> 1
(sqrt 4) ;; -> 926510094425921/463255047212960

(define (avg-damp f)
  (lambda(x)
    (avg x (f x))))

((avg-damp square) 2) ;; -> 3

(define (sqrt x)
  ((fixed-point (avg-damp (lambda(g) (/ x g)))) 1))

(sqrt 4) ;; -> 926510094425921/463255047212960

(define (deriv f)
  (lambda(x)
    (let ((dx 0.00001))
      (/ (- (f (+ x dx)) (f x)) dx))))

(define (newton f)
  (lambda(g)
    (let ((df (deriv f)))
      ((fixed-point (lambda(a) (- a (/ (f a) (df a))))) g))))

(define (sqrt x)
  ((newton (lambda(a) (- (square a) x))) 1))

(sqrt 4) ;; -> 2.000
(sqrt 2) ;; -> 1.414


(define (atom? e) (and (not (null? e)) (not (pair? e))))
(define (deriv exp var)
  (define (cons? e) (and (atom? e) (not (eq? e var))))
  (define (var? e) (and (atom? e) (eq? e var)))
  (define s1 cadr)
  (define p1 cadr)
  (define s2 caddr)
  (define p2 caddr)
  (define (sum? e) (eq? '+ (car e)))
  (define (prod? e) (eq? '* (car e)))
  (define (make-sum a b)
    (cond ((and (number? a) (number? b)) (+ a b))
	  ((eq? a '0) b)
	  ((eq? b '0) a)
	  ((eq? a b) (list '* 2 a))
	  (else (list '+ a b))))
    (define (make-prod a b)
      (cond ((number? a) (number? b) (* a b))
	    ((eq? a '1) b)
	    ((eq? b '1) a)
	    ((eq? a '0) 0)
	    ((eq? b '0) 0)
	    ((eq? a b) (list '^ a 2))
	     (else (list '* a b))))
  (define (make-pow a p) (list '^ a p))
  (cond ((cons? exp) 0)
	((var? exp) 1)
	((sum? exp) (make-sum (deriv (s1 exp) var) (deriv (s2 exp) var)))
	((prod? exp) (make-sum (make-prod (p1 exp) (deriv (p2 exp) var)) (make-prod (p2 exp) (deriv (p1 exp) var))))))

(deriv '(+ (* x x) (+ (* b x) c)) 'x) ;; -> (+ (* 2 x) b)

(define (fib n)
  (define (iter old new count)
    (if (> count n)
	old
	(iter new (+ old new) (1+ count))))
  (iter 0 1 1))

(fib 5) ;; -> 5
