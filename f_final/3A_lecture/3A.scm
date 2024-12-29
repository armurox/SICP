
(define null '())
(define (my-map f a)
  (if (null? a)
      a
      (cons (f (car a)) (my-map f (cdr a)))))

(my-map square (list 1 4 6))

(define (iter-map f a)
  (define (iter store-temp next)
    (if (null? next)
	(cons store-temp ())
	(iter (cons store-temp (f (car next))) (cdr next))))
  (iter () a))

(iter-map square (list 1 2))

(define (my-reduce f initial a)
  (define (iter l result)
    (if (null? l)
	result
	(iter (cdr l) (f result (car l)))))
  (iter a initial))

(my-reduce + 0 (list 1 2 3 4)) ;; -> 10
(reduce + 0 (list 1 2 3 4)) ;; -> 10

(filter (lambda(a) (= a 4)) (list 1 2 3 4)) ;; -> 4

(define (my-filter f a)
  (define (iter l new-l)
    (cond ((null? l) (cons new-l ()))
	  ((f (car l)) (if (null? new-l) (iter (cdr l) (cons (car l) ())) (iter (cdr l) (cons new-l (car l)))))
	  (else
	   (iter (cdr l) new-l))))
  (iter a ()))

(my-filter (lambda(a) (= a 4)) (list 1 2 3 4 4 4))
(list 1 2 3 4)
(cons 1 (cons 2 (cons 3 (cons 4 null))))

(define 1-to-4 (list 1 2 3 4))

(define (my-for-each f l)
  (cond ((null? l)
      (display "done"))
	(else (f (car l)) (my-for-each f (cdr l)))))

(map display 1-to-4)
(my-for-each display 1-to-4)
	
