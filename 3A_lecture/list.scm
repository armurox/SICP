(define 1-to-4
  (cons 1 (cons 2 (cons 3 (cons 4 ())))))

(define 1-to-4
  (list 1 2 3 4))

(car (cdr 1-to-4)) ;2

(cdr 1-to-4) ;(2,3,4)

1-to-4 ;(1, 2, 3, 4)

(define (scale-list s l)
  (if (null? l)
      ()
      (cons (* s (car l)) (scale-list s (cdr l)))))

(scale-list 10 1-to-4)

(define (scale-list-1 s l)
  (define (reverse cur li)
    (if (null? li)
	cur
	(reverse (cons (car li) cur) (cdr li))))
  (define (helper curr lis)
    (if (null? lis)
	curr
	(helper (cons (* s (car lis)) curr) (cdr lis))))
  (helper () (reverse () l)))

(scale-list-1 10 1-to-4)

(define (map func l)
  (if (null? l)
      ()
      (cons (func (car l)) (map func (cdr l)))))

(map (lambda(i) (* 10 i)) 1-to-4)

;; Alternatively
(define (scale-list s l)
  (map (lambda(i) (* s i)) l))

(scale-list 10 1-to-4)

(map square 1-to-4)

(define (for-each func l)
  (if (null? l)
      "done"
      (begin
	(func (car l))
	(for-each func (cdr l)))))

(for-each display 1-to-4)x
