(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (square a)
  (* a a))


(define (sum-int a b)
  (define (identity x) x)
  (sum identity a 1+ b))

(define (sum_s a b)
  (sum square a 1+ b))

(define (pi-sum a b)
  (sum
   (lambda(i) (/ 1 (* i (+ i 2))))
   a
   (lambda(i) (+ i 4))
   b))

(sum-int 1 4)
(sum_s 1 4)
(pi-sum 1 6)
