(define (sum a b)
  (if (> a b)
      0
      (+ a (sum (1+ a) b))))
(define (square a)
  (* a a))

(define (sum_s a b)
  (if (> a b)
      0
      (+ (square a) (sum_s (1+ a) b ))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(sum 1 4)
(sum_s 1 5)
(pi-sum 1 6)
