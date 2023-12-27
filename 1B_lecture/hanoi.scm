(define (han n)
  (if (= n 1)
      1
      (+ (* (han (- n 1)) 2) 1)))

(define (pow x n)
  (if (= n 1)
      x
      (* x (pow x (- n 1)))))

(define (han-1 n)
  (- (pow 2 n) 1))

(pow 2 4)

(han 4)

(han-1 4)
