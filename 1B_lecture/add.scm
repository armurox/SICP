(define (+ x y)
  (if (= x 0)
      y
      (+ (-1+ x) (1+ y))))

(+ 3 4)


(define (+ x y)
  (if (= x 0)
      y
      (1+ (+ (-1+ x) y))))

(+ 3 4)
