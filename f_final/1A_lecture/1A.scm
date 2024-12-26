(define (average a b)
  (/ (+ a b) 2))

(define (square a)
  (* a a))

(define (mean-square a b)
  (average (square a) (square b)))

(mean-square 2 3)


