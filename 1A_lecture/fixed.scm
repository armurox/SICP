(define (square x) (* x x))
(define (average x y) (/ (+ x y ) 2))
(define (mean-square x y) (average (square x) (square y)))
(mean-square 3 4)
(mean-square 2 3)
+
