(+ 3 17.4 5) ;; -> combination, operator applied to some operands

(+ 3 (* 5 6) 8 2) ;; -> 43

(define A (* 5 5)) ;; -> a

(* a a) ;; -> 625

(define square
  (lambda(a)
    (* a a))) ;; -> square

(square 10) ;; -> 100

(define (average x y)
  (/ (+ x y) 2))

(define (mean-square x y)
  (average (square x) (square y)))

(mean-square 3 4) ;; -> 25/2

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(abs -1) ;; -> 1
(abs 1) ;; -> 1

