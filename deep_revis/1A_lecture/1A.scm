;; A file going through the initial relative simple programs presented in the first lecture
(+ 3 17.4 5) ;; -> 25.4
(+ 3 (* 5 6) 8 2) ;; -> 43
(+ 3 4 8) ;; -> 15

(+ (* 3 (+ 7 19.5)) 4) ;; -> 83.5

(define A (* 5 5)) ;; -> a

A ;; -> 25
(* A A) ;; -> 625
(define B (+ A (* 5 A)))
B ;; -> 150

(+ a (/ b 5)) ;; -> 55

;; Iterative implementation of a fibonacci function
(define (fib a)
  (define (iter curr next n)
    (if (or (= n a)  (> n a))
	curr
	(iter next (+ curr next) (1+ n))))
  (iter 0 1 0))

(fib 4) ;; -> 3

;; Recursive implementation of a fibonacci function
(define (fib-recur a)
  (cond ((or (< a 0) (= a 0)) 0)
	((= a 1) 1)
	(else
	 (+ (fib (- a 1)) (fib (- a 2))))))

(fib-recur 4) ;; -> 3

(define (square x) (* x x))

(square 10) ;; -> 100
(square (+ 5 7)) ;; -> 144
(square (square (square 1001))) ;; -> 1008028056070056028008001

(define (average x y)
  (/ (+ x y) 2))

(define (mean-square x y)
  (average (square x) (square y)))

(mean-square 1 2) ;; -> 5 / 2

;; Iterative implementation of addition of two numbers
(define (+ a b)
  (define (iter first second)
    (if (= second 0)
	first
	(iter (1+ first) (-1+ second))))
  (iter a b))


(+ 1 2) ;; -> 3

;; Recursive definition of addition of two numbers
(define (+ a b)
  (if (= b 0)
      a
      (1+ (+ a (-1+ b)))))

(+ 1 2) ;; -> 3

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(abs -4) ;; -> 4
(abs 8) ;; -> 8


