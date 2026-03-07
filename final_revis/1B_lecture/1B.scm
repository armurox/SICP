(define (square x)
  (* x x))

(define (sum-square a b)
  (+ (square a) (square b)))

(sum-square 1 2) ;; -> 5
(sum-square 3 4) ;; -> 25

(define (p) (p))


(define (test x y)
  (if (= x 0)
      x
      y)) ;; -> test to confirm if our application is applicative-order or normal order. Running it shows
;;that it is applicative order


;; Iterative addition
(define (+ x y)
  (if (= x 0)
      y
      (+ (-1+ x) (1+ y))))

(+ 1 2) ;; -> 3
(+ 4 5) ;; -> 9

;; Recursive addition

(define (+ x y)
  (if (= x 0)
      y
      (1+ (+ (-1+ x) y))))

(+ 1 2) ;; -> 3
(+ 4 5) ;; -> 9


