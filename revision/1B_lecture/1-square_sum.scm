(define sum-square
  (lambda(x y)
    (define (square a)
      (* a a))
    (+
     (square x)
     (square y)
     )))

(sum-square 3 4)

(define (+ x y)
  (if (= y 0)
      x
      (+ (1+ x) (-1+ y))))

(+ 2 3)

(define (+ x y)
  (if (= y 0)
      x
      (1+ (+ x (-1+ y)))))
(+ 2 3)

(define (fib n)
  (define (start a b s)
    (if (= s n)
	b
	(start (+ a b) a (1+ s))))
  (start 1 1 1))

(fib 6)
