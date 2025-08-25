(define (deriv f)
  (define h 0.000000001)
  (lambda (x)
    (/ (- (f (+ x h)) (f x)) h)))

(define (square x)
  (* x x))

((deriv square) 8)
