(define square
  (lambda (a) (* a a)))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 4 3) ;; -> 25

;; Procedure that leads to a recursive process for +
(define (+ x y)
  (if (= y 0)
      x
      (1+ (+ x (-1+ y)))))

(+ 4 5) ;; -> 9

;; Procedure that leads to an iterative process for x
(define (+ x y)
  (if (= y 0)
      x
      (+ (1+ x) (-1+ y))))


(+ 4 5) ;; -> 9

;; The below procedure can't be interpeted yet on my machine, I'll have to look into adding some graphic library to get it working, will come back to this, but is illustrative of a plotting procedure that is iterative in its nature. Here we are simply using Euler's method to plot a circle, which is first order numerical procedure for ODE's (more info can be found here: https://en.wikipedia.org/wiki/Euler_method)
(define (draw-circle x y)
  (plot x y) ;; Whatever plot may mean
  (draw-circle (- x (* y dt)) (+ y (* x dt))))


;; Recursive implementation of a fibonacci sequence
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(fib 4) ;; -> 3
(fib 10) ;; -> 55
(fib 20) ;; -> 6765
(fib 30) ;; -> 832040 (noticably slow)

;; Iterative implementation of a fibonacci sequence
(define (fib n)
  (define (iter curr next count)
    (if (= count n)
	curr
	(iter next (+ curr next) (1+ count))))
  (iter 0 1 0))

(fib 4) ;; -> 3
(fib 10) ;; -> 55
(fib 20) ;; -> 6765
(fib 30) ;; -> 832040 (noticably faster)

;; Towers of Hanoi, lets do the mathematical version first (recursive computation, given n towers, how many moves does it take to move from one pole to another?)
(define (towers-of-hanoi n)
  (if (= n 0)
      n
      (+ (* 2 (towers-of-hanoi (- n 1))) 1)))

(towers-of-hanoi 3) ;; -> 7

;; Iterative implementation of the above
(define (towers-of-hanoi n)
  (define (iter curr next count)
    (if (= count n)
	curr
	(iter next (+ (* 2 next) 1) (1+ count))))
  (iter 0 1 0))

(towers-of-hanoi 3) ;; -> 7

;; Finally, we can use the recurrence relation, derived mathematically as (2 ** n)- 1
(define (towers-of-hanoi n)
  (- (expt 2 n) 1))

(towers-of-hanoi 3) ;; -> 7

;; Below is an example of actually running the process based on the lecture itself (simply illustrative, this procedure doesn't actually do anything when run.
(define (move n from to spare)
  (cond ((= n 0) "done")
	(else
	 (move (-1+ n) from spare to)
	 (print-move from to)
	 (move (-1+ n) spare to from))))


      

