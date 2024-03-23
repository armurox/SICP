;; 
;;  The play-loop procedure takes as its  arguments two prisoner's
;;  dilemma strategies, and plays an iterated game of approximately
;;  one hundred rounds.  A strategy is a procedure that takes
;;  two arguments: a history of the player's previous plays and 
;;  a history of the other player's previous plays.  The procedure
;;  returns either a "c" for cooperate or a "d" for defect.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (play-loop strat0 strat1)
  (define (play-loop-iter strat0 strat1 count history0 history1 limit)
    (cond ((= count limit) (print-out-results history0 history1 limit))
	  (else (let ((result0 (strat0 history0 history1))
		      (result1 (strat1 history1 history0)))
		  (play-loop-iter strat0 strat1 (+ count 1)
				  (extend-history result0 history0)
				  (extend-history result1 history1)
				  limit)))))
  (play-loop-iter strat0 strat1 0 the-empty-history the-empty-history
		  (+ 90 (random 21))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The following procedures are used to compute and print
;;  out the players' scores at the end of an iterated game
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-out-results history0 history1 number-of-games)
  (let ((scores (get-scores history0 history1)))
    (newline)
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)))

(define (get-scores history0 history1)
  (define (get-scores-helper history0 history1 score0 score1)
    (cond ((empty-history? history0)
	   (list score0 score1))
	  (else (let ((game (make-play (most-recent-play history0)
				       (most-recent-play history1))))
		  (get-scores-helper (rest-of-plays history0)
				     (rest-of-plays history1)
				     (+ (get-player-points 0 game) score0)
				     (+ (get-player-points 1 game) score1))))))
  (get-scores-helper history0 history1 0 0))

(define (get-player-points num game)
  (list-ref (get-point-list game) num))

(define *game-association-list*
  ;; format is that first sublist identifies the players' choices 
  ;; with "c" for cooperate and "d" for defect; and that second sublist 
  ;; specifies payout for each player
  '((("c" "c") (3 3))
    (("c" "d") (0 5))
    (("d" "c") (5 0))
    (("d" "d") (1 1))))

(define (get-point-list game)
  (cadr (extract-entry game *game-association-list*)))

;; note that you will need to write extract-entry

(define make-play list)

(define the-empty-history '())

(define extend-history cons)
(define empty-history? null?)

(define most-recent-play car)
(define rest-of-plays cdr)

;; A sampler of strategies

(define (NASTY my-history other-history)
  "d")

(define (PATSY my-history other-history)
  "c")

(define (SPASTIC my-history other-history)
  (if (= (random 2) 0)
      "c"
      "d"))

(define (EGALITARIAN  my-history other-history)
  (define (count-instances-of test hist)
    (cond ((empty-history? hist) 0)
	  ((string=? (most-recent-play hist) test)
	   (+ (count-instances-of test (rest-of-plays hist)) 1))
	  (else (count-instances-of test (rest-of-plays hist)))))
  (let ((ds (count-instances-of "d" other-history))
	(cs (count-instances-of "c" other-history)))
    (if (> ds cs) "d" "c")))

(define (EYE-FOR-EYE my-history other-history)
  (if (empty-history? my-history)
      "c"
      (most-recent-play other-history)))

;; My code starts here
;; Problem 1
(define (extract-entry play gal)
  (if (equal? play (car (car gal)))
      (car gal)
      (extract-entry play (cdr gal))))

;; Some test cases
(define a-play (make-play "c" "d"))
(define b-play (make-play "d" "d"))
(define c-play (make-play "d" "c"))
(define d-play (make-play "c" "c"))

(extract-entry a-play *game-association-list*) ; -> (("c" "d") (0 5))
(extract-entry b-play *game-association-list*) ; -> (("d" "d") (1 1))
(extract-entry c-play *game-association-list*) ; -> (("d" "c") (5 0))
(extract-entry d-play *game-association-list*) ; -> (("c" "c") (3 3))

;; Problem 2

;; I wrote a helper function to generate all possible combination of the loop, to make matrix generation easier
(define (generate-combination count1 count2)
  (if (null? (cdr count2))
      (if (null? (cdr count1))
	  (begin
	    (display "Strategy 1: ")
	    (display (car count1))
	    (newline)
	    (display "Strategy 2: ")
	    (display (car count2))
	  (play-loop (car count1) (car count2)))
	  (begin
	    (display "Strategy 1: ")
	    (display (car count1))
	    (newline)
	    (display "Strategy 2: ")
	    (display (car count2))
	    (play-loop (car count1) (car count2))
	    (generate-combination (cdr count1) (cdr count1)))
	  )
      (begin
	(display "Strategy 1: ")
	    (display (car count1))
	    (newline)
	    (display "Strategy 2: ")
	    (display (car count2))
	(play-loop (car count1) (car count2))
	(generate-combination count1 (cdr count2))
	)
      )
  )

;; Generate all the combinations
(generate-combination (list NASTY PATSY EGALITARIAN EYE-FOR-EYE SPASTIC) (list NASTY PATSY EGALITARIAN EYE-FOR-EYE SPASTIC))

#|
 ----------------------------------------------------------------------------
|             | NASTY     | PATSY     | EGALITARIAN | EYE-FOR-EYE | SPASTIC   | 
| NASTY       | 1   1     | 5   0     | 1.04 0.99   | 1.04 0.99   | 3.12 0.47 |
| PATSY       | 0   5     | 3   3     | 3    3      | 3    3      | 1.75 3.83 |
| EGALITARIAN | 0.99 1.04 | 3   3     | 3    3      | 3    3      | 2.41 1.98 |
| EYE-FOR-EYE | 0.99 1.04 | 3   3     | 3    3      | 3    3      | 2.32 2.36 |
| SPASTIC     | 0.48 3.08 | 4.12 1.32 | 2.93 2.01   | 2.41 2.41   | 2.12 2.38 |
 ----------------------------------------------------------------------------

Some key things to notice:
- EYE-FOR-EYE with Egalitarian and Patsy is always cooperating. It also matches with Spastic quite often, since it closely follows its pattern
- Egalitarian with itself also always cooperates, as the average is always towards cooperation
- Nasty always beats Patsy with full score but tends to have a lower score when playing against others (due to them reacting to it) 
|#

;; Problem 3
;; Iterative version of Egalitarian
#|
(define (Egalitarian my-history other-history)
  (define (majority-loop cs ds hist)
    (cond ((empty-history? hist) (if (> ds cs) “d” “c”))
	  ((string=? (most-recent-play hist) “c”)
	   (majority-loop (+ 1 cs) ds (rest-of-plays hist)))
	  (else
	   (majority-loop cs (+ 1 ds) (rest-of-plays hist)))))
  (majority-loop 0 0 other-history))


While the iterative version of the code provided is better space complexity wise (it is O(1)), it's time complexity is the same as the base Egalitarian: O(n). It might be slightly fatser because it doesn't do it twice i.e. 2N vs N, but for large N, that doesn't matter too much, especially on the scales we are comparing at. 
|#

;; Problem 4 and 5
(define (EYE-FOR-N-EYE n)
  (lambda(my-history other-history)
    (define (count i other-hist)
      (if (empty-history? other-hist)
	  "c"
	  (if (= i n)
	      "d"
	      (if (string=? (most-recent-play other-hist) "c")
		  "c"
		  (count (+ i 1) (rest-of-plays other-hist))))))
    (count 0 other-history)))

(generate-combination (list (EYE-FOR-N-EYE 2)) (list NASTY PATSY EGALITARIAN EYE-FOR-EYE SPASTIC))
(generate-combination (list (EYE-FOR-N-EYE 3)) (list NASTY PATSY EGALITARIAN EYE-FOR-EYE SPASTIC))
#|
The two strategies generally perform the same as eye-for-eye, except against spastic and nasty, where in the first case, since they cooperate more on average, they tend to lose more against spastic, while in the nasty case, the larger n is, the more rounds its looking for before it starts to defect against nasty, hence the cooperate command lasts longer.
|#

;; Problem 6
(define (make-rotating-strategy strat0 strat1 freq0 freq1)
  (lambda(my-history other-history)
    (define (count-rounds hist count)
      (if (empty-history? hist)
	  count
	  (count-rounds (cdr hist) (+ count 1))))
    (define (is-even? num)
      (= (remainder num 2) 0))
    (let ((num-rounds (count-rounds my-history 0)))
    (if (is-even? (floor (/ num-rounds freq0))) 
	(strat0 my-history other-history)
	(strat1 my-history other-history)
	))))

(define rotating-strat-eye-and-egalitarian (make-rotating-strategy EYE-FOR-EYE EGALITARIAN 2 5))

(generate-combination (list rotating-strat-eye-and-egalitarian)
		      (list rotating-strat-eye-and-egalitarian NASTY PATSY EGALITARIAN EYE-FOR-EYE SPASTIC))
#|
The newly generated strategy does worse than a pure eye-for-eye against nasty, but better than pure egalitarian, due the partway weightage on immediately responding to nasty. Against spastic, due to the weightage with egalitarian, it fluctuates more than eye-for-eye. Against the others, it cooperates, due to the nature of egalitarian
|#

;; Problem 7
(define (make-higher-order-spastic strats)
  (lambda (my-history other-history)
    (define (len li count)
      (if (null? li)
	  count
	  (len (cdr li) (+ count 1))))
    (define (loop-through s count)
      (let ((curr-strat-loc (remainder (len my-history 0) (len strats 0))))
	(if (= count curr-strat-loc)
	    ((car s) my-history other-history)
	    (loop-through (cdr s) (+ count 1)))))
    (loop-through strats 0)))

(play-loop (make-higher-order-spastic (list NASTY PATSY EGALITARIAN EYE-FOR-EYE)) NASTY)
(generate-combination
 (list (make-higher-order-spastic (list NASTY PATSY EGALITARIAN EYE-FOR-EYE)))
 (list (make-higher-order-spastic (list NASTY PATSY EGALITARIAN EYE-FOR-EYE))
       NASTY PATSY EGALITARIAN EYE-FOR-EYE SPASTIC))
      
#|
The newly generated strategy does quite badly against nasty, due to the constant usage of patsy reappearing every 4 rounds, but it does do better against patsy than pure patsy, due to the reappearance of nasty. It also does better against egalitarian, due to the fact it can insert a nasty every 4 rounds, but not enough to affect the average. It does the same as any against eye=to-eye, as eye-to-eye always responds to it (in that sense, the name is good, it does behave a lot like a higher-order-spastic function, constantly switching between strategies)
|#
;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; code to use in 3 player game
;;	    

;(define *game-association-list*
;  (list (list (list "c" "c" "c") (list 4 4 4))
;        (list (list "c" "c" "d") (list 2 2 5))
;        (list (list "c" "d" "c") (list 2 5 2))
;        (list (list "d" "c" "c") (list 5 2 2))
;        (list (list "c" "d" "d") (list 0 3 3))
;        (list (list "d" "c" "d") (list 3 0 3))
;        (list (list "d" "d" "c") (list 3 3 0))
;        (list (list "d" "d" "d") (list 1 1 1))))


;; in expected-values: #f = don't care 
;;                      X = actual-value needs to be #f or X 
;(define (test-entry expected-values actual-values) 
;   (cond ((null? expected-values) (null? actual-values)) 
;         ((null? actual-values) #f) 
;         ((or (not (car expected-values)) 
;              (not (car actual-values)) 
;              (= (car expected-values) (car actual-values))) 
;          (test-entry (cdr expected-values) (cdr actual-values))) 
;         (else #f))) 
;
;(define (is-he-a-fool? hist0 hist1 hist2) 
;   (test-entry (list 1 1 1) 
;               (get-probability-of-c 
;                (make-history-summary hist0 hist1 hist2))))
;
;(define (could-he-be-a-fool? hist0 hist1 hist2)
;  (test-entry (list 1 1 1)
;              (map (lambda (elt) 
;                      (cond ((null? elt) 1)
;                            ((= elt 1) 1)  
;                            (else 0)))
;                   (get-probability-of-c (make-history-summary hist0 
;                                                               hist1
;                                                               hist2)))))

