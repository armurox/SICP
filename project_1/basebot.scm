;;; Project 1, 6.001, Spring 2005

;;; idea is to simulate a baseball robot

;; imagine hitting a ball with an initial velocity of v 
;; at an angle alpha from the horizontal, at a height h
;; we would like to know how far the ball travels.

;; as a first step, we can just model this with simple physics
;; so the equations of motion for the ball have a vertical and a 
;; horizontal component

;; the vertical component is governed by
;; y(t) = v sin alpha t + h - 1/2 g t^2 
;; where g is the gravitational constant of 9.8 m/s^2

;; the horizontal component is governed by
;; x(t) = v cos alpha t
;; assuming it starts at the origin

;; First, we want to know when the ball hits the ground
;; this is governed by the quadratic equation, so we just need to know when 
;; y(t)=0 (i.e. for what t_impact is y(t_impact)= 0).
;; note that there are two solutions, only one makes sense physically

(define square
  (lambda (x) (* x x)))

;; these are constants that will be useful to us
(define gravity 9.8)  ;; in m/s
(define pi 3.14159)

;; Problem 1

(define position
  (lambda (a v u t)
     (+ (/ (* a (* t t)) 2) (* v t) u)))

;; you need to complete this procedure, then show some test cases

(position 0 0 0 0) ;; -> 0
(position 0 0 20 0) ;; -> 20
(position 0 5 10 10) ;; -> 60
(position 2 2 2 2)
(position 5 5 5 5)


;; Problem 2

(define root1
  (lambda (a b c)
     (define (disc a b c)
      (- (square b) (* 4 a c)))
    (if (< (disc a b c) 0)
	(> (disc a b c) 0)
	(/ (- (- b) (sqrt (disc a b c))) (* 2 a)))))

(define root2
  (lambda (a b c)
     (define (disc a b c)
      (- (square b) (* 4 a c)))
    (if (< (disc a b c) 0)
	(> (disc a b c) 0)
	(/ (+ (- b) (sqrt (disc a b c))) (* 2 a)))))

;; complete these procedures and show some test cases
(root1 1 5 6) ; -> -3
(root2 1 5 6) ; -> -2
;; Problem 3

(define time-to-impact
  (lambda (vertical-velocity elevation)
    (root1
     (- (/ gravity 2)) vertical-velocity elevation)))
(time-to-impact 4.9 0) ; -> 1
(time-to-impact 9.8 0) ; -> 2
;; Note that if we want to know when the ball drops to a particular height r 
;; (for receiver), we have

(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    (time-to-impact vertical-velocity (- elevation target-elevation))
    ))

(time-to-height 9.8 4.9 4.9) ; -> 2
(time-to-height 9.8 0 4.9) ; -> 1

;; Problem 4

;; once we can solve for t_impact, we can use it to figure out how far the ball went

;; conversion procedure
(define degree2radian
  (lambda (deg)
    (/ (*  deg pi) 180.)))

(define travel-distance-simple
  (lambda (elevation velocity angle)
    (* (* velocity (cos angle))
       (time-to-impact (* velocity (sin angle)) elevation))
    ))

;; let's try this out for some example values.  Note that we are going to 
;; do everything in metric units, but for quaint reasons it is easier to think
;; about things in English units, so we will need some conversions.

(define meters-to-feet
  (lambda (m)
    (/ (* m 39.6) 12)))

(define feet-to-meters
  (lambda (f)
    (/ (* f 12) 39.6)))

(define hours-to-seconds
  (lambda (h)
    (* h 3600)))

(define seconds-to-hours
  (lambda (s)
    (/ s 3600)))

(define radian2degree
  (lambda (r)
    (* (/ 180 pi) r)))
;; what is time to impact for a ball hit at a height of 1 meter
;; with a velocity of 45 m/s (which is about 100 miles/hour)
;; at an angle of 0 (straight horizontal)
;; at an angle of (/ pi 2) radians or 90 degrees (straight vertical)
;; at an angle of (/ pi 4) radians or 45 degrees

;; what is the distance traveled in each case?
;; record both in meters and in feet
(time-to-impact 0 1) ; -> .451739514526256 seconds
(travel-distance-simple 1 45 0) ; 20.33 meters

(time-to-impact (* 45 (sin (/ pi 4))) 1) ; 6.53 seconds
(travel-distance-simple 1 45 (/ pi 4)) ; 207.6 meters

(time-to-impact (* 45 (sin (/ pi 2))) 1) ; -> 9.21 seconds
(travel-distance-simple 1 45 (/ pi 2)) ; -> 0.00055 meters

(travel-distance-simple 1 45 1.56)
;; Problem 5

;; these sound pretty impressive, but we need to look at it more carefully

;; first, though, suppose we want to find the angle that gives the best
;; distance
;; assume that angle is between 0 and (/ pi 2) radians or between 0 and 90
;; degrees

(define alpha-increment 0.01)
(define find-best-angle
  (lambda (velocity elevation)
     (define (max-angle angle max)
      (if (< (- (/ pi 2) angle) 0.001)
	  max
	  (if (> (travel-distance-simple elevation velocity angle) (travel-distance-simple elevation velocity max))
	      (max-angle (+ angle 0.01) angle)
	      (max-angle (+ angle 0.01) max))))
    (max-angle 0 0)))

;; find best angle
;; try for other velocities
;; try for other heights
(radian2degree (find-best-angle 45 1))
(radian2degree (find-best-angle 90 400))

;; Problem 6

;; problem is that we are not accounting for drag on the ball (or on spin 
;; or other effects, but let's just stick with drag)
;;
;; Newton's equations basically say that ma = F, and here F is really two 
;; forces.  One is the effect of gravity, which is captured by mg.  The
;; second is due to drag, so we really have
;;
;; a = drag/m + gravity
;;
;; drag is captured by 1/2 C rho A vel^2, where
;; C is the drag coefficient (which is about 0.5 for baseball sized spheres)
;; rho is the density of air (which is about 1.25 kg/m^3 at sea level 
;; with moderate humidity, but is about 1.06 in Denver)
;; A is the surface area of the cross section of object, which is pi D^2/4 
;; where D is the diameter of the ball (which is about 0.074m for a baseball)
;; thus drag varies by the square of the velocity, with a scaling factor 
;; that can be computed

;; We would like to again compute distance , but taking into account 
;; drag.
;; Basically we can rework the equations to get four coupled linear 
;; differential equations
;; let u be the x component of velocity, and v be the y component of velocity
;; let x and y denote the two components of position (we are ignoring the 
;; third dimension and are assuming no spin so that a ball travels in a plane)
;; the equations are
;;
;; dx/dt = u
;; dy/dt = v
;; du/dt = -(drag_x/m + g_x)
;; dv/dt = -(drag_y/m + g_y)
;; we have g_x = - and g_y = - gravity
;; to get the components of the drag force, we need some trig.
;; let speed = (u^2+v^2)^(1/2), then
;; drag_x = - drag * u /speed
;; drag_y = - drag * v /speed
;; where drag = beta speed^2
;; and beta = 1/2 C rho pi D^2/4
;; note that we are taking direction into account here

;; we need the mass of a baseball -- which is about .15 kg.

;; so now we just need to write a procedure that performs a simple integration
;; of these equations -- there are more sophisticated methods but a simple one 
;; is just to step along by some step size in t and add up the values

;; dx = u dt
;; dy = v dt
;; du = - 1/m speed beta u dt
;; dv = - (1/m speed beta v + g) dt

;; initial conditions
;; u_0 = V cos alpha
;; v_0 = V sin alpha
;; y_0 = h
;; x_0 = 0

;; we want to start with these initial conditions, then take a step of size dt
;; (which could be say 0.1) and compute new values for each of these parameters
;; when y reaches the desired point (<= 0) we stop, and return the distance (x)

(define drag-coeff 0.5)
(define density 1.25)  ; kg/m^3
(define mass .145)  ; kg
(define diameter 0.074)  ; m
(define beta (* .5 drag-coeff density (* 3.14159 .25 (square diameter))))



















(define integrate
  (lambda (x0 y0 u0 v0 dt g m beta)
    (if (< y0 0)
	x0
	(integrate (+ x0 (* u0 dt))
		   (+ y0 (* v0 dt))
		   (+ u0 (- (/
			(* (sqrt
			    (+ (square u0) (square v0)))
			   u0 beta dt)
			m))) (+ v0 (* (- (+ (/
			 (* (sqrt
			     (+ (square u0) (square v0)))
			    v0 beta)
			 m)
			g))
				     dt))
			dt g m beta)
	)))

(define travel-distance
  (lambda (elevation velocity angle)
    (integrate 0 elevation (* velocity (cos angle)) (* velocity (sin angle)) 0.01 gravity mass beta)))


;; RUN SOME TEST CASES
(travel-distance 1 45 (/ pi 2)) ; -> 0.00024 meters
(travel-distance 1 45 (/ pi 4)) ; ->  92.23 meters
(travel-distance 1 45 0) ; -> 19.34 meters

;; what about Denver?

;; Problem 7
 
;; now let's turn this around.  Suppose we want to throw the ball.  The same
;; equations basically hold, except now we would like to know what angle to 
;; use, given a velocity, in order to reach a given height (receiver) at a 
;; given distance

;; Integration equation to compute time given equation
(define integrate-time
  (lambda (x0 y0 u0 v0 dt g m beta t)
    (if (< y0 0)
	t
	(integrate-time (+ x0 (* u0 dt))
		   (+ y0 (* v0 dt))
		   (+ u0 (- (/
			(* (sqrt
			    (+ (square u0) (square v0)))
			   u0 beta dt)
			m))) (+ v0 (* (- (+ (/
			 (* (sqrt
			     (+ (square u0) (square v0)))
			    v0 beta)
			 m)
			g))
				     dt))
			dt g m beta (+ t dt))
	)))

(define compute-time
  (lambda (elevation velocity angle)
    (integrate-time 0 elevation (* velocity (cos angle)) (* velocity (sin angle)) 0.01 gravity mass beta 0)))

(define compute-time-given-valid-trajectory
  (lambda (elevation velocity desired-distance)
    (define (try-angle e v angle)
      (if (> angle (/ pi 2))
	  0
	  (if (> (travel-distance e v angle) desired-distance)
	      (compute-time e v angle)
	      (try-angle e v (+ angle 0.01)))))
    (try-angle elevation velocity (- (/ pi 2)))))
  
  
;; a cather trying to throw someone out at second has to get it roughly 36 m
;; (or 120 ft) how quickly does the ball get there, if he throws at 55m/s,
;;  at 45m/s, at 35m/s?
(compute-time-given-valid-trajectory 0 35 36) ; -> 1.27 s
;; try out some times for distances (30, 60, 90 m) or (100, 200, 300 ft) 
;; using 45m/s
(compute-time-given-valid-trajectory 0 45 90) ; -> 3.75 s
(compute-time-given-valid-trajectory 0 45 60) ; -> 1.87 s
(compute-time-given-valid-trajectory 0 45 30) ; -> 0.79 s
(compute-time-given-valid-trajectory 0 45 9000) ; -> 0 s
;; Problem 8
(define travel-distance-with-bounce
  (lambda (elevation velocity angle num_bounces)
    (define (helper-travel-distance e v a n d)
      (if (= n 0)
	  d
	  (helper-travel-distance 0 (/ v 2) a (- n 1) (+ d (travel-distance e v a)))))
    (helper-travel-distance elevation velocity angle (+ num_bounces 1) 0)))

(travel-distance-with-bounce 0 45 (/ pi 4) 4) ; -> 145.04 meters
(travel-distance-with-bounce 0 45 (/ pi 4) 0) ; -> 91.69 meters
  
;; Problem 9
(define integrate-velocity
  (lambda (x0 y0 u0 v0 dt g m beta t)
    (if (< y0 0)
	(sqrt (+ (square u0) (square v0)))
	(integrate-time (+ x0 (* u0 dt))
		   (+ y0 (* v0 dt))
		   (+ u0 (- (/
			(* (sqrt
			    (+ (square u0) (square v0)))
			   u0 beta dt)
			m))) (+ v0 (* (- (+ (/
			 (* (sqrt
			     (+ (square u0) (square v0)))
			    v0 beta)
			 m)
			g))
				     dt))
			dt g m beta (+ t dt))
	)))

(define adjusted-travel-distance-with-bounce
  (lambda (elevation velocity angle num_bounces)
    (define (helper-travel-distance e v a n d t)
      (if (= n 0)
	  d
	  (helper-travel-distance 0 (integrate-velocity d e (* v (cos a)) (* v (sin a)) 0.01 gravity mass beta (+ t (compute-time e v a))) a (- n 1) (+ d (travel-distance e v a)) (+ t (compute-time e v a)))))
    (helper-travel-distance elevation velocity angle (+ num_bounces 1) 0 0)))

(adjusted-travel-distance-with-bounce 0 45 (/ pi 4) 4) ; -> 124.88 meters
(adjusted-travel-distance-with-bounce 0 45 (/ pi 4) 0) ; -> 91.69 meters
