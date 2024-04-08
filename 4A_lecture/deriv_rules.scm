;; As a first example, below are some derivative rules in a pattern-skeleton language form that we wish to work with
(define deriv-rules
  '(
    ((dd (?c c) (? v))       0)
    ((dd (?v v) (? v))       1)
    ((dd (?v u) (? v))       0)
    ((dd (+ (? x1) (? x2)) (? v))
     (+ (dd (: x1) (: v)) (dd (: x2) (: v))))
    ((dd (* (? x1) (? x2)) (? v))
     (+
      (* (: x1) (dd (: x2) (: v)))
      (* (: x2) (dd (: x1) (: v)))))))

#|
The generate form of the rules are as follows:
For pattern matching:
- foo -> foo
- (f a b) -> a list with elements being f, a, b
- (? x) -> any expression which we will label as x in our matching
- (?c x) -> any constant which we will label as x in our matching
- (?v x) -> any variable which we will label as x in our matching

For skeleton instantiantion:
- foo -> foo
- (f a b) -> a list with the elements being the instantianed form of f a b
- (: x) -> Whatever the pattern x matched with in the pattern matching sequence

The goal is to create a function, which will be called 'simplifier', which takes in a set of rules in this pattern-skeleton form, and return a function which can then take in any expression that works within the context of the rules, and spits out the correct evaluated form.
|#

;; Goal
(define d-simp
  (simplifier deriv-rules))

;; => (d-simp '(dd (+ x y) x)) -> (+ 1 0)

