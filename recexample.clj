;;; Solution to recursive Clojure exercises.
;;; Code developed in class (27-Aug-2015).

(ns recexample)

(defn dup
  "Returns a new list which duplicates all the elements of lst."
  [lst]
  (if (empty? lst)
    ()
    (cons (first lst)
          (cons (first lst)
                (dup (rest lst))))))

(dup '(1 2 3))
(dup ())
(dup '(a b c d))

(defn biggest
  "Returns the biggest value of a and b."
  [a b]
  (if (> a b)
    a
    b))

(defn largest
  "Returns the largest element contained in lst, which must
  have at least one element."
  [lst]
  (if (= 1 (count lst))
    (first lst)
    (biggest (first lst) (largest (rest lst)))))

(largest '(3))
(largest '(4 6 1 8 0 -10 5))
(largest '(-4 -2 -10 -999 -5))

(defn how-many
  "Returns how many times value appears in lst with
possible nested lists."
  [value lst]
  (cond
   (empty? lst)            0
   (list? (first lst))     (+ (how-many value (first lst))
                              (how-many value (rest lst)))
   (= value (first lst))   (inc (how-many value (rest lst)))
   :else                   (how-many value (rest lst))))

(how-many 'x ())
(how-many 'x '(x))
(how-many 'x '(x y z x y z))
(how-many 'x '((x (y x)) (z (((x))) y z)))
