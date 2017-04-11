;;; ITESM CEM, September 30, 2015.
;;; Clojure Source File
;;; Activity: Higher-Order Functions
;;; Author: Kevin Raymundo Serrano Vilchis, 1169528

(ns higherorder
  (:use clojure.test))

(defn aprox=
  "Checks if x is approximately equal to y. Returns true
  if |x - y| < epsilon, or false otherwise."
  [epsilon x y]
  (< (Math/abs (- x y)) epsilon))

;; --------------------------- my-map-indexed ---------------------------

(defn my-map-indexed
  "returns a list consisting of the result of applying f to 0 and the first item of lst,
  followed by applying f to 1 and the second item in lst, and so on until lst is exhausted.
  Function f should accept 2 arguments: index and item."
  [f lst]
  (if (empty? lst)
    ()
    (map f (range) lst)))

(my-map-indexed vector '(a b c))

(deftest test-my-map-indexed
  (is (= () (my-map-indexed vector ())))
  (is (= '([0 a] [1 b] [2 c] [3 d])
         (my-map-indexed vector '(a b c d))))
  (is (= '(10 4 -2 8 5 5 13)
         (my-map-indexed + '(10 3 -4 5 1 0 7))))
  (is (= '(0 1 -4 3 1 0 6)
         (my-map-indexed min '(10 3 -4 5 1 0 7)))))

;; --------------------------- my-drop while ---------------------------

(defn my-drop-while
  "takes two arguments: a function f and a list lst. It returns a list of items from lst
  dropping the initial items that evaluate to true when passed to f. Once a false value is
  encountered, the rest of the list is returned. Function f should accept one argument."
  [f lst]
  (cond
   (empty? lst)     ()
   (f (first lst))  (my-drop-while f (rest lst))
   :juana           lst))

(deftest test-my-drop-while
  (is (= () (my-drop-while neg? ())))
  (is (= '(0 1 2 3 4)
         (my-drop-while
           neg?
           '(-10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4))))
  (is (= '(2 three 4 five)
         (my-drop-while
           symbol?
           '(zero one 2 three 4 five))))
  (is (= '(0 one 2 three 4 five)
         (my-drop-while
           symbol?
           '(0 one 2 three 4 five)))))

;; --------------------------- bisection ---------------------------

(defn bisection
  "Find root of pomlynomial using bisection method"
  [a b f]
  (let [c (/ (+ a b) 2.0)]
    (cond
     (< (Math/abs (f c)) 1e-15)     c
     (< (* (f a) (f c)) 0)      (bisection a c f)
     :juana                     (bisection c b f))))

(bisection 1 4 (fn [x] (* (- x 3) (+ x 4))))

(deftest test-bisection
  (is (aprox= 0.0001 3.0 (bisection 1 4 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001 -4.0 (bisection -5 0 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001 Math/PI (bisection 1 4 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001 (* 2 Math/PI) (bisection 5 10 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001 1.618033988749895
                     (bisection 1 2 (fn [x] (- (* x x) x 1)))))
  (is (aprox= 0.0001 -0.6180339887498948
                     (bisection -10 1 (fn [x] (- (* x x) x 1))))))

;; --------------------------- derivative ---------------------------

(defn deriv
  [f h]
  (fn [x] (/ (- (f (+ x h)) (f x)) h)))

(defn f [x] (* x x x))
(def df (deriv f 0.001))
(def ddf (deriv df 0.001))
(def dddf (deriv ddf 0.001))

(deftest test-deriv
  (is (aprox= 0.05 75 (df 5)))
  (is (aprox= 0.05 30 (ddf 5)))
  (is (aprox= 0.05 6 (dddf 5))))

;; --------------------------- Simpson's rule ---------------------------

(defn integral
  [a b n f]
  (loop [k   0
         sum 0
         h   (/ (- b a) n)]
    (cond
     (= k n)   (* (/ h 3) (+ sum (f (+ a (* k h)))))
     (= k 0)   (recur (inc k) (+ sum (f a)) h)
     (odd? k)  (recur (inc k) (+ sum (* 4 (f (+ a (* k h))))) h)
     (even? k) (recur (inc k) (+ sum (* 2 (f (+ a (* k h))))) h))))

((fn [x] ((fn [y] (+ x y)) 2)) 3)

(deftest test-integral
  (is (= 1/4 (integral 0 1 10 (fn [x] (* x x x)))))
  (is (= 21/4
         (integral 1 2 10
           (fn [x]
             (integral 3 4 10
               (fn [y]
                 (* x y))))))))

(run-tests)
