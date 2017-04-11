
(ns higherorder
  (:use clojure.test))


(defn aprox=
  "Checks if x is approximately equal to y. Returns true
  if |x - y| < epsilon, or false otherwise."
  [epsilon x y]
  (< (Math/abs (- x y)) epsilon))

(defn my-map
  [fun lst]
  (if (empty? lst)
    ()
    (cons (fun (first lst)) (my-map fun (rest lst)))))

(defn power-set
  [lst]
  (if (empty? lst)
    '(())
    (let [r (power-set (rest lst))]
      (concat r
              (map #(cons (first lst) %) r)))))

(defn my-map-indexed
  [f lst]
  (if (empty? lst)
    '(())
    (let [r 0]
    (cons (f (first lst)(inc r)) (my-map-indexed f (rest lst))))))



(deftest test-my-map-indexed
  (is (= () (my-map-indexed vector ())))
  (is (= '([0 a] [1 b] [2 c] [3 d])
         (my-map-indexed vector '(a b c d))))
  (is (= '(10 4 -2 8 5 5 13)
         (my-map-indexed + '(10 3 -4 5 1 0 7))))
  (is (= '(0 1 -4 3 1 0 6)
         (my-map-indexed min '(10 3 -4 5 1 0 7)))))



(my-map-indexed vector '(10 3 4 5))

(defn combinations
  [lst n]
  (filter #(= n (count %)) (power-set lst)))

(defn my-drop-while
  [f lst]
  (if (empty? lst)
    '(())
     (if (f (first lst))
        (my-drop-while f (rest lst))
        lst)))



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

(my-drop-while neg? '(-10 -6 -8 2 3 4))
(my-drop-while symbol? '(zero one 2 three 4 five))

(defn aprox=
  "Checks if x is approximately equal to y. Returns true
  if |x - y| < epsilon, or false otherwise."
  [epsilon x y]
  (< (Math/abs (- x y)) epsilon))


(defn bisection
  [a b f]
  (let [c (/ (+ a b) 2)]
    (if (< (Math/abs (f(c))) 0.000000000000001)
      c
      (if (< (- (f (a)) (f (c)))(- (f(b))(f(c))))
        (bisection (f (a)) (f (c)) f)
        (bisection (f (c)) (f (b)) f)))))



(deftest test-bisection
  (is (aprox= 0.0001 3.0 (bisection 1 4 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001 -4.0 (bisection -5 0 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001 Math/PI (bisection 1 4 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001 (* 2 Math/PI) (bisection 5 10 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001 1.618033988749895
                     (bisection 1 2 (fn [x] (- (* x x) x 1)))))
  (is (aprox= 0.0001 -0.6180339887498948
                     (bisection -10 1 (fn [x] (- (* x x) x 1))))))


(bisection (1 4 (fn [x] (* (- x 3) (+ x 4)))))

(defn deriv
  [f h]

  (/ (- (f(+ x h)) f(x)) h)


(defn f [x] (* x x x))
(def df (deriv f 0.001))
(def ddf (deriv df 0.001))
(def dddf (deriv ddf 0.001))

(deftest test-deriv
  (is (aprox= 0.05 75 (df 5)))
  (is (aprox= 0.05 30 (ddf 5)))
  (is (aprox= 0.05 6 (dddf 5))))




(run-tests)

