;;; ITESM CEM, September 2, 2015.
;;; Clojure Source File
;;; Activity: Recursive Functions, Part I
;;; Author: Jean-Robin Foehn, 01673007

(ns Recursion)

;;Question 1
(defn test-my-count [lst]
  (if empty? lst
      (loop [n 0 lst lst]
        (if (next lst)
          (recur (+ 1 n) (next lst))
          (+ n 1)))))

;;Question 2
(defn add-list [lst]
    (loop [n lst]
      (+ n (first lst))
      (recur (+ n add-list[(rest lst)]))))

;;Question 3
(defn member [x lst]
  (loop [x lst]
    (true? (= x (first lst)))
    (recur (member [x (rest lst)]))))

;;Question 4
(defn list-of-symbols [lst]
    (true? (symbol (first lst)))
    (recur (list-of-symbols [(rest lst)])))

;;Question 5
(defn my-last [lst]
  (if (empty? lst)
    ())
  (first (reverse list)))

;;Question 6
(defn conj-end [x lst]
  (if (empty? lst)
    ())
  (cons(lst x)))

;;Question 7
(defn my-butlast [lst]
  (if (empty? lst)
    ())
  (reverse (rest (reverse lst))))

;;Question 8
;;I haven't found the answer...

;;Question 9
(defn my-reverse [lst]
  (loop [lst lst]
    (if (empty? lst)
      (recur (cons (first lst) (next lst))))))

;;Question 10
;; For the last question, I searched the documentation and have found a function called map that seems to do the trick
(defn deep-reverse [lst]
  (map deep-reverse (reverse lst)))
