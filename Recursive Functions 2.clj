;;; ITESM CEM, September 2, 2015.
;;; Clojure Source File
;;; Activity: Recursive Functions, Part II
;;; Author: Jean-Robin Foehn, 01673007

(ns recursion2)
(use 'clojure.test)

;;Question 1
(defn my-repeat [n x]
  (if (= n 0)
    ()
    (cons x (my-repeat (dec n) x))))

(deftest test-my-repeat
  (is (= () (my-repeat 0 'x)))
  (is (= '(6 6 6) (my-repeat 3 6)))
  (is (= '((ha ha) (ha ha) (ha ha)) (my-repeat 3 '(ha ha))))
  (is (= '(true true true true true) (my-repeat 5 true))))

;;Question 2
(defn invert-pairs [lst]
  (reduce #(cons %2 %1) lst))

(deftest test-invert-pairs
  (is (= () (invert-pairs ())))
  (is (= '([1 a][2 a][1 b][2 b]))(invert-pairs '([a 1][a 2][b 1][b 2])))
  (is (= '([1 January][2 February][3 March])
         (invert-pairs '([January 1][February 2][March 3])))))

;;Question 3
(defn enlist[lst]
  (if (empty? lst)
    ()
    (cons (list (first lst)) (enlist (rest lst)))))

(deftest test-enlist
  (is (= () (enlist ())))
  (is (= '((a) (b) (c)) (enlist '(a b c))))
  (is (= '(((1 2 3)) (4) ((5)) (7) (8)) (enlist '((1 2 3) 4 (5) 7 8)))))

;;Question 4
(defn my-interleave [a b]
  (if (or (empty? a) (empty? b))
    ()
    (cons (first a) (cons (first b) (my-interleave (rest a) (rest b))))))

(deftest test-my-interleave
  (is (= () (my-interleave () ())))
  (is (= () (my-interleave '(a) ())))
  (is (= () (my-interleave () '(1))))
  (is (= '(a 1 b 2 c 3 d 4 e 5) (my-interleave '(a b c d e) '(1 2 3 4 5))))
  (is (= '(a 1 b 2 c 3 d 4) (my-interleave '(a b c d e) '(1 2 3 4))))
  (is (= '(a 1 b 2 c 3 d 4) (my-interleave '(a b c d) '(1 2 3 4 5))))
  (is (= '(a 1) (my-interleave '(a) '(1 2 3 4 5))))
  (is (= '(a 1) (my-interleave '(a b c d e) '(1)))))

;;Question 5
(defn my-flatten [lst]
  (reduce (fn [lst1 lst2] (if (list? lst2)
                      (concat lst1 (my-flatten lst2))
                      (concat lst1 (list lst2)))
            )
          '()
          lst))

(deftest test-my-flatten
  (is (= () (my-flatten ())))
  (is (= '(a b c d e) (my-flatten '((a b) ((c) d (e))))))
  (is (= '(one two three four)
         (my-flatten '(((one) ((two))) () (three (())) four)))))

;;Question 6
(defn exchange [x1 x2 lst]
  (cond (empty? lst) ()
        (list? (first lst))(cons (exchange x1 x2 (first lst)) (exchange x1 x2 (rest lst)))
        (= x1 (first lst)) (cons x2 (exchange x1 x2 (rest lst)))
        (= x2 (first lst)) (cons x1 (exchange x1 x2 (rest lst)))
               :else (cons (first lst) (exchange x1 x2 (rest lst)))))

(deftest test-exchange
  (is (= () (exchange 'x 'y ())))
  (is (= '(d b c a) (exchange 'a 'd '(a b c d))))
  (is (= '((42) true ((cool (true)) (42))))
         (exchange true 42 '((true) 42 ((cool (42)) (true))))))

;;Question 7
(defn insert [n lst]
  (cond (empty? lst)(list n)
        (> (first lst) n) (conj lst n)
        :else
        (conj (insert n (rest lst)) (first lst))))

(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))

;;Quesiton 8
(defn my-sort [lst]
  (if (empty? lst)
    ()
    (insert (first lst) (my-sort (rest lst)))))

(deftest test-my-sort
  (is (= () (my-sort ())))
  (is (= '(0 1 3 3 4 6 7 8 9) (my-sort '(4 3 6 8 3 0 9 1 7))))
  (is (= '(1 2 3 4 5 6) (my-sort '(1 2 3 4 5 6))))
  (is (= '(1 5 5 5 5 5 5) (my-sort '(5 5 5 1 5 5 5)))))

;;Question 9
(defn binary [n]
  (if (= 0 n)
    ()
    (conj (vec (binary (quot n 2))) (mod n 2))))

(deftest test-binary
  (is (= () (binary 0)))
  (is (= '(1 1 1 1 0) (binary 30)))
  (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))

;;Question 11
(defn compress [lst]
  (map first (partition-by identity lst)))

(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e) (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))


;;Question 12
(defn pack [lst]
  (partition-by identity lst))

(deftest test-pack
  (is (= () (pack ())))
  (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
         (pack '(a a a a b c c a a d e e e e))))
  (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
  (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))

;;Question 13
(defn encode [lst]
  (map #(list (count %) (first %))
       (pack lst)))

(deftest test-encode
  (is (= () (encode ())))
  (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
         (encode '(a a a a b c c a a d e e e e))))
  (is (= '([1 1] [1 2] [1 3] [1 4] [1 5]) (encode '(1 2 3 4 5))))
  (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))

;;Question 15
(defn decode [lst]
  (flatten (map (fn [n x] (repeat n x)) lst)))

(deftest test-decode
  (is (= () (decode ())))
  (is (= '(a a a a b c c a a d e e e e)
         (decode '([4 a] b [2 c] [2 a] d [4 e]))))
  (is (= '(1 2 3 4 5) (decode '(1 2 3 4 5))))
  (is (= '(9 9 9 9 9 9 9 9 9) (decode '([9 9])))))



(run-tests)
