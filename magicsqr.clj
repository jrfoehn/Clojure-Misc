; How to use clojure to quickly create a magic square.

; First, use clojure to create some data.

; Find all combinations that sum to 15.
(def all-combos (into #{} (let [r (range 1 10)]
                            (for [a r
                                  b r
                                  c r
                                  :when (and (not= a b)
                                             (not= a c)
                                             (not= c b)
                                             (= 15 (+ a b c)))]
                              (sort [a b c])))))

; How many unique combinations?
(count all-combos)
; => 8

; What are the combinations?
(doseq [n all-combos] (println n) )
; => (2 6 7)
;    (1 6 8)
;    (4 5 6)
;    (3 5 7)
;    (2 5 8)
;    (1 5 9)
;    (3 4 8)
;    (2 4 9)

; How often does each number appear in the combinations?
(frequencies (flatten (vec all-combos)))
; => { 1 2
;      2 3
;      3 2
;      4 3
;      5 4
;      6 3
;      7 2
;      8 3
;      9 2 }

; Second, analyze.  Some takeaways:
;
; - There are only 8 combinations that sum to 15.  This is convenient, because
;   we need exactly 8.
;
; - Only one value, 5, is involved in 4 sums.  This is also convenient, because
;   we only need one card for the middle.
;
; - There are four cards that are only involved in 2 sums: 4, 8, 2, 6.  Also
;   convenient, because we need 4 cards for the corners.
;
; Knowing this, you can rearrange playing cards and get yourself an answer in a
; couple of minutes.  Here's my result:
;
;    4 3 8
;    9 5 1
;    2 7 6