;;; ITESM CEM, November 11, 2015.
;;; Clojure Source File
;;; Activity: Using Parallel Map
;;; Authors:
;;;          A01673007 Jean-Robin Foehn
;;;          A01672031 Kimberley Buatois

(ns parallel)

(defn isprime [n]
  (if (= 1 n)
    false
    (every? #(pos? (rem n %)) (range 2 (Math/sqrt (inc n))))))

(defn ranges [lim n-threads]
  (let [threshold (/ lim n-threads)]
    (map
     #(list (* threshold %) (* threshold (inc %)))
     (range n-threads))))

(ranges 5000000 4)

(map fchunk (ranges 10 2))

(defn sumprimes [n-threads]
  (let [fchunk (fn [[start finish]]
                  (loop [sum  0
                         i    start]
                    (if (= i finish)
                      sum
                      (recur (if (isprime i)
                               (+ sum i)
                               sum)
                             (inc i)))))]
    (reduce
     +
     (pmap fchunk (ranges 5000000 n-threads)))))

(sumprimes 4)

(time (println (parallel/sumprimes 1)))
(time (println (parallel/sumprimes 2)))
(time (println (parallel/sumprimes 4)))
(time (println (parallel/sumprimes 8)))
