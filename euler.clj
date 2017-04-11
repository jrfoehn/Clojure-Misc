(ns euler);

;;Problem 1

(defn Problem1 [n]
        (reduce + (filter #(or
                            (zero?(mod % 3))
                            (zero?(mod % 5)))
                          (range 1 n))))
(Problem1 1000)

;;Problem2

(defn fibo [] (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1]))) ;;Here I used Halloway's code to get the Fibonacci numbers

(defn Problem2 [n]
  (->> (fibo)
   (take-while #(<= % n))
       (filter even?)
       (reduce +)))

(Problem2 4000000)

;;Problem3

(defn prime [n]
    (if
      (or (not (integer? n))
          (< n 2) (and (> n 2)
                       (even? n))) false
      (not-any? true?
                (map #(zero? (mod n %))
                           (range 2 n)))))

(defn Problem3 [n]
  (first (drop-while #(not (prime %))
    (map #(/ n %)
         (iterate inc 2)))))

(Problem3 600851475143)


;;Problem4

(defn palindromic [n]
  (let [string (str n)]
    (= string (apply str (reverse string)))))

(defn product [n]
  (boolean (first (drop-while
                   #(not (and (zero? (mod n %)) (= n (count (str (/ n %))))))
                   (range max (quot max 10) -1)))))

(defn Problem4 [n]
  (let [max (dec (10 n))]
    (first (drop-while #(not (product %))
                       (filter palindromic (iterate dec (* max max)))))))

(Problem4 3)

;;Problem5

(defn divisible [n div]
    (reduce #(and %1 (zero? (mod n %2))) true div))

(defn Problem5 [n]
  (first (drop-while #(not (divisible % (range 1 (inc n))))
    (range n Integer/MAX_VALUE n))))

(Problem5 20)

;;Problem6

(defn square [x]
  (* x x))

(defn sum-square [n]
  (apply + (map square (range 1 (+ n 1)))))

(defn square-sum [n]
  (square (apply + (range 1 (+ n 1)))))

(defn Problem6 [n]
  (- (square-sum n) (sum-square n)))

(Problem6 100)

;;Problem7

(defn is-prime [n]
  (cond (<= n 1) false
        (= n 2) true
        :else (loop [f 2]
                (cond (zero? (mod n f)) false
                      (> f (Math/sqrt n)) true
                      :else (recur (inc f))))))

(defn problem7 [n]
  ([n] (first ((dec n) (filter is-prime (iterate inc 1))))))

(Problem7 10001)

;;Problem9

(defn Problem9 [sum]
  (for [a (range 1 sum)
        b (range a sum)
        c [(- sum a b)]
        :when (= (+ (* a a) (* b b)) (* c c))]
    (bigint (* a b c))))

(Problem9 1000)

;;Problem10

(defn primesgen []
  (letfn [(enqueue [sieve n step]
            (let [m (+ n step)]
              (if (sieve m)
                (recur sieve m step)
                (assoc sieve m step))))
          (next-sieve [sieve n]
            (if-let [step (sieve n)]
              (-> sieve
                  (dissoc n)
                  (enqueue n step))
              (enqueue sieve n (+ n n))))
          (next-primes [sieve n]
            (if (sieve n)
              (recur (next-sieve sieve n) (+ n 2))
              (cons n (lazy-seq (next-primes (next-sieve sieve n) (+ n 2))))))]
    (cons 2 (lazy-seq (next-primes {} 3)))))

(defn Problem10 [] (reduce + (take-while #(> 2000000 %) (primesgen))))

(Problem10)

;;Problem22


(def letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(def chartonum (apply hash-map (interleave letters (iterate inc 1))))

(defn score [s]
  "Returns the score of the letters of string s."
  (reduce + (map #(chartonum %) s)))

(defn get-names [str]
  (re-seq #"\w+" str))

(defn Problem22 [file]
  (let [names (sort (get-names (slurp file)))]
    (reduce + (map * (map score names) (iterate inc 1)))))

(Problem22 "/users/jrfoehn/Downloads/names.txt")

;;Problem48
(defn last-n [n string]
    (if (> (.length string) n)
      (.substring string (- (.length string) n))
      string))


  (defn term [n]
    (loop [i 1 x n]
      (if (= i n)
        (BigInteger. (last-n 10 (str x)))
        (recur (inc i) (BigInteger. (last-n 10 (str (*' x n))))))))

  (defn problem48 []
    (last-n 10 (str (apply +' (map term (range 1 1001))))))
  (problem48)
