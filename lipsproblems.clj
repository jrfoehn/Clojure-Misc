(ns lisp-problems.lists)

(defn my-last [x]
    (first (reverse x)))

(defn my-last-2 [x]
    (if (= 1 (count x))
        (first x)
        (recur (rest x))))

(defn my-but-last [x]
    (if (= 2 (count x))
        (first x)
        (recur (rest x))))

(defn element-at [x n]
    (nth x n))

(defn element-at-2 [x n]
    (if (= n 0)
        (first x)
        (recur (rest x) (dec n))))

(defn number-of-elements [x]
    (count x))

(defn number-of-elements-2 [x]
    (loop [xs x total 0]
        (if (empty? xs)
            total
            (recur (rest xs) (inc total)))))

(defn number-of-elements-3 [x]
    (reduce (fn [l x] (inc l)) 0 x))

(defn my-reverse [lst]
    (reduce #(cons %2 %1) '() lst))

(defn palindrome [x]
    (= x (reverse x)))

; consumes stack...
(defn my-flatten [x]
    (reduce
        (fn [acc e]
            (if (list? e)
                (concat acc (my-flatten e))
                (concat acc (list e)))
        )
        '()
        x
    ))

(defn my-compress [lst]
    (reduce (fn [acc e]
        (cond
            (or (empty? acc)
                (not (= e (last acc))))
                (concat acc (list e))
            :default acc
        )
    ) '() lst))

(defn my-compress-2 [lst]
    (map first (partition-by identity lst)))

(defn my-pack [lst]
    (partition-by identity lst))

(defn my-pack-2 [lst]
    (-> (reduce
            (fn [acc e]
                (let [vals (-> (get acc e)
                            count
                            inc)]
                    (assoc acc e (repeat vals e)))
            )
            (hash-map)
            lst
        )
        vals
        reverse))

(defn my-encode [lst]
    (->> (partition-by identity lst)
         (map #(list (count %) (first %)))))

(defn my-encode-2 [lst]
    (map #(list (count %) (first %))
        (my-pack lst)))

(defn my-encode-mod [lst]
    (map #(let [[n val] %]
            (if (= 1 n) val %))
        (my-encode lst)))

(defn- explode-run-pair [[n x]]
    (repeat n x))

(defn- explode-run-element [x]
    (if (list? x)
        (explode-run-pair x)
        x))

(defn my-decode [lst]
    (flatten (map explode-run-element lst)))

(defn my-decode-2 [lst]
    (let [fuse #(if (list? %) (explode-run-pair %) (list %))
          explode-all (fn [acc e] (concat acc (fuse e)))]
        (reduce explode-all '() lst)))

(defn my-duplicate [lst]
    (reduce
        #(concat %1 (repeat 2 %2))
        '() lst))

(defn my-duplicate-2 [lst]
    (mapcat #(list % %) lst))

(defn my-duplicate-x [lst x]
    (mapcat #(repeat x %) lst))

(defn my-drop-every-x [lst n]
    "Drop every nth item"
    (->> (partition-all n lst)
         (map (partial take (dec n)))
         (flatten)))

(defn my-split-first
    "Split list to first x items and rest"
    [lst x]
    (let [spl (partition-all x lst)]
        (conj '()
            (flatten (rest spl))
            (first spl))))

(defn my-split-first-2 [lst x]
    (list (take x lst) (drop x lst)))

(defn my-splice
    "Splice a section of a list"
    [lst start end]
    (drop (dec start) (take end lst)))

(defn my-rotate [lst n]
    (loop [res lst c n]
        (if (= c 0)
            res
            (recur
                (cons (last res)
                      (take (dec (count res)) res))
                (dec c)))))

(defn my-rotate-2 [lst n]
    (let [total (count lst)
          rotate-by (mod (- total n) total)]
        (concat
            (drop rotate-by lst)
            (take rotate-by lst))))

(defn my-remove-at [lst n]
    (concat (take (dec n) lst)
            (drop n lst)))

(defn my-insert-at [x lst n]
    (let [[before after] (my-split-first-2 lst (dec n))]
        (concat before (list x) after)))

(defn my-range [start end]
    (loop [lst '() c end]
        (if (< c start)
            lst
            (recur (cons c lst) (dec c)))))

(defn my-random-elements
    "Extract a number of random elements fom a list"
    [lst n]
    (take n (shuffle lst)))

(defn my-random-numbers
    [n max]
    (my-random-elements (my-range 1 max) n))

(defn- my-rotations
    "Generate all rotations of the given list"
    [lst]
    (reduce
        (fn [acc e]
            (cons (my-rotate lst (count acc)) acc)
        )
        '() lst))

(defn- my-weave
    "Weave x into the list to generate permutations"
    [lst x]
    (let [size (+ 2 (count (first lst)))
          ranges (partial range 1 size)
          inserts (mapcat ranges (ranges))]
        (map
            #(my-insert-at x %1 %2)
            lst inserts
        )
    )
)

(defn my-permute
    "Generates all n length permutations of list"
    [lst n]
    (-> (my-rotations (butlast lst))
        (my-duplicate-x (count lst))
        (my-weave (last lst))))

(defn multi-split-list
    "Split a list into sublists of specified lengths"
    [lst lengths]
    (loop [result '()
           remaining lst
           index (count lengths)]
        (if (= 0 index)
            result
            (recur
                (cons (take (nth lengths index) remaining))
                (drop (nth lengths index))
                (dec index))
        )))

(defn group-subsets
    "Group list items into all permutations of 2, 3 and 4 subsets"
    [lst])
___________________________________________________________________
___________________________________________________________________

(deftest test-returns-last-element-of-list
  "Should return the last element of a list"
  (let [x (list 1 2 3 4 5)]
    (is (= (my-last x) 5))
    (is (= (my-last-2 x) 5))))

(deftest test-returns-last-but-one-of-list
  "Should return the last but one element of a list"
  (let [x (list 1 2 3 4 5)]
    (is (= (my-but-last x) 4))))

(deftest test-get-n-in-list
  "Get the nth element in a list"
  (let [x (list 1 2 3 4 5 6 7)]
   (is (= (element-at x 2) 3))
   (is (= (element-at-2 x 2) 3))))

(deftest test-counts-elements-in-list
  "Count the number of elements in a list"
  (let [x (list 1 2 3 4)
        y (list 9 8 7 6 5 4 3 2 1)]
    (is (= (number-of-elements y) 9))
    (is (= (number-of-elements-2 y) 9))
    (is (= (number-of-elements-3 y) 9))))

(deftest test-reverses-a-list
  "Can reverse the elements in a list"
  (let [x (list 1 2 3)]
    (is (= (my-reverse x) '(3 2 1)))))

(deftest test-palindrome
  (let [pal '(1 2 3 2 1)
        no-pal '(1 2 3 4 5)]
  (is (= (palindrome pal) true))
  (is (= (palindrome no-pal) false))))

(deftest test-flatten
    (let [lst '(1 2 (3 (4) 5) 6 7)
          flat (range 1 8)]
        (is (= (my-flatten lst) flat))
    ))

(deftest test-compress
    (let [lst '(a a a b b a a c c c c d d)
          cmp '(a b a c d)]
        (is (= (my-compress lst) cmp))
        (is (= (my-compress-2 lst) cmp))
    )
)

(deftest test-pack
    (let [lst '(a a a b b c)
          pck '((a a a) (b b) (c))]
        (is (= (my-pack lst) pck))
        (is (= (my-pack-2 lst) pck))
    )
)

(deftest test-encode
    (let [lst '(a a a a b c c a a d e e e e)
          ecd '((4 a) (1 b) (2 c) (2 a) (1 d)(4 e))]
        (is (= (my-encode lst) ecd))
        (is (= (my-encode-2 lst) ecd))
    ))

(deftest test-encode-modified
    (let [lst '(a a a b c c d e e e)
          ecd '((3 a) b (2 c) d (3 e))]
        (is (= (my-encode-mod lst) ecd))
    ))

(deftest test-decode-run-length
    (let [lst '((4 a) b (2 c) (3 e) a)
          dec '(a a a a b c c e e e a)]
        (is (= (my-decode lst) dec))
        (is (= (my-decode-2 lst) dec))
    ))

(deftest test-duplicate-list
    (let [lst '(a b c)
          dup '(a a b b c c)]
        (is (= (my-duplicate lst) dup))
        (is (= (my-duplicate-2 lst) dup))
    ))

(deftest test-duplicate-list-x-times
    (let [lst '(a b c)
          dup '(a a a b b b c c c)]
        (is (= (my-duplicate-x lst 3) dup))
    ))

(deftest test-drop-every-x-elements
    (let [lst '(a b c d e f g h i j k)
          res '(a b d e g h j k)]
        (is (= (my-drop-every-x lst 3) res))
    ))

(deftest test-split-first-x-items
    (let [lst '(a b c d e)]
        (is (= (my-split-first lst 2) '((a b) (c d e))))
        (is (= (my-split-first lst 3) '((a b c) (d e))))
    ))

(deftest test-split-first-x-items
    (let [lst '(a b c d e)]
        (is (= (my-split-first-2 lst 2) '((a b) (c d e))))
        (is (= (my-split-first-2 lst 3) '((a b c) (d e))))
    ))

(deftest test-splice-list
    (let [lst '(a b c d e f)]
        (is (= (my-splice lst 1 3) '(a b c)))
        (is (= (my-splice lst 3 4) '(c d)))
        (is (= (my-splice lst 3 10) '(c d e f)))
    ))

(deftest test-rotate-list
    (let [lst '(a b c d e f)]
        (is (= (my-rotate lst 3) '(d e f a b c)))
        (is (= (my-rotate lst 5) '(b c d e f a)))
        (is (= (my-rotate lst 11) '(b c d e f a)))
    ))

(deftest test-rotate-list
    (let [lst '(a b c d e f)]
        (is (= (my-rotate-2 lst 3) '(d e f a b c)))
        (is (= (my-rotate-2 lst 5) '(b c d e f a)))
        (is (= (my-rotate-2 lst 11) '(b c d e f a)))
    ))

(deftest test-remove-at
    (let [lst '(a b c d e)]
        (is (= (my-remove-at lst 3) '(a b d e)))
        (is (= (my-remove-at lst 1) '(b c d e)))
        (is (= (my-remove-at lst 10) '(a b c d e)))
    ))

(deftest test-insert-at
    (let [lst '(a b c d e)]
        (is (= (my-insert-at 'f lst 1) '(f a b c d e)))
        (is (= (my-insert-at 'f lst 2) '(a f b c d e)))
        (is (= (my-insert-at 'f lst 4) '(a b c f d e)))
    ))

(deftest test-range
    (is (= (my-range 4 9) '(4 5 6 7 8 9)))
    (is (= (my-range 3 4) '(3 4))))

(deftest test-random-extraction
    (let [lst '(a b c d e f g h)]
        (is (= (count (my-random-elements lst 3)) 3))
        (is (= (count (my-random-elements lst 2)) 2))
    ))

(deftest test-random-numbers
    (is (= (count (my-random-numbers 6 49)) 6))
)
