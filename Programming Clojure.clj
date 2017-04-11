(ns book)

(defn hello [name] (str "Hello, " name))

(hello "JR")

(conj #{} "Stu")

(def visitor (atom #{}))
(swap! visitor conj "Stu")

[1 2 3]
(concat [1 2] [3 4])
(concat [1 [2 3] 4] [5 6])

(defrecord Book [title author])
(def b(->Book "1984" "George Orwell"))
b

'(+ 1 2) ;;quote: doesn't evaluate function

(let [[x y] [1 2 3]] [x y]) ;;destructure vector [1 2 3] to [1 2]

(defn is-small? [number]
  (if (< number 100) "yes" "no"))
(is-small? 15000)

(+ 1 2)




