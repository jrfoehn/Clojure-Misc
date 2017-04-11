;;; ITESM CEM, November 18, 2015.
;;; Clojure Source File
;;; Activity: Macros
;;; Author: Kevin Raymundo Serrano Vilchis, 1169528

(ns macros)

;; Using syntax-quote
(defmacro my-or
  "Executes short circuit or"
  ([] nil)
  ([x] x)
  ([x & lst]
   `(let [t# ~x]
      (if t#
        t#
        (my-or ~@lst)))))

(macroexpand-1 `(my-or false :one nil :two false :three))

(my-or)
(my-or false :one nil :two false :three)
(my-or false false nil)
(my-or nil nil false)

(defmacro do-loop
  "Executes do-while or do-until depending on keyword"
  [& body]
  (let [exec (butlast body)
        mode (first (last body))
        test (last (last body))]
  `(cond
    (= :while ~mode) (loop []
                      (do ~@exec)
                      (when ~test (recur)))
    (= :until ~mode) (loop []
                      (do ~@exec)
                      (when (not ~test) (recur))))))

    (macroexpand-1 '(do-loop
                  (println @j)
                  (swap! j inc)
                  (:while (<= @j 5))))

(println "do-until")
(def i (atom 0))
(do-loop
  (println @i)
  (swap! i inc)
  (:until (= @i 5)))

(println "do-while")
(def j (atom 1))
(do-loop
  (println @j)
  (swap! j inc)
  (:while (<= @j 5)))

(println "Hola")

(defmacro def-pred
  [name arg & body]
  `(do (defn ~name
         ~arg
         ~@body)
       (defn
         ~(symbol (clojure.string/join "-" ["not" name]))
         ~arg
         ~@(butlast body) (not ~(last body)))))

(macroexpand-1 '(def-pred less-than-one? [x] (< x 1)))

(macroexpand-1 '(def-pred plural? [s] (println "check s in" s) (= \s (last s))))

(def-pred less-than-one? [x] (< x 1))
(less-than-one? 0)
(less-than-one? 2)
(not-less-than-one? 0)
(not-less-than-one? 2)

(def-pred plural? [s] (println "check s in" s) (= \s (last s)))
(plural? "boys")
(plural? "girl")
(not-plural? "boys")
(not-plural? "girl")

(macroexpand-1 '(fn-curry [y] (* x (+ y 1))))

(defmacro fn-curry [args & body]
  (if (= 1 (count args))
    `(fn ~(vector (first args)) (do ~@body))
    `(fn ~(vector (first args)) (fn-curry ~(rest args) ~@body))))

(defmacro defn-curry [name args & body]
  (if (<=(count args) 1)
    `(defn ~name ~args ~@body)
    `(defn ~name ~(vector (first args)) (fn-curry ~(rest args) ~@body))))

(macroexpand-1 '(defn-curry sum [a b c d] (prn 'args a b c d) (+ a b c d)))
(macroexpand-1 '(fn-curry (b c d) (prn (quote args) a b c d) (+ a b c d)))
(macroexpand-1 '(fn-curry (c d) (prn (quote args) a b c d) (+ a b c d)))
(macroexpand-1 '(fn-curry (d) (prn (quote args) a b c d) (+ a b c d)))

(defn-curry sum [a b c d] (prn 'args a b c d) (+ a b c d))
((((sum 1) 2) 3) 4)
((((sum 15) 8) 16) 42)

(defn-curry go [x y] (* x (+ y 1)))
((go 2) 3)
((go 3) 2)

(defn-curry add1 [x] (+ x 1))
(add1 0)
(add1 41)

(defn-curry hello [] "hello")
(hello)
