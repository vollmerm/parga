;; ## Knapsack problem

(ns parga.examples.knapsack
  (:use [parga.strings])
  (:use [parga.core])
  (:use [clojure.core.match :only [match]])
  (:gen-class :main true))

(def items-count 50)
(def items-weight (repeatedly items-count #(rand-int 100)))
(def items-worth (repeatedly items-count #(rand-int 100)))
(def pack-size 500)

(defn knapsack 
  [vals]
  (loop [i 0, v vals, s 0, w 0]
    (match [v]
           [([] :seq)] (if (> w pack-size) w (- 0 s))
           [([0 & r] :seq)] (recur (inc i) r s w)
           [([1 & r] :seq)] (recur (inc i) r 
                                   (nth items-worth i)
                                   (nth items-weight i))
           :else (throw (Throwable. (str "Expected binary list: " vals))))))

(def domain [1 0])
(def str-size 50)
(def pop-size 100)
(def island-count 4)
(def mutation-rate 0.3)
(def tournament-size 3)
(def knapsack-world (build-world domain str-size pop-size island-count))

(defn test-knapsack
  [n1 n2]
  (let [world (run-world n1 n2
                         knapsack-world knapsack domain mutation-rate
                         tournament-size println)
        answer (:string (:best world))]
    (println (str "Answer: " (seq answer) " -> " (knapsack answer)))))

(defn bench-knapsack
  [n]
  (time (test-knapsack n 100))
  (time (test-knapsack n 100)))

(defn -main
  [& args]
  (if (empty? args)
    (bench-knapsack 30)
    (bench-knapsack (read-string (first args)))))
