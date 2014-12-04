;; ## Sinbowl problem

(ns parga.examples.sinbowl
  (:use [parga.strings])
  (:use [parga.core])
  (:use [criterium.core]))

(defn scale-num 
  [x min max a b]
  (+ (/ (* (- b a) (- x min)) (- max min)) a))

(defn sinbowl 
  [value]
  (let [v (double value)]
    (- (* (Math/abs v) 0.1) (Math/sin v))))

(defn eval-sinbowl
  [str]
  (sinbowl (scale-num (binary-to-num str) 0 1048575 -120.0 240.0)))

(defn log-sinbowl
  [best]
  (println best))

(def domain [1 0])
(def str-size 20)
(def pop-size 10)
(def island-count 8)
(def mutation-rate 0.3)
(def tournament-size 3)
(def sinbowl-world (build-world domain str-size pop-size island-count))

(defn test-sinbowl
  [n1 n2]
  (let [world (run-world n1 n2 
                         sinbowl-world eval-sinbowl domain mutation-rate 
                         tournament-size log-sinbowl)
        answer (scale-num (binary-to-num (:string (:best world))) 
                          0 1048575 -120.0 240.0)]
    (println (str "Answer: " answer " -> " (sinbowl answer)))))

(defn bench-sinbowl
  []
  (bench (test-sinbowl 30 10)))
