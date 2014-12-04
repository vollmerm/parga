;; ## Driver functions
;;
;; This is the *sequential* version. No fancy parallelism!

(ns parga.core
  (:use [parga.genetic])
  (:use [parga.strings]))

(defn build-world
  "Create a *world,* which is a collection of populations living on
  metaphorical *islands.*"
  [domain str-size pop-size island-count]
  (take island-count 
        (repeatedly #(generate-strings domain str-size pop-size))))

(defn run-island
  "This loop is for running the genetic algorithm on a single island. It
  repeats for a certain number of iterations, passing the population of
  strings through selection and mutation. When it's done, it returns a 
  map of the whole population and the best individual, in the form of
  `{:pop pop, :best best}`"
  [iters init-pop init-best func domain mutation-rate tournament-size]
  (loop [n iters, pop init-pop, best init-best]
    (if (zero? n) 
      { :pop pop, :best best }
      (let [ranked-pop (-> (if (nil? best) 
                             pop
                             (conj (rest pop) (:string best)))
                           (rank-pop func))
            best       (get-best ranked-pop)
            pop        (-> ranked-pop 
                           (tournament-selection tournament-size)
                           (mutation domain mutation-rate))]
        (recur (dec n) pop best)))))
        
(defn run-world
  "The outer loop: operates on a *world* full of *islands,* and calls 
  `run-island` on each one repeatedly. Ideally we would be sending random strings
  from island to island on little boats, but right now they just share the \"best\"
  string so far."
  [n1 n2 init-world func domain mutation-rate tournament-size logger]
  (letfn [(inner-world [pop best]
            (run-island n2 pop best func domain mutation-rate tournament-size))]
    (loop [n n1, world init-world, best nil]
      (if (zero? n) 
        { :world world, :best best }
        (let [map-world (map #(inner-world % best) world) ;; plain map
              best      (:best (apply min-key (comp :fitness :best) map-world))]
          (logger best)
          (recur (dec n) (map :pop map-world) best))))))

