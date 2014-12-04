;; ## Helpers for the genetic algorithm

(ns parga.genetic
  (:use [parga.strings]))

;; ### Selection

(defn rank-one
  "Call fitness function on a single string and return a map
  with *:fitness* and *:string.*"
  [str func] { :fitness (func str),
               :string str })

(defn rank-pop
  "Rank individual strings based on a fitness function."
  [pop func]
  (map #(rank-one % func) pop))

(defn rank-compare
  "Compare two individual strings"
  [a b]
  (< (:fitness a) (:fitness b)))

(defn get-best
  "Find individual string with lowest (best) score."
  [fit-pop]
  (apply min-key :fitness fit-pop))

(defn crossover 
  "Perform crossover on two individual strings. Basically, we combine
  the two strings in some way to produce a new \"child\" string. Here
  we do a simple one-point crossover by chopping up two strings and
  combining one piece from each with append."
  [a b]
  (let [size (max (count a) (count b))
        point (rand-int size)]
    (concat (take point a)
            (take (- size point) (reverse b)))))

;; We're going to use tournament selection here. In tournament selection,
;; we pick strings out at random and have them compete in a "tournament,"
;; where the best two of them crossover and contribute a child string.
;; The larger the tournament size (in the code below, the *size* parameter),
;; the more wee favor *exploitation* over *exploration.*

(defn select-two 
  "Of a collection of individual strings, take out a certain number
  (the tournament size) of them, sort that subset, and return the 
  best two."
  [fit-pop size] 
  (take 2 (sort rank-compare (take size (shuffle fit-pop)))))

(defn inner-tournament
  "Run one round of the tournament, producing a new child string." 
  [fit-pop size]
  (apply crossover (map :string (select-two fit-pop size))))

(defn tournament-selection
  "Run tournament selection on a population. This function expects a 
  collection of maps containing strings and their fitnesses, and it
  produces a collection of new strings."
  [fit-pop size]
  (take (count fit-pop) 
        (repeatedly #(inner-tournament fit-pop size))))

;; ### Mutation

(defn mutation
  "Mutate every string in a population."
  [pop domain rate]
  (map #(mutate-string % domain rate) pop))

