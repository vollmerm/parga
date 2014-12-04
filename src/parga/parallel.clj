(ns parga.parallel
  (:require [environ.core :refer [env]]))

(defn limited-pmap
  "This is just like Clojure's built-in `pmap` function, but it gets the number of
  threads from the system environment."
  ([f coll]
   (let [n (Integer/parseInt (env :num-threads))
         rets (map #(future (f %)) coll)
         step (fn step [[x & xs :as vs] fs]
                (lazy-seq
                 (if-let [s (seq fs)]
                   (cons (deref x) (step xs (rest s)))
                   (map deref vs))))]
     (step rets (drop n rets)))))
