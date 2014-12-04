;; ## Functions for handling strings
;;
;; These functions manipulate strings of genetic material.

(ns parga.strings
  (:require [clojure.math.numeric-tower :as math])
  (:use [clojure.core.match :only [match]]))

;; ### Generating strings

(defn n-things-from
  "Return collection of *n* things randomly picked from a collection"
  [size domain]
  (take size (repeatedly #(rand-nth domain))))

(defn generate-strings
  "Generate random string by selecting elements from an alphabet set"
  [domain size num]
  (take num (repeatedly #(n-things-from size domain))))


;; ### General helpers

(defn binary-to-num
  "Convert a string representing a binary number into an integer"
  [binary]
  (loop [i 0, sum 0, b binary]
    (match [b]
           [([] :seq)] sum
           [([0 & r] :seq)] (recur (inc i) sum r)
           [([1 & r] :seq)] (recur (inc i) (+ sum (math/expt 2 i)) r)
           :else (throw (Throwable. (str "Expected binary list: " binary))))))

(defn mutate-string
  "Take a string, its alphabet set (domain), and a mutation rate,
  and return a new string. For each element in the string, we have
  a chance (determined by the mutation rate) of swapping it with
  a different element of the domain."
  [str domain rate]
  (map (fn [elm] 
         (if (< (rand) rate) 
           (rand-nth domain) 
           elm)) 
       str))
  
