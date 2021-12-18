(ns advent-of-code.year2021.day18
  (:require
   [advent-of-code.core :refer [cond+]]
   [clojure.data.priority-map :refer [priority-map]]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.java.math :as math]
   [clojure.math.combinatorics :as comb]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.zip :as z])
  (:import
   [java.io StringReader PushbackReader]))

(set! *warn-on-reflection* true)

(set! *print-namespace-maps* false)

(def example (slurp "inputs/year2021/day18.example"))

(def input (slurp "inputs/year2021/day18"))

(defn parse [input]
  (with-open [reader (PushbackReader. (StringReader. input))]
    (->> (repeatedly #(edn/read {:eof nil} reader))
      (take-while some?)
      vec)))

(defn move-left [z pred]
  (loop [z (z/prev z)]
    (cond
      (nil? z) nil
      (pred z) z
      :else    (recur (z/prev z)))))

(defn move-right [z pred]
  (loop [z (z/next z)]
    (cond
      (z/end? z) nil
      (pred z)   z
      :else      (recur (z/next z)))))

(defn explodes? [z]
  (and (vector? (z/node z)) (>= (count (z/path z)) 4)))

(defn simple? [z]
  (number? (z/node z)))

(defn explode-once [n]
  (let [z (z/vector-zip n)]
    (z/root
      (if-some [z (move-right z explodes?)]
        (let [[a b] (z/node z)
              z (if-some [z' (move-left z simple?)]
                  (-> z' (z/edit + a) (move-right explodes?))
                  z)
              z (z/replace z 0)]
          (if-some [z' (move-right z simple?)]
            (z/edit z' + b)
            z))
        z))))

(defn splits? [z]
  (let [node (z/node z)]
    (and (number? node) (>= node 10))))

(defn split-once [n]
  (let [z (z/vector-zip n)]
    (z/root
      (if-some [z (move-right z splits?)]
        (let [val (z/node z)]
          (z/replace z [(quot val 2) (quot (inc val) 2)]))
        z))))

(defn simplify [n]
  (let [n' (explode-once n)]
    (if (not= n' n)
      (recur n')
      (let [n' (split-once n)]
        (if (not= n' n)
          (recur n')
          n)))))

(defn magnitude [n]
  (if (number? n)
    n
    (let [[a b] n]
      (+ (* 3 (magnitude a)) (* 2 (magnitude b))))))

(defn part1
  ([] (part1 input))
  ([input]
   (magnitude
     (reduce
       (fn [acc n]
         (simplify [acc n]))
       (parse input)))))

(defn part2
  ([] (part2 input))
  ([input]
   (let [nums (parse input)
         pairs (->> (comb/selections nums 2)
                 (remove (fn [[a b]] (= a b))))]
     (reduce
       (fn [acc [a b]]
         (max acc (magnitude (simplify [a b]))))
       0
       pairs))))

(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2021.day18 :reload))
  (simplify [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]])
  (magnitude [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]])
  (part1 example)
  (part1 input)
  (part2 example)
  (part2 input)
  (require '[clj-async-profiler.core :as profiler])
  (profiler/profile (part2 example)))
