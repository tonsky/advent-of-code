(ns advent-of-code.year2021.day7
  (:require
   [advent-of-code.core :as core]
   [clojure.java.io :as io]
   [clojure.java.math :as math]
   [clojure.string :as str]
   [clojure.set :as set]
   #_[clj-async-profiler.core :as profiler]))

(def example "16,1,2,0,4,2,7,1,2,14")

(def input (slurp "inputs/year2021/day7"))

(defn parse [input]
  (-> input
    (str/split #",")
    (->> (mapv parse-long))))

(defn part1
  ([] (part1 input))
  ([input]
   (let [xs (parse input)]
     (->>
       (for [res (range (reduce min xs) (inc (reduce max xs)))]
         (reduce (fn [acc x] (+ acc (core/abs (- x res)))) 0 xs))
       (reduce min)))))

(defn cost2 [from to]
  (let [dist (core/abs (- from to))]
    (quot (* dist (+ dist 1)) 2)))

(defn part2
  ([] (part2 input))
  ([input]
   (let [xs (parse input)]
    (->>
      (for [res (range (reduce min xs) (inc (reduce max xs)))]
        (reduce (fn [acc x] (+ acc (cost2 x res))) 0 xs))
      (reduce min)))))

(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2021.day7 :reload))
  (part1 example)
  (time (part1))
  (profiler/profile (part1))
  (part2 example)
  (time (part2)))