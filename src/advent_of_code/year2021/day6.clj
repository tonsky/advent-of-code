(ns advent-of-code.year2021.day6
  (:require
   [clojure.java.io :as io]
   [clojure.java.math :as math]
   [clojure.string :as str]
   [clojure.set :as set]
   #_[clj-async-profiler.core :as profiler]))

(def example "3,4,3,1,2")

(def input "5,1,2,1,5,3,1,1,1,1,1,2,5,4,1,1,1,1,2,1,2,1,1,1,1,1,2,1,5,1,1,1,3,1,1,1,3,1,1,3,1,1,4,3,1,1,4,1,1,1,1,2,1,1,1,5,1,1,5,1,1,1,4,4,2,5,1,1,5,1,1,2,2,1,2,1,1,5,3,1,2,1,1,3,1,4,3,3,1,1,3,1,5,1,1,3,1,1,4,4,1,1,1,5,1,1,1,4,4,1,3,1,4,1,1,4,5,1,1,1,4,3,1,4,1,1,4,4,3,5,1,2,2,1,2,2,1,1,1,2,1,1,1,4,1,1,3,1,1,2,1,4,1,1,1,1,1,1,1,1,2,2,1,1,5,5,1,1,1,5,1,1,1,1,5,1,3,2,1,1,5,2,3,1,2,2,2,5,1,1,3,1,1,1,5,1,4,1,1,1,3,2,1,3,3,1,3,1,1,1,1,1,1,1,2,3,1,5,1,4,1,3,5,1,1,1,2,2,1,1,1,1,5,4,1,1,3,1,2,4,2,1,1,3,5,1,1,1,3,1,1,1,5,1,1,1,1,1,3,1,1,1,4,1,1,1,1,2,2,1,1,1,1,5,3,1,2,3,4,1,1,5,1,2,4,2,1,1,1,2,1,1,1,1,1,1,1,4,1,5")

(defn progress [fish]
  (reduce-kv
    (fn [fish' day count]
      (if (zero? day)
        (-> fish'
          (assoc 8 count)
          (update 6 (fnil + 0) count))
        (-> fish'
          (update (dec day) (fnil + 0) count))))
    {}
    fish))

(defn parse [s]
  (->> (str/split s #",")
    (mapv parse-long)
    (frequencies)))

(defn part1
  ([] (part1 input))
  ([input] (part1 input 80))
  ([input days]
    (loop [fish (parse input)
           day  0]
      (if (>= day days)
        (reduce + (vals fish))
        (recur (progress fish) (inc day))))))

(defn part2
  ([] (part2 input))
  ([input]
    (part1 input 256)))

(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2021.day6 :reload))
  (part1 example)
  (part1)
  (profiler/profile (part1))
  (part2 example)
  (part2))