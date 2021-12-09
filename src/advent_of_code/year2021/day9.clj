(ns advent-of-code.year2021.day9
  (:require
   [advent-of-code.core :refer [cond+]]
   [clojure.java.io :as io]
   [clojure.java.math :as math]
   [clojure.string :as str]
   [clojure.set :as set]
   #_[clj-async-profiler.core :as profiler]))

(set! *warn-on-reflection* true)

(set! *print-namespace-maps* false)

(def example
"2199943210
3987894921
9856789892
8767896789
9899965678")

(def input (slurp "inputs/year2021/day9"))

(defn parse [input]
  (let [lines (str/split input #"\n")]
    (mapv
      (fn [line] (mapv #(parse-long (str %)) line))
      lines)))

(defn width [floor]
  (count (first floor)))

(defn height [floor]
  (count floor))

(defn val-at [floor [x y]]
  (nth (nth floor y) x))

(defn neighbours [floor [x y]]
  (cond-> []
    (> x 0)                    (conj [(dec x) y])
    (< x (dec (width floor)))  (conj [(inc x) y])
    (> y 0)                    (conj [x (dec y)])
    (< y (dec (height floor))) (conj [x (inc y)])))

(defn minimums [floor]
  (for [y (range 0 (height floor))
        x (range 0 (width floor))
        :let [neighbours (neighbours floor [x y])
              val        (val-at floor [x y])]
        :when (every? #(> (val-at floor %) val) neighbours)]
    [x y]))

(defn part1
  ([] (part1 input))
  ([input]
    (let [floor (parse input)]
      (->> (minimums floor)
        (map #(val-at floor %)) 
        (reduce #(+ %1 %2 1) 0)))))

(defn basin [floor start]
  (loop [border  [start]
         visited #{}]
    (cond+
      (empty? border)
      visited
      
      :let [pos (peek border)]
      
      (visited pos)
      (recur (pop border) visited)

      (= (val-at floor pos) 9)
      (recur (pop border) visited)

      :else
      (recur (into border (neighbours floor pos)) (conj visited pos)))))

(defn part2
  ([] (part2 input))
  ([input]
   (let [floor (parse input)]
     (->> (minimums floor)
       (map #(basin floor %))
       (into #{})
       (map count)
       (sort #(compare %2 %1))
       (take 3)
       (reduce * 1)))))

(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2021.day9 :reload))
  (part1 example)
  (part1)
  (profiler/profile (part1))
  (time (part2 example))
  (profiler/profile (part2 example))
  (part2))