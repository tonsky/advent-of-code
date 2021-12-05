(ns advent-of-code.year2021.day5
  (:require
   [clojure.java.io :as io]
   [clojure.java.math :as math]
   [clojure.string :as str]
   [clojure.set :as set]
   #_[clj-async-profiler.core :as profiler]))

(defrecord Vent [x1 y1 x2 y2])

(defn parse-vent [s]
  (let [[x1 y1 x2 y2] (->> (re-matches #"\s*(\d+)\s*,\s*(\d+)\s*->\s*(\d+)\s*,\s*(\d+)\s*" s)
                        (next)
                        (mapv parse-long))]
    (Vent. x1 y1 x2 y2)))

(defn parse [input]
  (-> input
    (str/trim)
    (str/split #"\n")
    (->> (mapv parse-vent))))

(def example
  (slurp "inputs/year2021/day5.example"))

(def input
  (slurp "inputs/year2021/day5"))

(defn sign ^long [^long x]
  (cond
    (> x 0) 1
    (< x 0) -1
    (== x 0) 0))

(defn vent-seq [vent]
  (let [{:keys [^long x1 ^long x2 ^long y1 ^long y2]} vent
        steps (max (math/abs (- x2 x1)) (math/abs (- y2 y1)))
        dx    (sign (- x2 x1))
        dy    (sign (- y2 y1))]
    (mapv #(vector (+ x1 (* dx %)) (+ y1 (* dy %))) (range (inc steps)))))

(defn solve [vents]
  (let [width   (inc (reduce max (mapcat (juxt :x1 :x2) vents)))
        height  (inc (reduce max (mapcat (juxt :y1 :y2) vents)))
        matrix  ^longs (make-array Long/TYPE (* height width))
        xy->idx (fn [x y] (+ x (* y width)))
        *res    (volatile! 0)]
    (doseq [vent vents
            [x y] (vent-seq vent)
            :let [val (aget matrix (xy->idx x y))]]
      (aset matrix (xy->idx x y) (inc val))
      (when (== val 1)
        (vswap! *res inc)))
    @*res))

(defn part1
  ([] (part1 input))
  ([input]
    (solve (->> (parse input)
             (filter #(or (= (:x1 %) (:x2 %)) (= (:y1 %) (:y2 %))))))))

(defn part2
  ([] (part2 input))
  ([input]
    (solve (parse input))))

(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2021.day5 :reload))
  (part1 example)
  (part1)
  (profiler/profile (part1))
  (part2 example)
  (part2))