(ns advent-of-code.year2021.day11
  (:require
   [advent-of-code.core :refer [cond+]]
   [clojure.java.io :as io]
   [clojure.java.math :as math]
   [clojure.string :as str]
   [clojure.set :as set]))

(set! *warn-on-reflection* true)

(set! *print-namespace-maps* false)

(def example (slurp "inputs/year2021/day11.example"))

(def input (slurp "inputs/year2021/day11"))

(defn parse [input]
  (->> input
    (re-seq #"\d")
    (map parse-long)
    (into-array Long/TYPE)))

(defrecord Point [^long x ^long y])

(defn floor-get ^long [^longs floor ^Point pos]
  (aget floor (+ (* (.-y pos) 10) (.-x pos))))

(defn floor-set ^long [^longs floor ^Point pos ^long val]
  (aset floor (+ (* (.-y pos) 10) (.-x pos)) val))

(defn neighbours [^Point pos]
  (let [x (.-x pos)
        y (.-y pos)]
    (cond-> []
      (> x 0) (conj (Point. (dec x) y))
      (< x 9) (conj (Point. (inc x) y))
      (> y 0) (conj (Point. x (dec y)))
      (< y 9) (conj (Point. x (inc y)))
      (and (> x 0) (> y 0)) (conj (Point. (dec x) (dec y)))
      (and (> x 0) (< y 9)) (conj (Point. (dec x) (inc y)))
      (and (< x 9) (> y 0)) (conj (Point. (inc x) (dec y)))
      (and (< x 9) (< y 9)) (conj (Point. (inc x) (inc y))))))

(def points
  (vec
    (for [y (range 10)
          x (range 10)]
      (Point. x y))))

(defn floor->str [^longs floor]
  (->> floor
    (partition 10)
    (map (fn [line]
           (->> line
            (map #(format "%2d" %))
            (str/join " "))))
    (str/join "\n")))

(defn step! ^long [^longs floor]
  (dotimes [i 100]
    (aset floor i (inc (aget floor i))))
  (loop [points  (set (filter #(> (floor-get floor %) 9) points))
         flashes 0]
    (let [point (first points)]
      (if (nil? point)
        flashes
        (let [_          (floor-set floor point 0)
              neighbours (for [neighbour (neighbours point)
                               :let [val (floor-get floor neighbour)]
                               :when (>= val 1)
                               :let [_ (floor-set floor neighbour (inc val))]
                               :when (>= val 9)]
                           neighbour)]
          (recur (into (disj points point) neighbours) (inc flashes)))))))

(defn part1
  ([] (part1 input))
  ([input]
   (loop [floor   (parse input)
          flashes 0
          steps   100]
     (if (<= steps 0)
      flashes
      (let [df (step! floor)]
        (recur floor (+ flashes df) (dec steps)))))))

(defn part2
  ([] (part2 input))
  ([input]
   (loop [floor (parse input)
          step  0]
     (if (every? zero? floor)
      step
      (let [df (step! floor)]
        (recur floor (inc step)))))))

(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2021.day11 :reload))
  (part1 example)
  (part1)
  (part2 example)
  (part2)
  (require '[clj-async-profiler.core :as profiler]))