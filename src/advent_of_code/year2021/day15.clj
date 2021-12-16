(ns advent-of-code.year2021.day15
  (:require
   [advent-of-code.core :refer [cond+]]
   [clojure.java.io :as io]
   [clojure.java.math :as math]
   [clojure.string :as str]
   [clojure.set :as set]))

(set! *warn-on-reflection* true)

(set! *print-namespace-maps* false)

(def example (slurp "inputs/year2021/day15.example"))

(def input (slurp "inputs/year2021/day15"))

(defn parse [input]
  (let [lines (str/split-lines input)
        array (into-array Long/TYPE (mapv parse-long (re-seq #"\d" input)))]
    {:width  (count (first lines))
     :height (count lines)
     :values array}))

(defn neighbours [w h [x y]]
  (cond-> []
    (> x 0)       (conj [(dec x) y])
    (< x (dec w)) (conj [(inc x) y])
    (> y 0)       (conj [x (dec y)])
    (< y (dec h)) (conj [x (inc y)])))

(defn solve [{:keys [width height ^longs values]}]
  (let [costs ^longs (into-array Long/TYPE (repeat (* width height) Long/MAX_VALUE))
        idx   (fn [[x y]] (+ x (* y width)))]
    (aset costs (idx [0 0]) 0)
    (loop [queue (conj (clojure.lang.PersistentQueue/EMPTY) [0 0])]
      (if (empty? queue)
        (aget costs (idx [(dec width) (dec height)]))
        (let [pos     (peek queue)
              cost    (aget costs (idx pos))
              updated (for [pos'  (neighbours width height pos)
                            :let  [idx'  (idx pos')
                                   cost' (+ cost (aget values idx'))]
                            :when (< cost' (aget costs idx'))
                            :let  [_ (aset costs idx' cost')]]
                        pos')]
          (recur (into (pop queue) updated)))))))

(defn part1
  ([] (part1 input))
  ([input]
   (solve (parse input))))

(defn parse2 [input]
  (let [{:keys [width height ^longs values]} (parse input)]
    {:width  (* width 5)
     :height (* height 5)
     :values (into-array Long/TYPE
               (for [y (range (* 5 height))
                     x (range (* 5 width))
                     :let [delta (+ (quot y height) (quot x width))]]
                 (-> (aget values (+ (mod x width) (* (mod y height) width)))
                   (+ delta)
                   (- 1)
                   (mod 9)
                   (+ 1))))}))

(defn part2
  ([] (part2 input))
  ([input]
   (solve (parse2 input))))

(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2021.day15 :reload))
  (part1 example)
  (part1 input)
  (part2 example)
  (part2 input)
  (require '[clj-async-profiler.core :as profiler])
  (profiler/profile (part2 example)))