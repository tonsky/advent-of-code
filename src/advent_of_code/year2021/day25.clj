(ns advent-of-code.year2021.day25
  (:require
   [advent-of-code.core :refer [cond+ forv]]
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

(def example (slurp "inputs/year2021/day25.example"))

(def input (slurp "inputs/year2021/day25"))

(defn parse [s]
  (->> (str/split-lines s)
    (mapv vec)))

(defn move-right [lines]
  (let [width  (count (first lines))
        height (count lines)]
    (forv [y (range height)
           :let [line (nth lines y)]]
      (forv [x (range (count (first lines)))
             :let [left  (nth line (-> x (+ (dec width)) (mod width)))
                   this  (nth line x)
                   right (nth line (-> x (+ 1) (mod width)))]]
        (cond
          (= [left this]  [\> \.]) \>
          (= [this right] [\> \.]) \.
          :else this)))))

(defn move-down [lines]
  (let [width  (count (first lines))
        height (count lines)]
    (forv [y (range height)
           :let [top    (nth lines (-> y (+ (dec height)) (mod height)))
                 line   (nth lines y)
                 bottom (nth lines (-> y (+ 1) (mod height)))]]
      (forv [x (range (count (first lines)))]
        (cond
          (= [(nth top x) (nth line x)] [\v \.]) \v
          (= [(nth line x) (nth bottom x)] [\v \.]) \.
          :else (nth line x))))))

(defn part1
  ([] (part1 input))
  ([input]
   (loop [lines (parse input)
          turn  1]
     (let [lines' (-> lines move-right move-down)]
       (if (= lines lines')
         turn
         (recur lines' (inc turn)))))))

(defn part2
  ([] (part2 input))
  ([input]
   "Happy Christmas!"))

(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2021.day25 :reload))
  (part1 example)
  (part1 input)
  (part2 example)
  (part2 input)
  (require '[clj-async-profiler.core :as profiler])
  (profiler/profile (part2 example)))
