(ns advent-of-code.year2021.day13
  (:require
   [advent-of-code.core :refer [cond+]]
   [clojure.java.io :as io]
   [clojure.java.math :as math]
   [clojure.string :as str]
   [clojure.set :as set]))

(set! *warn-on-reflection* true)

(set! *print-namespace-maps* false)

(def example (slurp "inputs/year2021/day13.example"))

(def input (slurp "inputs/year2021/day13"))

(defn parse [input]
  (let [dots (into #{} 
               (map (fn [[_ x y]] [(parse-long x) (parse-long y)]))
               (re-seq #"(\d+),(\d+)" input))
        folds (into []
                (map (fn [[_ axis coord]] [(keyword axis) (parse-long coord)]))
                (re-seq #"fold along (x|y)=(\d+)" input))]
    {:dots dots
     :folds folds}))

(defn fold [dots [axis coord]]
  (reduce
    (fn [acc [x y :as dot]]
      (case axis
        :y (conj acc (if (<= y coord) dot [x (- (* 2 coord) y)]))
        :x (conj acc (if (<= x coord) dot [(- (* 2 coord) x) y]))))
    #{}
    dots))

(defn part1
  ([] (part1 input))
  ([input]
   (let [{:keys [dots folds]} (parse input)]
     (count (fold dots (first folds))))))

(defn dots->str [dots]
  (let [max-x (inc (transduce (map first) max 0 dots))
        max-y (inc (transduce (map second) max 0 dots))
        sb    (StringBuilder. ^long (* (inc max-x) max-y))]
    (dotimes [y max-y]
      (.append sb "\n")
      (dotimes [x max-x]
        (.append sb (if (dots [x y]) "█" "░"))))
    (str sb)))

(defn part2
  ([] (part2 input))
  ([input]
   (let [{:keys [dots folds]} (parse input)
         dots' (reduce fold dots folds)]
     (dots->str dots'))))

(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2021.day13 :reload))
  (part1 example)
  (part1)
  (part2 example)
  (println (part2))
  (require '[clj-async-profiler.core :as profiler])
  (profiler/profile (part2 example)))