(ns advent-of-code.year2021.day20
  (:require
   [advent-of-code.core :as core :refer [cond+]]
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

(def example (slurp "inputs/year2021/day20.example"))

(def input (slurp "inputs/year2021/day20"))

(defn parse [input extra]
  (let [[code _ & lines] (str/split-lines input)
        w (+ (* 2 extra) (count (first lines)))
        h (+ (* 2 extra) (count lines))]
    {:code  (mapv {\. 0 \# 1} code)
     :image (concat
              (repeat extra (repeat w 0))
              (mapv #(concat (repeat extra 0) (mapv {\. 0 \# 1} %) (repeat extra 0)) lines)
              (repeat extra (repeat w 0)))
     :outer 0}))

(defn enchance [{:keys [code image outer]}]
  (let [w     (count (first image))
        h     (count image)
        maxx  (dec w)
        maxy  (dec h)
        outer (long outer)]
    {:code  code
     :image (core/forv [y (range h)]
              (core/forv [x (range w)]
                (let [nums [(if (and (> x 0)    (> y 0)   ) (nth (nth image (dec y)) (dec x)) outer)
                            (if                 (> y 0)     (nth (nth image (dec y))      x ) outer)
                            (if (and (< x maxx) (> y 0)   ) (nth (nth image (dec y)) (inc x)) outer)
                            (if (and (> x 0)              ) (nth (nth image      y ) (dec x)) outer)
                            (if true                        (nth (nth image      y )      x ) outer)
                            (if (and (< x maxx)           ) (nth (nth image      y ) (inc x)) outer)
                            (if (and (> x 0)    (< y maxy)) (nth (nth image (inc y)) (dec x)) outer)
                            (if                 (< y maxy)  (nth (nth image (inc y))      x ) outer)
                            (if (and (< x maxx) (< y maxy)) (nth (nth image (inc y)) (inc x)) outer)]
                      idx (reduce #(+ (* 2 %1) %2) 0 nums)]
                  ; (println x y nums idx (nth code idx))
                  (nth code idx))))
     :outer (nth code (case outer 0 0 1 511))}))

(defn lit [{:keys [image]}]
  (count
    (for [line image
          pos  line
          :when (= 1 pos)]
      1)))

(defn print-image [{:keys [image] :as m}]
  (doseq [line image]
    (doseq [pos line]
      (print (case (long pos) 0 "░" 1 "█")))
    (println))
  m)

(defn part1
  ([] (part1 input))
  ([input]
   (-> (parse input 2)
     (enchance)
     (enchance)
     ; (print-image)
     (lit))))


(defn part2
  ([] (part2 input))
  ([input]
   (-> (parse input 50)
     (->> (iterate enchance))
     (nth 50)
     ; (print-image)
     (lit))))

(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2021.day20 :reload))
  (parse input)
  (part1 example)
  (part1 input)
  (part2 example)
  (part2 input)
  (require '[clj-async-profiler.core :as profiler])
  (profiler/profile (part2 example)))
