(ns advent-of-code.year2021.day10
  (:require
   [advent-of-code.core :refer [cond+]]
   [clojure.java.io :as io]
   [clojure.java.math :as math]
   [clojure.string :as str]
   [clojure.set :as set]
   #_[clj-async-profiler.core :as profiler]))

(set! *warn-on-reflection* true)

(set! *print-namespace-maps* false)

(def example (slurp "inputs/year2021/day10.example"))

(def input (slurp "inputs/year2021/day10"))

(defn score [line]
  (loop [i 0
         stack []]
    (cond+
      (>= i (count line)) 0
      :let [ch (nth line i)]
      (#{\( \[ \{ \<} ch) (recur (inc i) (conj stack ch))
      :let [pair (peek stack)]
      (and (= \) ch) (= \( pair)) (recur (inc i) (pop stack))
      (and (= \] ch) (= \[ pair)) (recur (inc i) (pop stack))
      (and (= \} ch) (= \{ pair)) (recur (inc i) (pop stack))
      (and (= \> ch) (= \< pair)) (recur (inc i) (pop stack))
      :else ({\) 3 \] 57 \} 1197 \> 25137} ch))))

(defn part1
  ([] (part1 input))
  ([input]
   (transduce (map score) + 0 (str/split input #"\n"))))

(defn autocomplete [line]
  (loop [i 0
         stack []]
    (cond+
      (>= i (count line))
      (reduce
        (fn [score ch]
          (+ (* score 5) ({\( 1 \[ 2 \{ 3 \< 4} ch)))
        0 (reverse stack))
      :let [ch (nth line i)]
      (#{\( \[ \{ \<} ch) (recur (inc i) (conj stack ch))
      :let [pair (peek stack)]
      (and (= \) ch) (= \( pair)) (recur (inc i) (pop stack))
      (and (= \] ch) (= \[ pair)) (recur (inc i) (pop stack))
      (and (= \} ch) (= \{ pair)) (recur (inc i) (pop stack))
      (and (= \> ch) (= \< pair)) (recur (inc i) (pop stack)))))

(defn median [xs]
  (nth xs (quot (count xs) 2)))

(defn part2
  ([] (part2 input))
  ([input]
   (->> (str/split input #"\n")
     (remove #(pos? (score %)))
     (map autocomplete)
     (sort)
     (median))))

(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2021.day10 :reload))
  (part1 example)
  (part1)
  (profiler/profile (part1))
  (part2 example)
  (profiler/profile (part2 example))
  (part2))