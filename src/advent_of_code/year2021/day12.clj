(ns advent-of-code.year2021.day12
  (:require
   [advent-of-code.core :refer [cond+]]
   [clojure.java.io :as io]
   [clojure.java.math :as math]
   [clojure.string :as str]
   [clojure.set :as set]))

(set! *warn-on-reflection* true)

(set! *print-namespace-maps* false)

(def example1 (slurp "inputs/year2021/day12.example1"))

(def example2 (slurp "inputs/year2021/day12.example2"))

(def example3 (slurp "inputs/year2021/day12.example3"))

(def input (slurp "inputs/year2021/day12"))

(defn parse [input]
  (reduce
    (fn [acc [start end]]
      (-> acc
        (update start (fnil conj #{}) end)
        (update end (fnil conj #{}) start)))
    {}
    (->> input
      (str/split-lines)
      (map #(next (re-matches #"([a-zA-Z]+)-([a-zA-Z]+)" %))))))

(defn small? [^String s]
  (Character/isLowerCase (.charAt s 0)))

(defn part1
  ([] (part1 input))
  ([input]
   (let [chart (parse input)]
     (loop [unfinished [["start"]]
            terminal   #{}]
       (if-some [path (peek unfinished)]
         (let [end (peek path)]
           (if (= "end" end)
             (recur (pop unfinished) (conj terminal path))
             (let [visited? (set (filter small? path))
                   nexts    (->> (chart end) (remove visited?))]
               (recur (into (pop unfinished) (map #(conj path %) nexts)) terminal))))
         (count terminal))))))

(defn nexts2 [chart path]
  (let [end     (peek path)
        nexts   (->> (chart end) (remove #(= "start" %)))
        visited (frequencies (filter small? path))]
    (if (every? #(= 1 %) (vals visited))
      nexts
      (remove visited nexts))))

(defn part2
  ([] (part2 input))
  ([input]
   (let [chart (parse input)]
     (loop [unfinished [["start"]]
            terminal   #{}]
       (if-some [path (peek unfinished)]
         (let [end (peek path)]
           (if (= "end" end)
             (recur (pop unfinished) (conj terminal path))
             (let [nexts (nexts2 chart path)]
               (recur (into (pop unfinished) (map #(conj path %) nexts)) terminal))))
         (count terminal))))))

(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2021.day12 :reload))
  (part1 example1)
  (part1 example2)
  (part1 example3)
  (part1)
  (part2 example1)
  (part2 example2)
  (part2 example3)
  (part2)
  (require '[clj-async-profiler.core :as profiler]))