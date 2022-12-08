(ns advent-of-code.year2022.day08
  (:require
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [clojure.set :as set]
    [remote-require.core :as rr]))

(rr/from "https://twitter.com/nikitonsky/status/1584634959717097472"
  :require [cond+])

(def sample1
  "30373
25512
65332
33549
35390")

(def data
  (slurp (io/file "inputs/year2022/day08")))

(defn parse [data]
  (->> data
    (str/split-lines)
    (mapv #(mapv (comp parse-long str) %))))

(defn visible? [forest x y]
  (let [w    (count (first forest))
        h    (count forest)
        tree (fn [x y] (get-in forest [y x]))
        hgth (tree x y)]
    (or
      (every? #(< (tree % y) hgth) (range 0 x))
      (every? #(< (tree % y) hgth) (range (inc x) w))
      (every? #(< (tree x %) hgth) (range 0 y))
      (every? #(< (tree x %) hgth) (range (inc y) h)))))

(defn part1 [data]
  (let [forest (parse data)
        w      (count (first forest))
        h      (count forest)]
    (count
      (filter true?
        (for [x (range 0 w)
              y (range 0 h)]
          (visible? forest x y))))))

(comment
  (part1 sample1)
  (part1 data))

(defn take-trees [pred xs]
  (loop [res []
         xs  xs]
    (cond+
      (empty? xs) res
      :let [x (first xs)]
      (not (pred x)) (conj res x)
      :else (recur (conj res x) (next xs)))))

(defn score [forest x y]
  (let [w    (count (first forest))
        h    (count forest)
        tree (fn [x y] (get-in forest [y x]))
        hgth (tree x y)]
    (*
      (count (take-trees #(< (tree % y) hgth) (range (dec x) -1 -1)))
      (count (take-trees #(< (tree % y) hgth) (range (inc x) w)))
      (count (take-trees #(< (tree x %) hgth) (range (dec y) -1 -1)))
      (count (take-trees #(< (tree x %) hgth) (range (inc y) h))))))

(defn part2 [data]
  (let [forest (parse data)
        w      (count (first forest))
        h      (count forest)]
    (reduce max 0
      (for [x (range 0 w)
            y (range 0 h)]
        (score forest x y)))))

(comment
  (part2 sample1)
  (part2 data))
