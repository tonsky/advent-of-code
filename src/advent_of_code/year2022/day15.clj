(ns advent-of-code.year2022.day15
  (:require
    [advent-of-code.v2 :as v2 :refer [v2]]
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [clojure.set :as set]
    [remote-require.core :as rr]))

(rr/from "https://twitter.com/nikitonsky/status/1584634959717097472"
  :require [cond+])

(rr/from "https://twitter.com/nikitonsky/status/1584629264909225984"
  :require [zip between?])

(def sample1
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3")

(def data
  (slurp (io/file "inputs/year2022/day15")))

(defn parse [data]
  (->> data
    (re-seq #"-?\d+")
    (map parse-long)
    (partition 2)
    (map #(apply v2 %))
    (partition 2)))

(comment
  (parse sample1)
  (parse data))

(defn intersects? [[a b] [c d]]
  (or (<= a c b)
    (<= c a d)))

(defn intersect [[a b] [c d]]
  (assert (intersects? [a b] [c d]))
  [(min a c) (max b d)])

(defn intervals [data row]
  (let [
        intervals (for [[sensor beacon] data
                        :let [dist  (v2/dist sensor beacon)
                              dy    (abs (- (:y sensor) row))
                              range (- dist dy)]
                        :when (>= range 0)]
                    [(- (:x sensor) range) (+ (:x sensor) range 1)])]
    (loop [intervals intervals
           safe      []]
      (cond+
        (empty? intervals)
        (sort-by first safe)
                       
        :let [[interval & rest] intervals
              join (some #(when (intersects? interval %) %) rest)]
                       
        join
        (recur
          (cons (intersect interval join)
            (remove #(= join %) rest))
          safe)
                       
        :else
        (recur rest (conj safe interval))))))

(defn part1 [data row]
  (let [data (parse data)
        intervals (intervals data row)
        beacons (->> (map second data)
                  (filter #(= row (:y %)))
                  (map :x)
                  (set))]
    (reduce
      (fn [acc [a b]]
        (+ acc (- b a) (- (count (filter #(between? % a b) beacons)))))
      0
      intervals)))

(comment
  (part1 sample1 10)
  (part1 data 2000000))

(defn part2 [data max]
  (let [data (parse data)]
    (first
      (for [row (range 0 max)
            :let [intervals (intervals data row)]
            :when (> (count intervals) 1)]
        (+ row
          (* 4000000 (last (first intervals))))))))

(comment
  (part2 sample1 20)
  (part2 data 4000000))
