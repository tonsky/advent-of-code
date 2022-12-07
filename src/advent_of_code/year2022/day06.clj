(ns advent-of-code.year2022.day06
  (:require
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [clojure.set :as set]
    [remote-require.core :as rr]))

(def sample1
  "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(def data
  (slurp (io/file "inputs/year2022/day06")))

(defn solve [n data]
  (some
    #(let [s (subs data % (+ % n))]
       (when (apply distinct? (seq s))
         (+ % n)))
    (range 0 (- (count data) n))))

(defn part1 [data]
  (solve 4 data))

(comment
  (part1 sample1)
  (part1 data))

(defn part2 [data]
  (solve 14 data))

(comment
  (part2 sample1)
  (part2 data))
