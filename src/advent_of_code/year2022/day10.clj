(ns advent-of-code.year2022.day10
  (:require
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [clojure.set :as set]
    [remote-require.core :as rr]))

(rr/from "https://twitter.com/nikitonsky/status/1584634959717097472"
  :require [cond+])

(rr/from "https://twitter.com/nikitonsky/status/1584629264909225984"
  :require [zip])

(def sample1
  "noop
addx 3
addx -5")

(def sample2
  (slurp (io/file "inputs/year2022/day10.sample")))

(def data
  (slurp (io/file "inputs/year2022/day10")))

(defn parse [data]
  (->> data
    (str/split-lines)
    (mapv #(let [[_ c1 c2 arg] (re-matches #"(noop)|(addx) (-?\d+)" %)]
             [(keyword (or c1 c2)) (some-> arg parse-long)]))))

(defn xs [data]
  (reduce
    (fn [xs [cmd arg]]
      (case cmd
        :noop (conj xs (peek xs))
        :addx (-> xs
                (conj (peek xs))
                (conj (+ (peek xs) arg)))))
    [1 1]
    data))

(defn part1 [data]
  (->> (xs (parse data))
    (drop 20)
    (partition-all 40)
    (mapv (fn [i xs] (* (first xs) i)) (range 20 Long/MAX_VALUE 40))
    (reduce + 0)))

(comment
  (part1 sample1)
  (part1 sample2)
  (part1 data))

(defn part2 [data]
  (let [xs (next (xs (parse data)))]
    (->>
      (for [[reg p] (zip xs (cycle (range 0 40)))]
        (if (<= -1 (- p reg) 1)
          "█"
          " "))
      (partition 40)
      (map str/join)
      (str/join "\n"))))

(comment
  (part2 sample1)
  (println (part2 sample2))
  (println (part2 data)))
