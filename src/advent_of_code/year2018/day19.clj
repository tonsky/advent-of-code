(ns advent-of-code.year2018.day19
  (:require
   [advent-of-code.core :refer [cond+]]
   [clojure.java.io :as io]
   [clojure.java.math :as math]
   [clojure.math.combinatorics :as comb]
   [clojure.string :as str]
   [clojure.set :as set]))

(set! *warn-on-reflection* true)

(def example (slurp "inputs/year2018/day19.example")) ; ip = 0

(def problem (slurp "inputs/year2018/day19")) ; ip = 1

(defn addr [regs [_ a b c]]
  (assoc regs c (+ (nth regs a) (nth regs b))))

(defn addi [regs [_ a b c]]
  (assoc regs c (+ (nth regs a) b)))

(defn mulr [regs [_ a b c]]
  (assoc regs c (* (nth regs a) (nth regs b))))

(defn muli [regs [_ a b c]]
  (assoc regs c (* (nth regs a) b)))

(defn banr [regs [_ a b c]]
  (assoc regs c (bit-and (nth regs a) (nth regs b))))

(defn bani [regs [_ a b c]]
  (assoc regs c (bit-and (nth regs a) b)))

(defn borr [regs [_ a b c]]
  (assoc regs c (bit-or (nth regs a) (nth regs b))))

(defn bori [regs [_ a b c]]
  (assoc regs c (bit-or (nth regs a) b)))

(defn setr [regs [_ a b c]]
  (assoc regs c (nth regs a)))

(defn seti [regs [_ a b c]]
  (assoc regs c a))

(defn gtir [regs [_ a b c]]
  (assoc regs c (if (> a (nth regs b)) 1 0)))

(defn gtri [regs [_ a b c]]
  (assoc regs c (if (> (nth regs a) b) 1 0)))

(defn gtrr [regs [_ a b c]]
  (assoc regs c (if (> (nth regs a) (nth regs b)) 1 0)))

(defn eqir [regs [_ a b c]]
  (assoc regs c (if (= a (nth regs b)) 1 0)))

(defn eqri [regs [_ a b c]]
  (assoc regs c (if (= (nth regs a) b) 1 0)))

(defn eqrr [regs [_ a b c]]
  (assoc regs c (if (= (nth regs a) (nth regs b)) 1 0)))

(defn parse [input]
  (vec
    (for [line (str/split-lines input)
          :let [[_ op a b c] (re-matches #"([a-z]{4}) (\d+) (\d+) (\d+)" line)]]
      [(resolve (symbol "advent-of-code.year2018.day19" op))
       (parse-long a)
       (parse-long b)
       (parse-long c)])))

(defn solve [program ip regs max-steps]
  (loop [regs regs
         step 0]
    (if (> step max-steps)
      (nth regs 0)
      (let [op-idx (nth regs ip)]
        (if (>= op-idx (count program))
          (nth regs 0)
          (let [line   (nth program op-idx)
                regs'  ((first line) regs line)
                regs'' (update regs' ip inc)]
            (recur regs'' (inc step))))))))

(defn part1
  ([] (part1 problem 1))
  ([input ip]
   (solve (parse input) ip [0 0 0 0 0 0] Long/MAX_VALUE)))

(defn part2
  "r2 in [0..10551408]
   r4 in [0..10551408]
   when r2 * r4 = 10551408
   r0 += r4"
  []
  (let [number  10551408
        factors [2 2 2 2 3 7 31 1013]]
    (->>
      (for [subset (comb/subsets factors)
            :let [r2 (reduce * subset)
                  r4 (/ number r2)]]
        [r2 r4])
      (into #{})
      (reduce (fn [acc [a b]] (+ acc b)) 0))))

(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2018.day19 :reload))
  (part1 example 0)
  (part1)
  (part2)

  (let [res (volatile! 0)]
    (dotimes [r2 10551408]
      (dotimes [r4 10551408]
        (when (= 10551408 (* r2 r4))
          (vswap! res + r4))))
    @res)

  (solve (parse problem) 1 [1 0 0 0 0 0] 100)
  (solve (parse problem) 1 [0 3 10551407 10551408 1 0] 100)
  (solve (parse problem) 1 [0 3 10551408 10551408 10551408 0] 100)
  (solve (parse problem) 1 [100 3 10416 10551408 1013 0] 100)
)

