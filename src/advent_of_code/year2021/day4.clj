(ns advent-of-code.year2021.day4
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as set]))

(def example
  (slurp "inputs/year2021/day4.example"))

(def input
  (slurp "inputs/year2021/day4"))

(defn parse-board [board]
  (-> board
    (str/trim)
    (str/split #"\s+")
    (->> (mapv parse-long)
      (partition 5)
      (mapv vec)
      (vec))))

(defn transpose [board]
  (for [col (range (count (first board)))]
    (mapv #(nth % col) board)))

(defn parse [input]
  (let [[numbers boards] (str/split input #"\n" 2)]
    {:numbers (-> numbers (str/split #",") (->> (mapv parse-long))) 
     :boards  (-> boards (str/split #"\n\n") (->> (mapv parse-board)))}))

(defn score-rows [board nums]
  (let [num-set (set nums)]
    (when (some #(every? num-set %) board)
      (->> board
        (apply concat)
        (remove num-set)
        (reduce +)
        (* (last nums))))))

(defn score [board nums]
  (or (score-rows board nums)
    (score-rows (transpose board) nums)))

(defn part1
  ([] (part1 input))
  ([input]
    (let [{:keys [numbers boards]} (parse input)]
      (loop [n 1]
        (let [nums (take n numbers)]
          (or
            (some #(score % nums) boards)
            (recur (inc n))))))))

(defn part2
  ([] (part2 input))
  ([input]
    (let [{:keys [numbers boards]} (parse input)]
      (loop [n 1
             boards boards]
        (let [nums    (take n numbers)
              boards' (remove #(score % nums) boards)]
          (if (empty? boards')
            (score (last boards) nums)
            (recur (inc n) boards')))))))

(defn -main [& args]
  (println "Day 4")
  (println "├ part 1:" (part1))
  (println "└ part 2:" (part2)))

(comment
  (part1 example)
  (part1)
  (part2 example)
  (part2))