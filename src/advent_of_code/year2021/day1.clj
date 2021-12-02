(ns advent-of-code.year2021.day1
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as set]))

(def data
  (with-open [rdr (io/reader (io/file "inputs/year2021/day1"))]
    (->> (line-seq rdr)
      (mapv parse-long))))

(defn part1 []
  (->> (map - data (next data))
    (filter neg?)
    (count)))

(defn part2 []
  (let [windows (map + data (next data) (nnext data))]
    (->> (map - windows (next windows))
      (filter neg?)
      (count))))

(defn -main [& args]
  (println "Day 1")
  (println "├ part 1:" (part1))
  (println "└ part 2:" (part2)))

(comment
  (part1)
  (part2))