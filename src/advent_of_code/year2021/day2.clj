(ns advent-of-code.year2021.day2
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as set]))

(def data
  (with-open [rdr (io/reader (io/file "inputs/year2021/day2"))]
    (->> (line-seq rdr)
      (mapv #(let [[_ cmd arg] (re-matches #"([a-z]+) ([0-9]+)" %)]
              [(keyword cmd) (parse-long arg)])))))

(defn part1 []
  (let [[x y] (reduce 
                (fn [[x y] [cmd arg]]
                  (case cmd
                    :forward [(+ x arg) y]
                    :down    [x (+ y arg)]
                    :up      [x (- y arg)]))
                [0 0] data)]
    (* x y)))

(defn part2 []
  (let [[x y _] (reduce 
                  (fn [[x y aim] [cmd arg]]
                    (case cmd
                      :forward [(+ x arg) (+ y (* aim arg)) aim]
                      :down    [x y (+ aim arg)]
                      :up      [x y (- aim arg)]))
                  [0 0 0] data)]
    (* x y)))

(defn -main [& args]
  (println "Day 1")
  (println "├ part 1:" (part1))
  (println "└ part 2:" (part2)))

(comment
  (part1)
  (part2))