(ns advent-of-code.year2018.day21
  (:require
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [clojure.set :as set]
    [advent-of-code.year2018.day19 :as day19]
    [remote-require.core :as rr]))

(rr/from "https://twitter.com/nikitonsky/status/1584634959717097472"
  :require [cond+])

(defn parse [data]
  (day19/parse data))

(def data
  (parse (slurp (io/file "inputs/year2018/day21"))))

(comment
  (run data 1 [0 0 0 0 0 0] 2000)
  (run data 1 [0 0 0 0 0 0] Long/MAX_VALUE)
  
  (format "%32s" "1234")
  )

(defn part1 [program]
  (let [program (take 29 program)
        ip      1]
    (loop [regs [0 0 0 0 0 0]
           step 0]
      (cond+
        :let [op-idx (nth regs ip)]
        
        (>= op-idx (count program))
        (nth regs 5)
        
        :let [line   (nth program op-idx)
              regs'  ((first line) regs line)
              regs'' (update regs' ip inc)]
        
        :else
        (recur regs'' (inc step))))))

(comment
  (part1 data))

(defn part2 [program]
  (let [ip 1]
    (loop [regs [0 0 0 0 0 0]
           step 0
           r5s  #{}]
      (cond+
        :let [op-idx (nth regs ip)]
        
        (>= op-idx (count program))
        [step regs]
        
        :let [line   (nth program op-idx)
              regs'  ((first line) regs line)
              regs'' (update regs' ip inc)
              r5     (nth regs 5)
              r5s''  (if (= 28 op-idx) (conj r5s r5) r5s)]
        
        (and (= 28 op-idx) (contains? r5s r5))
        r5
        
        :else
        (recur regs'' (inc step) r5s'')))))

(comment
  (part2 data))
