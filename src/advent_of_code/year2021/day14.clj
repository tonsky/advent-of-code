(ns advent-of-code.year2021.day14
  (:require
   [advent-of-code.core :refer [cond+]]
   [clojure.java.io :as io]
   [clojure.java.math :as math]
   [clojure.string :as str]
   [clojure.set :as set]))

(set! *warn-on-reflection* true)

(set! *print-namespace-maps* false)

(def example (slurp "inputs/year2021/day14.example")) ;; NNCB

(def input (slurp "inputs/year2021/day14")) ;; VOKKVSKKPSBVOOKVCFOV

(defn parse [input]
  (->> input
    (re-seq #"([A-Z])([A-Z]) -> ([A-Z])")
    (map (fn [[_ a b c]] [(str a b) c]))
    (into {})))

(defn add [a b]
  (+ (or a 0) (or b 0)))

(defn step [rules pairs]
  (reduce-kv
    (fn [acc pair amount]
      (if-some [ins (rules pair)]
        (-> acc
          (update (str (first pair) ins) add amount)
          (update (str ins (second pair)) add amount))
        (assoc acc pair amount)))
    {}
    pairs))

(defn part1
  ([] (part1 input "VOKKVSKKPSBVOOKVCFOV" 10))
  ([input start steps]
   (let [rules  (parse input)
         pairs  (reduce
                  (fn [acc pair]
                    (update acc pair add 1))
                  {}
                  (map str (concat "^" start "$") (concat start "$")))
         pairs' (nth (iterate #(step rules %) pairs) steps)
         freqs  (reduce-kv
                  (fn [acc pair amount]
                    (-> acc
                      (update (subs pair 0 1) add amount)
                      (update (subs pair 1 2) add amount)))
                  {}
                  pairs')
         freqs' (->> (dissoc freqs "^" "$")
                  (vals)
                  (map #(/ % 2))
                  (sort))]
     (- (last freqs') (first freqs')))))

(defn part2
  ([] (part1 input "VOKKVSKKPSBVOOKVCFOV" 40)))

(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2021.day14 :reload))
  (part1 example "NNCB" 10)
  (part1 input "VOKKVSKKPSBVOOKVCFOV" 10)
  (part1 example "NNCB" 40)
  (part1 input "VOKKVSKKPSBVOOKVCFOV" 40)
  (println (part2))
  (require '[clj-async-profiler.core :as profiler])
  (profiler/profile (part2 example)))