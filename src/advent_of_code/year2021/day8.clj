(ns advent-of-code.year2021.day8
  (:require
   [clojure.java.io :as io]
   [clojure.java.math :as math]
   [clojure.string :as str]
   [clojure.set :as set]
   #_[clj-async-profiler.core :as profiler]))

(def example (slurp "inputs/year2021/day8.example"))

(def input (slurp "inputs/year2021/day8"))

(defn part1
  ([] (part1 input))
  ([input]
   (count
     (for [line (str/split input #"\n")
           :let [[_ output] (str/split line #"\|")]
           digit (str/split (str/trim output) #"\s+")
           :when (#{2 3 4 7} (count digit))]
       digit))))

;                                                                                        
;  0000    ████    ░░░░    ████    ████    ░░░░    ████    ████    ████    ████    ████  
; 1    2  █    █  ░    █  ░    █  ░    █  █    █  █    ░  █    ░  ░    █  █    █  █    █ 
; 1    2  █    █  ░    █  ░    █  ░    █  █    █  █    ░  █    ░  ░    █  █    █  █    █ 
;  3333    ░░░░    ░░░░    ████    ████    ████    ████    ████    ░░░░    ████    ████  
; 4    5  █    █  ░    █  █    ░  ░    █  ░    █  ░    █  █    █  ░    █  █    █  ░    █ 
; 4    5  █    █  ░    █  █    ░  ░    █  ░    █  ░    █  █    █  ░    █  █    █  ░    █ 
;  6666    ████    ░░░░    ████    ████    ░░░░    ████    ████    ░░░░    ████    ████  
;                                                                                        

(defn array-list [& xs]
  (let [res (java.util.ArrayList. (count xs))]
    (doseq [x xs]
      (.add res x))
    res))

(defn deduce
  "Takes
   
   #{\"be\" \"abcdefg\" \"bcdefg\" \"acdefg\" \"bceg\" \"cdefg\" \"abdefg\" \"bcdef\" \"abcdf\" \"bde\"}
   
   Returns

   {\"be\" 1 \"bde\" 7 ...}"
  [digits]
  (let [numbers {(array-list 0 1 2 4 5 6)   0
                 (array-list 2 5)           1
                 (array-list 0 2 3 4 6)     2
                 (array-list 0 2 3 5 6)     3
                 (array-list 1 2 3 5)       4
                 (array-list 0 1 3 5 6)     5
                 (array-list 0 1 3 4 5 6)   6 
                 (array-list 0 2 5)         7
                 (array-list 0 1 2 3 4 5 6) 8
                 (array-list 0 1 2 3 5 6)   9}
        decode  (fn [^String digit code]
                  (let [list (java.util.ArrayList. (count digit))]
                    (dotimes [i (count digit)]
                      (let [ch (.charAt digit i)]
                        (.add list (nth code (- (int ch) (int \a))))))
                    (.sort list nil)
                    list))
        code    (first
                  (for [a (range 0 7)
                        b (range 0 7)
                        :when (not (== a b))
                        c (range 0 7)
                        :when (and (not (== a c)) (not (== b c)))
                        d (range 0 7)
                        :when (and (not (== a d)) (not (== b d)) (not (== c d)))
                        e (range 0 7)
                        :when (and (not (== a e)) (not (== b e)) (not (== c e)) (not (== d e)))
                        f (range 0 7)
                        :when (and (not (== a f)) (not (== b f)) (not (== c f)) (not (== d f)) (not (== e f)))
                        g (range 0 7)
                        :when (and (not (== a g)) (not (== b g)) (not (== c g)) (not (== d g)) (not (== e g)) (not (== f g)))
                        :let [code   [a b c d e f g]
                              result (mapv #(decode % code) digits)]
                        :when (every? numbers result)]
                    code))]
    (into {}
      (for [digit digits]
        [digit (numbers (decode digit code))]))))

(defn join-digits [ds]
  (reduce (fn [acc d] (+ (* acc 10) d)) 0 ds))

(defn part2
  ([] (part2 input))
  ([input]
   (reduce + 0
     (for [line (str/split input #"\n")
           :let [[digits output] (str/split line #"\s+\|\s+")
                 digits (->> (str/split digits #"\s+")
                          (mapv #(str/join (sort %))))
                 code   (deduce digits)
                 output (->> (str/split output #"\s+")
                          (mapv #(str/join (sort %)))
                          (mapv code)
                          join-digits)]]
      output))))

(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2021.day8 :reload))
  (part1 example)
  (part1)
  (profiler/profile (part1))
  (time (part2 example))
  (profiler/profile (part2 example))
  (part2))