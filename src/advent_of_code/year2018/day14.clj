(ns advent-of-code.year2018.day14
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as set]))

(defn part1
  ([] (part1 190221))
  ([input]
   (loop [data [3 7]
          elf1 0
          elf2 1]
     (if (>= (count data) (+ input 10))
       (str/join "" (subvec data input (+ input 10)))
       (let [sum   (+ (data elf1) (data elf2))
             data' (if (>= sum 10)
                     (-> data (conj (quot sum 10)) (conj (mod sum 10)))
                     (-> data (conj sum)))
             elf1' (mod (+ elf1 1 (data elf1)) (count data'))
             elf2' (mod (+ elf2 1 (data elf2)) (count data'))]
         (recur data' elf1' elf2'))))))

(defn part2
  ([] (part2 "190221"))
  ([input]
   (let [digits (mapv #(parse-long (str %)) input)
         len    (count digits)]
     (loop [data [3 7]
            elf1 0
            elf2 1
            last 0]
       (if (>= (- (count data) last) len)
         (if (= digits (subvec data last (+ last len)))
           last
           (recur data elf1 elf2 (inc last)))
         (let [sum   (+ (data elf1) (data elf2))
               data' (if (>= sum 10)
                       (-> data (conj (quot sum 10)) (conj (mod sum 10)))
                       (-> data (conj sum)))
               elf1' (mod (+ elf1 1 (data elf1)) (count data'))
               elf2' (mod (+ elf2 1 (data elf2)) (count data'))]
           (recur data' elf1' elf2' last)))))))

(comment
  (part1 9)
  (part1 5)
  (part1 18)
  (part1 2018)
  (part1)

  (part2 "51589")
  (part2 "01245")
  (part2 "92510")
  (part2 "59414")
  (part2))