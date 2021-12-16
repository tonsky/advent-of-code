(ns advent-of-code.year2021.day16
  (:refer-clojure :exclude [eval])
  (:require
   [advent-of-code.core :refer [cond+]]
   [clojure.java.io :as io]
   [clojure.java.math :as math]
   [clojure.string :as str]
   [clojure.set :as set]))

(set! *warn-on-reflection* true)

(set! *print-namespace-maps* false)

(def example1 "D2FE28") ;; 2021

(def example2 "38006F45291200")

(def example3 "EE00D40C823060")

(def input (slurp "inputs/year2021/day16"))

(def hex->bits
  {\0 [0 0 0 0]
   \1 [0 0 0 1]
   \2 [0 0 1 0]
   \3 [0 0 1 1]
   \4 [0 1 0 0]
   \5 [0 1 0 1]
   \6 [0 1 1 0]
   \7 [0 1 1 1]
   \8 [1 0 0 0]
   \9 [1 0 0 1]
   \A [1 0 1 0]
   \B [1 0 1 1]
   \C [1 1 0 0]
   \D [1 1 0 1]
   \E [1 1 1 0]
   \F [1 1 1 1]})

(defn bits->number [bits]
  (reduce #(bit-or (bit-shift-left %1 1) %2) 0 bits))

(defn parse-literal [bits]
  (loop [acc  0
         bits bits
         len  0]
    (let [[a b c d e & rest] bits]
      (condp = a
        0
        (let [acc' (-> acc
                     (bit-shift-left 4)
                     (bit-or (bits->number [b c d e])))]
          {:value acc'
           :len   (+ len 5)})
        1
        (let [acc' (-> acc
                     (bit-shift-left 4)
                     (bit-or (bits->number [b c d e])))]
          (recur acc' rest (+ len 5)))))))

(defn parse-bits [bits]
  (let [[a b c d e f g & rest] bits
        version (bits->number [a b c])
        type    (bits->number [d e f])]
    (cond
      (= type 4)
      (merge-with +
        {:version version
         :type    type
         :len     6}
        (parse-literal (cons g rest)))
      
      (= 0 g) ; len-bounded
      (let [[len-bits rest] (split-at 15 rest)
            len (bits->number len-bits)]
        (loop [left len
               rest rest
               acc  []]
          (if (<= left 0)
            {:version version
             :type    type
             :len     (+ 7 15 len)
             :value   acc}
            (let [parsed (parse-bits rest)
                  len    (:len parsed)]
              (recur (- left len) (drop len rest) (conj acc parsed))))))

      (= 1 g) ; count-bounded
      (let [[count-bits rest] (split-at 11 rest)
            count             (bits->number count-bits)]
        (loop [left  count
               len   (+ 7 11)
               rest  rest
               acc   []]
          (if (<= left 0)
            {:version version
             :type    type
             :len     len
             :value   acc}
            (let [parsed (parse-bits rest)]
              (recur (dec left) (+ len (long (:len parsed))) (drop (:len parsed) rest) (conj acc parsed)))))))))

(defn parse [input]
  (parse-bits (mapcat hex->bits input)))

(defn sum-versions [{:keys [value version]}]
  (if (sequential? value)
    (+ version (reduce + 0 (map sum-versions value)))
    version))

(defn part1
  ([] (part1 input))
  ([input]
   (sum-versions (parse input))))

(defn eval [{:keys [type value]}]
  (condp = type
    0 (transduce (map eval) + 0 value)
    1 (transduce (map eval) * 1 value)
    2 (transduce (map eval) min Long/MAX_VALUE value)
    3 (transduce (map eval) max Long/MIN_VALUE value)
    4 value
    5 (let [[a b] value]
        (if (> (eval a) (eval b)) 1 0))
    6 (let [[a b] value]
        (if (< (eval a) (eval b)) 1 0))
    7 (let [[a b] value]
        (if (= (eval a) (eval b)) 1 0))))

(defn part2
  ([] (part2 input))
  ([input]
   (eval (parse input))))

(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2021.day16 :reload))
  (= 16 (part1 "8A004A801A8002F478"))
  (= 12 (part1 "620080001611562C8802118E34"))
  (= 23 (part1 "C0015000016115A2E0802F182340"))
  (= 31 (part1 "A0016C880162017C3686B18A3D4780"))
  (part1 input)

  (= 3  (part2 "C200B40A82"))
  (= 54 (part2 "04005AC33890"))
  (= 7  (part2 "880086C3E88112"))
  (= 9  (part2 "CE00C43D881120"))
  (= 1  (part2 "D8005AC2A8F0"))
  (= 0  (part2 "F600BC2D8F"))
  (= 0  (part2 "9C005AC2F8F0"))
  (= 1  (part2 "9C0141080250320F1802104A08"))
  (part2 input)
  (require '[clj-async-profiler.core :as profiler])
  (profiler/profile (part2 example)))