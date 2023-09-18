(ns advent-of-code.year2022.day25
  (:require
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [clojure.set :as set]
    [remote-require.core :as rr]))

(rr/from "https://twitter.com/nikitonsky/status/1584634959717097472"
  :require [cond+])

(rr/from "https://twitter.com/nikitonsky/status/1584629264909225984"
  :require [zip between? now])

(rr/from "https://twitter.com/nikitonsky/status/1602720736875057152"
  :require [index-of])

(def sample1
  "1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122")

(def data
  (slurp (io/file "inputs/year2022/day25")))

(defn parse-num [s]
  (->>
    (reduce
      (fn [[pow acc] d]
        [(* pow 5)
         (->
           (case d
             \0 0
             \1 1
             \2 2
             \- -1
             \= -2)
           (* pow)
           (+ acc))])
      [1 0]
      (reverse s))
    second))

(defn parse [data]
  (str/split-lines data))

(comment
  (parse sample1)
  (parse data))

(defn format-num [x]
  (loop [x x
         b 0
         l ()]
    (cond+
      (= 0 x) (str/join l)
      :let [x' (quot x 5)
            m  (+ b (mod x 5))]
      (= 0 m) (recur x' 0 (conj l \0))
      (= 1 m) (recur x' 0 (conj l \1))
      (= 2 m) (recur x' 0 (conj l \2))
      (= 3 m) (recur x' 1 (conj l \=))
      (= 4 m) (recur x' 1 (conj l \-))
      (= 5 m) (recur x' 1 (conj l \0)))))

(defn part1 [data]
  (let [data (parse data)
        sum  (transduce (map parse-num) + 0 data)]
    (assert (= sum (parse-num (format-num sum))))
    (format-num sum)))

(comment
  (part1 sample1)
  (format-num 4890)
  (part1 data))

(defn part2 [data]
  (let [data (parse data)]
    ))

(comment
  (part2 sample1)
  (part2 data))
