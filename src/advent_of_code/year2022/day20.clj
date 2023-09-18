(ns advent-of-code.year2022.day20
  (:require
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [clojure.set :as set]
    [remote-require.core :as rr])
  (:import
    [java.util LinkedList]))

(rr/from "https://twitter.com/nikitonsky/status/1584634959717097472"
  :require [cond+])

(rr/from "https://twitter.com/nikitonsky/status/1584629264909225984"
  :require [zip between? now])

(rr/from "https://twitter.com/nikitonsky/status/1602720736875057152"
  :require [index-of])

(def sample1
  "1
2
-3
3
-2
0
4")

(def data
  (slurp (io/file "inputs/year2022/day20")))

(defn parse [data]
  (->> data
    (str/split-lines)
    (mapv parse-long)))

(comment
  (parse sample1)
  (parse data))

(defn part1 [data]
  (let [data (parse data)
        list (doto (LinkedList.)
               (.addAll data))]
    (doseq [x data
            :let [i  (.indexOf list x)
                  x  (* (int (math/signum x)) (mod (abs x) (dec (count data))))
                  m  (dec (count data))
                  i' (mod (+ i x m m) m)
                  i' (cond
                       (= 0 i') m
                       ; (and (< x 0) (= 0 i')) m
                       ; (and (> x 0) (= (dec m) i')) 0
                       :else i')
                  ]]
      (.remove list i)
      (.add list i' x))
    (let [i (.indexOf list 0)
          m (count list)
          a (.get list (mod (+ i 1000) m))
          b (.get list (mod (+ i 2000) m))
          c (.get list (mod (+ i 3000) m))]
      (+ a b c))))

(comment
  (part1 sample1)
  (part1 data))

(defn parse2 [data]
  (parse data))

(defn part2 [data]
  (let [data (parse2 data)]
    ))

(comment
  (part2 sample1)
  (part2 data))
