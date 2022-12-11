(ns advent-of-code.year2022.day11
  (:require
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [clojure.set :as set]
    [clj-async-profiler.core :as profiler]
    [remote-require.core :as rr]))

(def sample1
  [{:items    [79 98]
    :op       #(* 19 %)
    :test     #(= 0 (mod % 23))
    :pass     2
    :fail     3
    :inspects 0}
   {:items    [54, 65, 75, 74]
    :op       #(+ 6 %)
    :test     #(= 0 (mod % 19))
    :pass     2
    :fail     0
    :inspects 0}
   {:items    [79, 60, 97]
    :op       #(* % %)
    :test     #(= 0 (mod % 13))
    :pass     1
    :fail     3
    :inspects 0}
   {:items    [74]
    :op       #(+ 3 %)
    :test     #(= 0 (mod % 17))
    :pass     0
    :fail     1
    :inspects 0}])

(def data
  [{:items    [89, 73, 66, 57, 64, 80]
    :op       #(* 3 %)
    :test     #(= 0 (mod % 13))
    :pass     6
    :fail     2
    :inspects 0}
   {:items    [83, 78, 81, 55, 81, 59, 69]
    :op       #(+ 1 %)
    :test     #(= 0 (mod % 3))
    :pass     7
    :fail     4
    :inspects 0}
   {:items    [76, 91, 58, 85]
    :op       #(* 13 %)
    :test     #(= 0 (mod % 7))
    :pass     1
    :fail     4
    :inspects 0}
   {:items    [71, 72, 74, 76, 68]
    :op       #(* % %)
    :test     #(= 0 (mod % 2))
    :pass     6
    :fail     0
    :inspects 0}
   {:items    [98, 85, 84]
    :op       #(+ 7 %)
    :test     #(= 0 (mod % 19))
    :pass     5
    :fail     7
    :inspects 0}
   {:items    [78]
    :op       #(+ 8 %)
    :test     #(= 0 (mod % 5))
    :pass     3
    :fail     0
    :inspects 0}
   {:items    [86, 70, 60, 88, 88, 78, 74, 83]
    :op       #(+ 4 %)
    :test     #(= 0 (mod % 11))
    :pass     1
    :fail     2
    :inspects 0}
   {:items    [81, 58]
    :op       #(+ 5 %)
    :test     #(= 0 (mod % 17))
    :pass     3
    :fail     5
    :inspects 0}])

(def ^:dynamic *reduce-stress*
  identity)

(defn round
  ([state]
   (reduce
     round
     state
     (range (count state))))
  ([state idx]
   (let [{:keys [items op test pass fail]} (nth state idx)]
     (reduce
       (fn [state item]
         (let [item'  (-> (op item)
                        *reduce-stress*)
               state' (-> state
                        (update-in [idx :inspects] inc)
                        (update-in [idx :items] #(vec (next %))))]
           (if (test item')
             (update-in state' [pass :items] conj item')
             (update-in state' [fail :items] conj item'))))
       state
       items))))

(defn part1 [data]
  (binding [*reduce-stress* #(quot % 3)]
    (->>
      (nth (iterate round data) 20)
      (map :inspects)
      (sort-by -)
      (take 2)
      (reduce * 1))))

(comment
  (round sample1)
  (part1 sample1)
  (part1 data))

(defn part2 [data]
  (binding [*reduce-stress* #(mod % (* 2 3 5 7 11 13 17 19 23))]
    (->>
      (nth (iterate round data) 10000)
      (map :inspects)
      (sort-by -)
      (take 2)
      (reduce * 1))))

(comment
  (part1 sample1)
  (part2 sample1)
  (part2 data))
