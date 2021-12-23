(ns advent-of-code.year2021.day21
  (:require
   [advent-of-code.core :refer [cond+]]
   [clojure.data.priority-map :refer [priority-map]]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.java.math :as math]
   [clojure.math.combinatorics :as comb]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.zip :as z])
  (:import
   [java.io StringReader PushbackReader]))

(set! *warn-on-reflection* true)

(set! *print-namespace-maps* false)

(def example [4 8])

(def input [1 3])

(defn add ^long [^long a ^long b ^long wrap]
  (-> (+ a b) (- 1) (mod wrap) (+ 1)))

(defn part1
  ([] (part1 input))
  ([[p1 p2]]
   (loop [turn  :p1
          p1    (long p1)
          p2    (long p2)
          s1    0
          s2    0
          rolls 0
          die   0]
     (let [p    (case turn :p1 p1 :p2 p2)
           roll (+ (add die 1 100) (add die 2 100) (add die 3 100))
           p'   (add p roll 10)]
      (case turn
        :p1 (if (>= (+ s1 p') 1000)
              (* s2 (+ 3 rolls))
              (recur :p2 p' p2 (+ s1 p') s2 (+ 3 rolls) (add die 3 100)))
        :p2 (if (>= (+ s2 p') 1000)
              (* s1 (+ 3 rolls))
              (recur :p1 p1 p' s1 (+ s2 p') (+ 3 rolls) (add die 3 100))))))))

(deftype LongPair [^long a ^long b])

(defn +* ^LongPair [^LongPair a ^LongPair b ^LongPair c ^LongPair d ^LongPair e ^LongPair f ^LongPair g]
  (LongPair.
    (-> (.-a a)
      (unchecked-add (.-a b))
      (unchecked-add (.-a c))
      (unchecked-add (.-a d))
      (unchecked-add (.-a e))
      (unchecked-add (.-a f))
      (unchecked-add (.-a g)))
    (-> (.-b a)
      (unchecked-add (.-b b))
      (unchecked-add (.-b c))
      (unchecked-add (.-b d))
      (unchecked-add (.-b e))
      (unchecked-add (.-b f))
      (unchecked-add (.-b g)))))

(defn ** ^LongPair [^long n ^LongPair p]
  (LongPair. (* n (.-a p)) (* n (.-b p))))

(declare play-p1)
(declare play-p2)

(defn play-p1-raw [^long p1 ^long p2 ^long s1 ^long s2]
  (if (>= s2 21)
    (LongPair. 0 1)
    (+*
      (let [p' (add p1 3 10)] (** 1 (play-p2 p' p2 (+ s1 p') s2)))
      (let [p' (add p1 4 10)] (** 3 (play-p2 p' p2 (+ s1 p') s2)))
      (let [p' (add p1 5 10)] (** 6 (play-p2 p' p2 (+ s1 p') s2)))
      (let [p' (add p1 6 10)] (** 7 (play-p2 p' p2 (+ s1 p') s2)))
      (let [p' (add p1 7 10)] (** 6 (play-p2 p' p2 (+ s1 p') s2)))
      (let [p' (add p1 8 10)] (** 3 (play-p2 p' p2 (+ s1 p') s2)))
      (let [p' (add p1 9 10)] (** 1 (play-p2 p' p2 (+ s1 p') s2))))))

(defn play-p2-raw [^long p1 ^long p2 ^long s1 ^long s2]
  (if (>= s1 21)
    (LongPair. 1 0)
    (+*
      (let [p' (add p2 3 10)] (** 1 (play-p1 p1 p' s1 (+ s2 p'))))
      (let [p' (add p2 4 10)] (** 3 (play-p1 p1 p' s1 (+ s2 p'))))
      (let [p' (add p2 5 10)] (** 6 (play-p1 p1 p' s1 (+ s2 p'))))
      (let [p' (add p2 6 10)] (** 7 (play-p1 p1 p' s1 (+ s2 p'))))
      (let [p' (add p2 7 10)] (** 6 (play-p1 p1 p' s1 (+ s2 p'))))
      (let [p' (add p2 8 10)] (** 3 (play-p1 p1 p' s1 (+ s2 p'))))
      (let [p' (add p2 9 10)] (** 1 (play-p1 p1 p' s1 (+ s2 p')))))))

(def play-p1 (memoize play-p1-raw))
(def play-p2 (memoize play-p2-raw))

(defn part2
  ([] (part2 input))
  ([[p1 p2]]
   (let [p ^LongPair (play-p1 p1 p2 0 0)]
     (max (.-a p) (.-b p)))))

(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2021.day21 :reload))
  (part1 example)
  (part1 input)
  (time (part2 example))                ; 0.041364 msecs
  (time (part2 input))                  ; 0.043284 msecs
  (require '[clj-async-profiler.core :as profiler])
  (profiler/profile (part2 input)))
