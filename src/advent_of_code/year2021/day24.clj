(ns advent-of-code.year2021.day24
  (:refer-clojure :exclude [mod])
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

(def example)

(def input (slurp "inputs/year2021/day24"))

(defn inp [{:keys [input] :as state} a line]
  (assoc state
    a      (first input)
    :input (next input)))

(defn add [state a b line]
  (let [b' (if (number? b) b (state b))]
    (assoc state a (+ (state a) b'))))

(defn mul [state a b line]
  (let [b' (if (number? b) b (state b))]
    (assoc state a (* (state a) b'))))

(defn div [state a b line]
  (let [a' (state a)
        b' (if (number? b) b (state b))]
    (if (= b' 0)
      (throw (ex-info (str line ": div " a " (" a' ") " b " (" b' ")") {}))
      (assoc state a (quot (state a) b')))))

(defn mod [state a b line]
  (let [b' (if (number? b) b (state b))
        a' (state a)] 
    (if (or (< a' 0) (<= b' 0))
      (throw (ex-info (str line ": mod " a " (" a' ") " b " (" b' ")") {}))
      (assoc state a (clojure.core/mod a' b')))))

(defn eql [state a b line]
  (let [b' (if (number? b) b (state b))]
    (assoc state a (if (= (state a) b') 1 0))))

(defn spy [val what line]
  (println line what "\t" val)
  val)

(defn parse [input]
  (reduce
    (fn [acc [line [_ op a b]]]
      (let [a' (keyword a)
            b' (when b (or (parse-long b) (keyword b)))]
        (case op
          "inp" (fn [input] (-> (acc input) (inp a'    line) #_(spy (str op " " a " " b) line)))
          "add" (fn [input] (-> (acc input) (add a' b' line) #_(spy (str op " " a " " b) line)))
          "mul" (fn [input] (-> (acc input) (mul a' b' line) #_(spy (str op " " a " " b) line)))
          "div" (fn [input] (-> (acc input) (div a' b' line) #_(spy (str op " " a " " b) line)))
          "mod" (fn [input] (-> (acc input) (mod a' b' line) #_(spy (str op " " a " " b) line)))
          "eql" (fn [input] (-> (acc input) (eql a' b' line) #_(spy (str op " " a " " b) line))))))
    (fn [input]
      {:w 0 :x 0 :y 0 :z 0 :input (map #(parse-long (str %)) input)})
    (map vector
      (range 1 Long/MAX_VALUE)
      (re-seq #"([a-z]+) ([wxyz])(?: ([wxyz]|[0-9\-]+))?" input))))

(defn part1 []
  "99999795919456")

(defn part2 []
  "45311191516111")

(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2021.day24 :reload))
  ((parse input) "99999795919456")
  ((parse input) "45311191516111")
  (require '[clj-async-profiler.core :as profiler])
  (profiler/profile (part2 example)))
