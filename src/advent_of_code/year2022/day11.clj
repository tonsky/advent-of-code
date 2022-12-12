(ns advent-of-code.year2022.day11
  (:require
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [clojure.set :as set]
    [clj-async-profiler.core :as profiler]
    [remote-require.core :as rr]
    [instaparse.core :as insta]))

(def parser
  (insta/parser
    "Monkeys = Monkey+
     Monkey  = <'Monkey '> id <':'> <newline> Items <newline> Op <newline> Test <newline> Cond <newline> Cond <newline*>
     id      = number
     Items   = <'Starting items: '> number (<', '> number)*
     Op      = <'Operation: new = old '> ('+' | '*') <' '> ('old' | number)
     Test    = <'Test: divisible by '> number
     Cond    = <'If '> ('true' | 'false') <': throw to monkey '> number
     newline = '\n' ' '*
     number  = #'[0-9]+'"))

(def transform
  {:number parse-long
   :Items  (fn [& items]
             [:items (vec items)])
   :Op     (fn [operand value]
             [:op
              (fn [x]
                ((case operand "+" + "*" *) x (case value "old" x value)))])
   :Test   (fn [n]
             [:test #(= 0 (mod % n))])
   :Cond   (fn [val id]
             (case val
               "true"  [:pass id]
               "false" [:fail id]))
   :Monkey (fn [& entries]
             (into {:inspects 0} entries))
   :Monkeys vector})

(comment
  (insta/transform transform
    (parser
      "Monkey 0:
       Starting items: 89, 73
       Operation: new = old * 3
       Test: divisible by 13
       If true: throw to monkey 6
       If false: throw to monkey 2"))
  (insta/transform transform
    (parser (slurp (io/file "inputs/year2022/day11")))))

(def sample1
  "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

(def data
  (slurp (io/file "inputs/year2022/day11")))

(defn parse [s]
  (->> s
    parser
    (insta/transform transform)))

(def ^:dynamic *reduce-stress*
  identity)

(defn round
  ([state]
   (reduce round state (range (count state))))
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
      (nth (iterate round (parse data)) 20)
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
      (nth (iterate round (parse data)) 10000)
      (map :inspects)
      (sort-by -)
      (take 2)
      (reduce * 1))))

(comment
  (part1 sample1)
  (part2 sample1)
  (part2 data))
