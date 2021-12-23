(ns ^{:doc "https://adventofcode.com/2021/day/23"}
  advent-of-code.year2021.day23
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

(def example
  "█████████████
   █           █
   ███B█C█B█D███
     █A█D█C█A█
     █████████"
  {:c [:B :A]
   :e [:C :D]
   :g [:B :C]
   :i [:D :A]})

(def input
  "█████████████
   █           █
   ███A█D█A█C███
     █C█D█B█B█
     █████████" 
  {:c [:A :C]
   :e [:D :D]
   :g [:A :B]
   :i [:C :B]})

(def example2
  "█████████████
   █           █
   ███B█C█B█D███
     █D█C█B█A█
     █D█B█A█C█
     █A█D█C█A█
     █████████"
  {:c [:B :D :D :A]
   :e [:C :C :B :D]
   :g [:B :B :A :C]
   :i [:D :A :C :A]})

(def input2
  "█████████████
   █           █
   ███A█D█A█C███
     █D█C█B█A█
     █D█B█A█C█
     █C█D█B█B█
     █████████"
  {:c [:A :D :D :C]
   :e [:D :C :B :D]
   :g [:A :B :A :B]
   :i [:C :A :C :B]})  

(def graph
  "█████████████
   █ab d f h jk█
   ███c█e█g█i███"
  {:a {:c 2, :e 4, :g 6, :i 8}
   :b {:c 1, :e 3, :g 5, :i 7}
   :d {:c 1, :e 1, :g 3, :i 5}
   :f {:c 3, :e 1, :g 1, :i 3}
   :h {:c 5, :e 3, :g 1, :i 1}
   :j {:c 7, :e 5, :g 3, :i 1}
   :k {:c 8, :e 6, :g 4, :i 2}

   :c {:a 2, :b 1, :d 1, :f 3, :h 5, :j 7, :k 8}
   :e {:a 4, :b 3, :d 1, :f 1, :h 3, :j 5, :k 6}
   :g {:a 6, :b 5, :d 3, :f 1, :h 1, :j 3, :k 4}
   :i {:a 8, :b 7, :d 5, :f 3, :h 1, :j 1, :k 2}})

(def on-path
  {#{:a :c} #{:b}
   #{:a :e} #{:b :d}
   #{:a :g} #{:b :d :f}
   #{:a :i} #{:b :d :f :h}
   #{:b :c} #{}
   #{:b :e} #{:d}
   #{:b :g} #{:d :f}
   #{:b :i} #{:d :f :h}
   #{:d :c} #{}
   #{:d :e} #{}
   #{:d :g} #{:f}
   #{:d :i} #{:f :h}
   #{:f :c} #{:d}
   #{:f :e} #{}
   #{:f :g} #{}
   #{:f :i} #{:h}
   #{:h :c} #{:f :d}
   #{:h :e} #{:f}
   #{:h :g} #{}
   #{:h :i} #{}
   #{:j :c} #{:h :f :d}
   #{:j :e} #{:h :f}
   #{:j :g} #{:h}
   #{:j :i} #{}
   #{:k :c} #{:j :h :f :d}
   #{:k :e} #{:j :h :f}
   #{:k :g} #{:j :h}
   #{:k :i} #{:j}})

(defn path-clear? [pos p p']
  (every? #(nil? (pos %)) (on-path #{p p'})))

(defn moves-from-room [{:keys [pos energy path] :as state} p]
  (let [room (pos p)
        [nils habitants] (split-with nil? room)]
    (cond
      (empty? habitants) nil
      (and (= :c p) (every? #(= :A %) habitants)) nil
      (and (= :e p) (every? #(= :B %) habitants)) nil
      (and (= :g p) (every? #(= :C %) habitants)) nil
      (and (= :i p) (every? #(= :D %) habitants)) nil
      :else
      (let [who   (first habitants)
            room' (concat nils [nil] (next habitants))]
        (for [p' [:a :b :d :f :h :j :k]
              :when (and (nil? (pos p')) (path-clear? pos p p'))
              :let  [cost (-> ((graph p) p')
                            (+ 1 (count nils))
                            (* ({:A 1 :B 10 :C 100 :D 1000} who)))]]
          {:pos    (assoc pos p room' p' who)
           :energy (+ energy cost)})))))

(defn moves-from-hallway [{:keys [pos energy path] :as state} p]
  (let [who  (pos p)
        p'   ({:A :c :B :e :C :g :D :i} who)
        room (pos p')
        [nils habitants] (split-with nil? room)]
    (when (and (every? #(= who %) habitants)
            (path-clear? pos p p'))
      (let [room' (concat (next nils) [who] habitants)
            cost  (-> ((graph p) p')
                    (+ (count nils))
                    (* ({:A 1 :B 10 :C 100 :D 1000} who)))]
        [{:pos    (-> pos (dissoc p) (assoc p' room'))
          :energy (+ energy cost)}]))))

(defn moves [state]
  (for [[p _] (:pos state)
        move (if (#{:c :e :g :i} p)
               (moves-from-room state p)
               (moves-from-hallway state p))]
    move))

(defn endgame? [{:keys [pos]}]
  (and
    (every? #(= :A %) (:c pos))
    (every? #(= :B %) (:e pos))
    (every? #(= :C %) (:g pos))
    (every? #(= :D %) (:i pos))))

(defn solve
  ([pos] (solve (atom {}) (atom Long/MAX_VALUE) {:pos pos :energy 0}))
  ([*seen *min {:keys [pos energy] :as state}]
   (cond
     (endgame? state)
     (do
       (swap! *min min energy)
       energy)

     (>= energy @*min)
     Long/MAX_VALUE

     (>= energy (@*seen pos Long/MAX_VALUE))
     Long/MAX_VALUE

     :else
     (do
       (swap! *seen assoc pos energy)
       (transduce
         (map #(solve *seen *min %))
         min
         Long/MAX_VALUE
         (moves state))))))

(defn part1 []
  (solve input))

(defn part2 []
  (solve input2))

(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2021.day23 :reload))
  (solve example)
  (solve input)
  (solve example2)
  (solve input2)
  (require '[clj-async-profiler.core :as profiler])
  (profiler/profile (part1 example0)))
