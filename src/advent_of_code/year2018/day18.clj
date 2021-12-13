(ns advent-of-code.year2018.day18
  (:require
   [advent-of-code.core :refer [cond+]]
   [clojure.java.io :as io]
   [clojure.java.math :as math]
   [clojure.string :as str]
   [clojure.set :as set]))

(set! *warn-on-reflection* true)

(def example (slurp "inputs/year2018/day18.example"))

(def problem (slurp "inputs/year2018/day18"))

(def GROUND \.)
(def TREE   \|)
(def LUMBER \#)

(defn parse [input]
  (let [lines  (str/split-lines input)
        width  (count (first lines))
        height (count lines)
        array  (for [y (range height)
                     :let [line ^String (nth lines y)]
                     x (range width)]
                 (.charAt line x))]
    {:width  width
     :height height
     :array  (vec array)}))

(defn neighbours [{:keys [width height]} [x y]]
  (let [maxx (dec width)
        maxy (dec height)]
    (cond-> []
      (> x 0)                     (conj [(dec x) y])
      (< x maxx)                  (conj [(inc x) y])
      (> y 0)                     (conj [x (dec y)])
      (< y maxy)                  (conj [x (inc y)])
      (and (> x 0) (> y 0))       (conj [(dec x) (dec y)])
      (and (> x 0) (< y maxy))    (conj [(dec x) (inc y)])
      (and (< x maxx) (> y 0))    (conj [(inc x) (dec y)])
      (and (< x maxx) (< y maxy)) (conj [(inc x) (inc y)]))))

(defn char-at [chart [x y]]
  (nth (:array chart) (+ (* (:width chart) y) x)))

(defn step [chart]
  (let [{:keys [width height array]} chart
        array' (for [y (range height)
                      x (range width)
                      :let [pos    [x y]
                            val    (char-at chart pos)
                            around (->> (neighbours chart pos)
                                     (map #(char-at chart %))
                                     (frequencies))]]
                  (cond
                    (and (= GROUND val) (>= (around TREE 0) 3))
                    TREE

                    (and (= TREE val) (>= (around LUMBER 0) 3))
                    LUMBER

                    (and (= LUMBER val) (or (= 0 (around LUMBER 0)) (= 0 (around TREE 0))))
                    GROUND

                    :else
                    val))]
    {:width  width
     :height height
     :array  (vec array')}))

(defn score [chart]
  (let [{:keys [width height array]} chart
        {lumber LUMBER trees TREE} (frequencies
                                     (for [y (range height)
                                           x (range width)]
                                       (char-at chart [x y])))]
    (* (or lumber 0) (or trees 0))))

(defn part1
  ([] (part1 problem))
  ([input]
   (score
     (reduce
       (fn [chart _] (step chart))
       (parse input)
       (range 10)))))

(defn part2
  ([] (part2 problem))
  ([input]
   (let [{:keys [charts start end]}
         (loop [chart  (parse input)
                steps  {chart 0}
                charts {0 chart}
                end    1]
           (let [chart' (step chart)]
             (if-some [start (steps chart')]
               {:start  start
                :end    end
                :charts charts}
               (recur chart' (assoc steps chart' end) (assoc charts end chart') (inc end)))))
         i (mod (- 1000000000 start) (- end start))]
     (score (charts (+ i start))))))

(comment
  (part1 example)
  (part1)
  (part2 example)
  (part2))