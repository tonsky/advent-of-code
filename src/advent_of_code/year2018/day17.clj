(ns advent-of-code.year2018.day17
  (:require
   [advent-of-code.core :refer [cond+]]
   [clojure.java.io :as io]
   [clojure.java.math :as math]
   [clojure.string :as str]
   [clojure.set :as set])
  (:import
   [java.util Arrays BitSet]))

(def example (slurp "inputs/year2018/day17.example"))

(def example2 (slurp "inputs/year2018/day17.example2"))

(def problem (slurp "inputs/year2018/day17"))

(defrecord Map [xmin ymin xmax ymax width height ^bytes array sources changed?])

(def WATER (byte \~))
(def SAND (byte 0))
(def CLAY (byte \#))
(def DRY (byte \|))

(defn parse-line [line]
  (if-some [[x y1 y2] (some->> (re-matches #"x=(\d+), y=(\d+)\.\.(\d+)" line)
                        (next)
                        (map parse-long))]
    [x (min y1 y2) x (max y1 y2)]
    (if-some [[y x1 x2] (some->> (re-matches #"y=(\d+), x=(\d+)\.\.(\d+)" line)
                          (next)
                          (map parse-long))]
      [(min x1 x2) y (max x1 x2) y]
      (throw (ex-info (str "Can't parse a line: " line) {:line line})))))

(defn xy->idx [map x y]
  (+ (- x (:xmin map))
    (* (:width map) (- y (:ymin map)))))

(defn map-get [map x y]
  (aget ^bytes (:array map) (xy->idx map x y)))

(defn map-set [map x y val]
  (aset ^bytes (:array map) (xy->idx map x y) (byte val))
  map)

(defn parse [input]
  (let [lines  (mapv parse-line (str/split input #"\n"))
        xmin   (dec (reduce min (mapcat (juxt #(nth % 0) #(nth % 2)) lines)))
        xmax   (inc (reduce max (mapcat (juxt #(nth % 0) #(nth % 2)) lines)))
        ymin   (reduce min (mapcat (juxt #(nth % 1) #(nth % 3)) lines))
        ymax   (reduce max (mapcat (juxt #(nth % 1) #(nth % 3)) lines))
        width  (inc (- xmax xmin))
        height (inc (- ymax ymin))
        map    (Map.
                 xmin
                 ymin
                 xmax
                 ymax
                 width
                 height
                 (make-array Byte/TYPE (* width height))
                 [[500 ymin]]
                 false)]
    (doseq [[x1 y1 x2 y2] lines]
      (if (= x1 x2)
        (doseq [y (range y1 (inc y2))]
          (map-set map x1 y CLAY))
        (if (= y1 y2)
          (doseq [x (range x1 (inc x2))]
            (map-set map x y1 CLAY)))))
    map))

(defn pass? [val]
  (or (= SAND val) (= DRY val)))

(defn xleft [map x y]
  (loop [x x]
    (cond
      (not (pass? (map-get map (dec x) y))) x
      (pass? (map-get map x (inc y))) x
      :else (recur (dec x)))))

(defn xright [map x y]
  (loop [x x]
    (cond
      (not (pass? (map-get map (inc x) y))) x
      (pass? (map-get map x (inc y))) x
      :else (recur (inc x)))))

(defn step
  ([map]
   (if-some [source (peek (:sources map))]
     (step (update map :sources pop) source)
     map))
  ([map [x0 y0]]
   (if (= WATER (map-get map x0 y0))
     (update map :sources conj [x0 (dec y0)])
     (loop [y y0]
       (cond+
         ; fall down beneath map bottom
         (>= y (:ymax map))
         (do
           (doseq [y (range y0 (inc y))]
             (map-set map x0 y DRY))
           map)
         
         :let [val (map-get map x0 (inc y))]
         
         ; fall down
         (pass? val) (recur (inc y))
         
         :let [xleft (xleft map x0 y)
               xright (xright map x0 y)]

         :do (doseq [y (range y0 (inc y))]
               (map-set map x0 y DRY))
         
         ; new sources
         (or
           (pass? (map-get map xleft (inc y)))
           (pass? (map-get map xright (inc y))))
         (do
           (doseq [x (range xleft (inc xright))]
             (map-set map x y DRY))
           (let [sources (concat
                           (when (pass? (map-get map xleft (inc y)))
                             [[xleft y]])
                           (when (pass? (map-get map xright (inc y)))
                             [[xright y]]))]
             (update map :sources into sources)))
         
         ; bounded from both sides
         :else
         (do
           (doseq [x (range xleft (inc xright))]
             (map-set map x y WATER))
           (update map :sources conj [x0 y0])))))))

(defn score [map what]
  (let [*res (volatile! 0)]
    (doseq [y (range (:ymin map) (inc (:ymax map)))
            x (range (:xmin map) (inc (:xmax map)))
            :let [val (map-get map x y)]
            :when (what val)]
      (vswap! *res inc))
    @*res))

(defn solve [input {:keys [on-turn]}]
  (loop [map (parse input)]
    (when on-turn
      (on-turn map))
    (if (seq (:sources map))
      (recur (step map))
      map)))

(defn part1
  ([] (part1 problem {}))
  ([input] (part1 input {}))
  ([input opts]
    (score (solve input opts) #{WATER DRY})))

(defn part2
  ([] (part2 problem {}))
  ([input] (part2 input {}))
  ([input opts]
    (score (solve input opts) #{WATER})))

(comment
  (part1 example)
  (part1)
  (part2 example)
  (part2))
