(ns advent-of-code.year2018.day15
  (:require
   [advent-of-code.core :refer [cond+]]
   [clojure.java.io :as io]
   [clojure.java.math :as math]
   [clojure.string :as str]
   [clojure.set :as set])
  (:import
   [java.util Arrays BitSet]))

(def example1
"#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######")

(def example2
"#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######")

(def example3
"#######
#E..EG#
#.#G.E#
#E.##E#
#G..#.#
#..E#.#
#######")

(def example4
"#######
#E.G#.#
#.#G..#
#G.#.G#
#G..#.#
#...E.#
#######")

(def example5
"#######
#.E...#
#.#..G#
#.###.#
#E#G#G#
#...#G#
#######")

(def example6
"#########
#G......#
#.E.#...#
#..##..G#
#...##..#
#...#...#
#.G...G.#
#.....G.#
#########")

(def problem (slurp "inputs/year2018/day15/input"))

(defrecord Pos [^long x ^long y]
  java.lang.Comparable
  (compareTo [_ o]
    (cond
      (< y (.-y ^Pos o)) -1
      (> y (.-y ^Pos o))  1
      (< x (.-x ^Pos o)) -1
      (> x (.-x ^Pos o))  1
      :else               0)))

(defrecord Unit [who attack hp id])

(def wall (Unit. :wall 0 0 0))

(defrecord Game [width height ^BitSet map units turn over?])

(defn parse
  ([input] (parse input {}))
  ([input {:keys [elf-attack]
           :or {elf-attack 3}}]
   (let [lines  (str/split input #"\n")
         height (count lines)
         width  (count (first lines))
         map    (BitSet. (* width height))]
     (loop [y     0
            x     0
            units (sorted-map)
            id    1]
       (cond+
         (>= y height)
         (Game. width height map units 0 false)
         
         (>= x width)
         (recur (inc y) 0 units id)
         
         :let [tile (nth (nth lines y) x)]
         
         (= \# tile)
         (do
           (.set map (+ (* y width) x))
           (recur y (inc x) units id))
         
         (= \. tile)
         (recur y (inc x) units id)
         
         (= \G tile)
         (recur y (inc x) (assoc units (Pos. x y) (Unit. :goblin 3 200 id)) (inc id))
         
         (= \E tile)
         (recur y (inc x) (assoc units (Pos. x y) (Unit. :elf elf-attack 200 id)) (inc id)))))))

(defn game-get [^Game game ^Pos pos]
  (or
    ((.-units game) pos)
    (when (.get (.-map game) (-> (.-y pos) (* (.-width game)) (+ (.-x pos))))
      wall)))

(defn surrounding [^Game game ^Pos pos]
  (not-empty
    (cond-> []
      (> (.-y pos) 0)                     (conj (Pos. (.-x pos) (dec (.-y pos))))
      (> (.-x pos) 0)                     (conj (Pos. (dec (.-x pos)) (.-y pos)))
      (< (.-x pos) (dec (.-width game)))  (conj (Pos. (inc (.-x pos)) (.-y pos)))
      (< (.-y pos) (dec (.-height game))) (conj (Pos. (.-x pos) (inc (.-y pos)))))))

(defn adjacent?
  "Checks if pos is adjacent to who"
  [game pos who]
  (->> pos
    (surrounding game)
    (keep #(game-get game %))
    (some #(= (:who %) who))))

(defn enemy [who]
  (case who :goblin :elf :elf :goblin))

(defn make-attack [game pos unit]
  (let [{:keys [who attack]} unit
        enemy  (enemy who)
        target (->> (surrounding game pos)
                 (filter #(= enemy (:who (game-get game %))))
                 (sort-by #(vector (:hp (game-get game %)) %))
                 (first))]
    (when target
      (if (<= (:hp (game-get game target)) attack)
        (update game :units dissoc target)
        (update game :units update target update :hp - attack)))))

(defn make-move
  "     1
        ↑
   2 <- _ -> 3
        ↓
        4"
  [game pos unit]
  (let [{w :width h :height} game
        {who :who}  unit
        enemy       (enemy who)
        {x :x y :y} pos
        pos->idx    #(+ (:x %) (* (:y %) w))
        map         ^longs (make-array Long/TYPE (* w h))
        *border     (atom (sorted-set))]
    (Arrays/fill map Long/MAX_VALUE)
    (aset map (pos->idx pos) 0)
    (when (> y 0)
      (let [pos' (Pos. x (dec y))]
        (when (nil? (game-get game pos'))
          (aset map (pos->idx pos') 11)
          (swap! *border conj pos'))))
    (when (> x 0)
      (let [pos' (Pos. (dec x) y)]
        (when (nil? (game-get game pos'))
          (aset map (pos->idx pos') 12)
          (swap! *border conj pos'))))
    (when (< x (dec w))
      (let [pos' (Pos. (inc x) y)]
        (when (nil? (game-get game pos'))
          (aset map (pos->idx pos') 13)
          (swap! *border conj pos'))))
    (when (< y (dec h))
      (let [pos' (Pos. x (inc y))]
        (when (nil? (game-get game pos'))
          (aset map (pos->idx pos') 14)
          (swap! *border conj pos'))))
    (loop [border @*border]
      (cond+
        ;; nowhere to go
        (empty? border)
        nil
        
        :let [adjacent (filter #(adjacent? game % enemy) border)]
        
        ;; found an enemy, go
        (seq adjacent)
        (let [target    (first (sort adjacent))
              distance  (aget map (pos->idx target))
              direction (mod distance 10)
              pos'      (case direction
                          1 (Pos. x (dec y))
                          2 (Pos. (dec x) y)
                          3 (Pos. (inc x) y)
                          4 (Pos. x (inc y)))]
          [(-> game
             (update :units dissoc pos)
             (update :units assoc pos' unit)) pos'])

        ;; expand border
        :else
        (recur
          (->> border
            (mapcat
              (fn [pos]
                (let [distance' (+ (aget map (pos->idx pos)) 10)
                      adjacent  (->> (surrounding game pos)
                                  (remove #(game-get game %)) ;; can’t walk
                                  (remove #(<= (aget map (pos->idx %)) distance')))] ;; seen, closer path exists
                  (doseq [pos adjacent]
                    (aset map (pos->idx pos) distance'))
                  adjacent)))
            (apply sorted-set)))))))

(defn make-turn-unit [game pos unit]
  (or
    (let [unit' ((:units game) pos)]
      (when (= (:id unit') (:id unit)) ;; still alive
        (if (empty? (remove #(= (:who %) (:who unit)) (vals (:units game)))) ;; no enemies
          (assoc game :over? true)
          (or
            (make-attack game pos unit')
            (when-some [[game' pos'] (make-move game pos unit')]
              (or (make-attack game' pos' unit') game'))))))
    game))

(defn make-turn [game]
  (update
    (reduce-kv make-turn-unit game (:units game))
    :turn inc))

(defn solve [input {:keys [delay on-turn]
                    :or {on-turn identity}
                    :as opts}]
 (let [game  (parse input opts)
       elves (count (filter #(= :elf (:who %)) (vals (:units game))))]
   (on-turn game)
   (loop [game game]
     (let [game' (make-turn game)]
       (on-turn game)
       (if (:over? game)
         (let [turn (dec (:turn game))
               hp   (reduce-kv (fn [acc _ unit] (+ acc (:hp unit))) 0 (:units game))]
           {:answer (* turn hp)
            :hp     hp
            :turn   turn
            :deaths (- elves (count (filter #(= :elf (:who %)) (vals (:units game)))))})
         (do
           (when delay
             (Thread/sleep delay))
           (recur game')))))))

(defn part1
  ([] (part1 problem {}))
  ([input] (part1 input {}))
  ([input opts]
    (:answer (solve input opts))))

(defn part2
  ([] (part2 problem {}))
  ([input] (part2 input {}))
  ([input {:keys [delay] :as opts}]
    (loop [attack 4]
      (let [{:keys [deaths] :as result} (solve input (assoc opts :elf-attack attack))]
        (if (= 0 deaths)
          (:answer result)
          (recur (inc attack)))))))

(comment
  (= 27730 (:answer (part1 example1)))
  (= 36334 (:answer (part1 example2)))
  (= 39514 (:answer (part1 example3)))
  (= 27755 (:answer (part1 example4)))
  (= 28944 (:answer (part1 example5)))
  (= 18740 (:answer (part1 example6)))

  (= 4988 (:answer (part2 example1)))
  (part2 example2)
  (= 31284 (:answer (part2 example3)))
  (= 3478  (:answer (part2 example4)))
  (= 6474  (:answer (part2 example5)))
  (= 1140  (:answer (part2 example6))))