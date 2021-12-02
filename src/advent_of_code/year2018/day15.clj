
(ns advent-of-code.year2018.day15
  (:require
   [advent-of-code.core :refer [cond+]]
   [clojure.java.io :as io]
   [clojure.java.math :as math]
   [clojure.string :as str]
   [clojure.set :as set]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.window :as window]
   [nrepl.cmdline :as nrepl])
  (:import
   [java.nio.file Files Path]
   [java.util Arrays BitSet]
   [io.github.humbleui.skija Canvas Font Image Paint Rect]))

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

(defrecord Unit [who attack hp])

(def wall (Unit. :wall 0 0))

(def font (Font. nil (float 26)))

(defrecord Game [width height ^BitSet map units])

(defn parse [input]
  (let [lines  (str/split input #"\n")
        height (count lines)
        width  (count (first lines))
        map    (BitSet. (* width height))]
    (loop [y     0
           x     0
           units (sorted-map)]
      (cond+
        (>= y height)
        (Game. width height map units)

        (>= x width)
        (recur (inc y) 0 units)

        :let [tile (nth (nth lines y) x)]

        (= \# tile)
        (do
          (.set map (+ (* y width) x))
          (recur y (inc x) units))

        (= \. tile)
        (recur y (inc x) units)

        (= \G tile)
        (recur y (inc x) (assoc units (Pos. x y) (Unit. :goblin 3 200)))

        (= \E tile)
        (recur y (inc x) (assoc units (Pos. x y) (Unit. :elf 3 200)))))))

(defonce *window (atom nil))
(def *game (atom (parse example1)))

(def sprite-floor  (Image/makeFromEncoded (Files/readAllBytes (Path/of "inputs/year2018/day15/floor.png" (make-array String 0)))))
(def sprite-wall   (Image/makeFromEncoded (Files/readAllBytes (Path/of "inputs/year2018/day15/wall.png" (make-array String 0)))))
(def sprite-goblin (Image/makeFromEncoded (Files/readAllBytes (Path/of "inputs/year2018/day15/goblin.png" (make-array String 0)))))
(def sprite-elf    (Image/makeFromEncoded (Files/readAllBytes (Path/of "inputs/year2018/day15/elf.png" (make-array String 0)))))

(defn game-get [^Game game ^Pos pos]
  (or
    ((.-units game) pos)
    (when (.get (.-map game) (-> (.-y pos) (* (.-width game)) (+ (.-x pos))))
      wall)))

(defn on-paint [window ^Canvas canvas game]
  (.clear canvas (unchecked-int 0xFFEEEEEE))
  (let [bounds      (.getContentRect (window/jwm-window window))
        sprite-size (math/floor
                      (min
                        (/ (.getWidth bounds) (:width game))
                        (/ (.getHeight bounds) (:height game))))
        sprite-rect (Rect/makeXYWH 0 0 sprite-size sprite-size)
        draw-unit   (fn [unit]
                      (case (:who unit)
                        :goblin (.drawImageRect canvas sprite-goblin sprite-rect)
                        :elf    (.drawImageRect canvas sprite-elf sprite-rect))
                      (with-open [paint (Paint.)]
                        (let [width (* (:hp unit) (- sprite-size 4) (/ 1 200))]
                          (.setColor paint (unchecked-int 0xFF33CC33))
                          (.drawRect canvas (Rect/makeXYWH 2 0 width 6) paint)
                          (.setColor paint (unchecked-int 0xFFCC3333))
                          (.drawRect canvas (Rect/makeXYWH (+ width 2) 0 (- sprite-size 4 width) 6) paint)
                          (.setColor paint (unchecked-int 0xFFFFFFFF))
                          ; (.drawString canvas (str (:hp unit)) 0 20 font paint)
                          )))]
    (.translate canvas
      (-> (.getWidth bounds) (- (* (:width game) sprite-size)) (quot 2))
      (-> (.getHeight bounds) (- (* (:height game) sprite-size)) (quot 2)))
    (doseq [y    (range 0 (:height game))
            x    (range 0 (:width game))
            :let [obj (game-get game (Pos. x y))]]
      (.save canvas)
      (.translate canvas (* x sprite-size) (* y sprite-size))
      (.drawImageRect canvas sprite-floor sprite-rect)
      ; (draw-unit nil)
      (cond
        (= wall obj) (.drawImageRect canvas sprite-wall sprite-rect)
        (some? obj)   (draw-unit obj))
      (.restore canvas))))

(comment
  (window/request-frame @*window))

(defn make-window []
  (let [w (window/make
            {:on-close (fn [_] (reset! *window nil))
             :on-paint (fn [window ^Canvas canvas]
                         (let [layer (.save canvas)]
                           (try
                             (let [on-paint (resolve 'advent-of-code.year2018.day15/on-paint)
                                   *game    (resolve 'advent-of-code.year2018.day15/*game)]
                               (@on-paint window canvas @@*game)
                               (window/request-frame window))
                             (catch Exception e
                               (.printStackTrace e)
                               (.clear canvas (unchecked-int 0xFFCC3333))))
                           (.restoreToCount canvas layer)))})]
    (window/set-title w "Goblins vs Elves")
    (window/set-visible w true)
    ; (.setContentSize (window/jwm-window w) 1000 1000)
    (.setWindowPosition (window/jwm-window w) 2836 632)
    (.setWindowSize (window/jwm-window w) 1000 1588)
    (window/set-z-order w :floating)
    (window/request-frame w)
    w))

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

(defn make-attack [game pos unit]
  (let [{:keys [who attack]} unit
        enemy  (case who :goblin :elf :elf :goblin)
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
        enemy       (case who :goblin :elf :elf :goblin)
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
    (when ((:units game) pos)
      (or
        (make-attack game pos unit)
        (when-some [[game' pos'] (make-move game pos unit)]
          (or (make-attack game' pos' unit) game'))))
    game))

(defn make-turn [game]
  (reduce-kv make-turn-unit game (:units game)))

(defn play!
  ([input] (play! input 0))
  ([input delay]
   (reset! *game (parse input))
   (loop [turn 0]
     (let [game  @*game
           game' (swap! *game make-turn)]
       (if (identical? game game')
         (let [turn (dec turn)
               hp   (reduce-kv (fn [acc _ unit] (+ acc (:hp unit))) 0 (:units game))]
           {:answer (* turn hp)
            :hp     hp
            :turn   turn})
         (do
           (when (pos? delay)
             (Thread/sleep delay))
           (recur (inc turn))))))))

(defn -main [& args]
  (future (apply nrepl/-main args))
  (hui/init)
  (reset! *window (make-window))
  (hui/start))

(comment
  (swap! *game make-turn)
  (mapv (fn [[_ unit]] (:hp unit)) (:units @*game))
  (reset! *game (parse example2))
  (= 27730 (:answer (play! example1)))
  (play! example1 16)
  (= 36334 (:answer (play! example2)))
  (= 39514 (:answer (play! example3)))
  (= 27755 (:answer (play! example4)))
  (= 28944 (:answer (play! example5)))
  (= 18740 (:answer (play! example6)))
  (play! problem)
  (play! problem 100)
  
  (window/request-frame @*window)

  (.getColorType (.getColorInfo (.getImageInfo sprite-goblin)))
  (.getWindowRect (window/jwm-window @*window))
  (hui/doui (window/close @*window))
  (reset! *window (hui/doui (make-window)))
  
  (hui/doui (window/set-z-order @*window :normal))
  (hui/doui (window/set-z-order @*window :floating))
)