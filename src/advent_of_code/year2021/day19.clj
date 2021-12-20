(ns advent-of-code.year2021.day19
  (:require
   [advent-of-code.core :as core :refer [cond+]]
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

(def example (slurp "inputs/year2021/day19.example"))

(def input (slurp "inputs/year2021/day19"))

(defrecord Point [^long x ^long y ^long z])

(defrecord Scanner [beacons distances])

(defn eq [^Point a ^Point b]
  (and
    (= (.-x a) (.-x b))
    (= (.-y a) (.-y b))
    (= (.-z a) (.-z b))))

(defn dist ^long [^Point a ^Point b]
  (+ 
    (* (- (.-x a) (.-x b)) (- (.-x a) (.-x b))) 
    (* (- (.-y a) (.-y b)) (- (.-y a) (.-y b)))
    (* (- (.-z a) (.-z b)) (- (.-z a) (.-z b)))))

(defn scanner [beacons]
  (Scanner. (set beacons) (frequencies (for [a beacons b beacons :when (not (eq a b))] (dist a b)))))

(defn parse-scanner [s]
  (scanner
    (->> (edn/read-string (str "[" s "]"))
      (partition 3)
      (map (fn [[x y z]] (Point. x y z))))))

(defn parse [input]
  (let [scanners (->> (str/split input #"--- scanner \d+ ---")
                   (remove str/blank?)
                   (mapv parse-scanner))]
    scanners))

(def rotations
  [(fn ^Point [^Point p] (Point.    (.-x p)     (.-y p)     (.-z p) ))
   (fn ^Point [^Point p] (Point.    (.-x p)  (- (.-z p))    (.-y p) ))
   (fn ^Point [^Point p] (Point.    (.-x p)  (- (.-y p)) (- (.-z p))))
   (fn ^Point [^Point p] (Point.    (.-x p)     (.-z p)  (- (.-y p))))

   (fn ^Point [^Point p] (Point. (- (.-x p))    (.-y p)  (- (.-z p))))
   (fn ^Point [^Point p] (Point. (- (.-x p))    (.-z p)     (.-y p) ))
   (fn ^Point [^Point p] (Point. (- (.-x p)) (- (.-y p))    (.-z p) ))
   (fn ^Point [^Point p] (Point. (- (.-x p)) (- (.-z p)) (- (.-y p))))

   (fn ^Point [^Point p] (Point.    (.-y p)  (- (.-x p))    (.-z p) ))
   (fn ^Point [^Point p] (Point.    (.-y p)  (- (.-z p)) (- (.-x p))))
   (fn ^Point [^Point p] (Point.    (.-y p)     (.-x p)  (- (.-z p))))
   (fn ^Point [^Point p] (Point.    (.-y p)     (.-z p)     (.-x p) ))
   
   (fn ^Point [^Point p] (Point. (- (.-y p))    (.-x p)     (.-z p)))
   (fn ^Point [^Point p] (Point. (- (.-y p)) (- (.-z p))    (.-x p)))
   (fn ^Point [^Point p] (Point. (- (.-y p)) (- (.-x p)) (- (.-z p))))
   (fn ^Point [^Point p] (Point. (- (.-y p))    (.-z p)  (- (.-x p))))
   
   (fn ^Point [^Point p] (Point.    (.-z p)     (.-y p)  (- (.-x p))))
   (fn ^Point [^Point p] (Point.    (.-z p)     (.-x p)     (.-y p)))
   (fn ^Point [^Point p] (Point.    (.-z p)  (- (.-y p))    (.-x p)))
   (fn ^Point [^Point p] (Point.    (.-z p)  (- (.-x p)) (- (.-y p))))

   (fn ^Point [^Point p] (Point. (- (.-z p))    (.-y p)     (.-x p)))
   (fn ^Point [^Point p] (Point. (- (.-z p)) (- (.-x p))    (.-y p)))
   (fn ^Point [^Point p] (Point. (- (.-z p)) (- (.-y p)) (- (.-x p))))
   (fn ^Point [^Point p] (Point. (- (.-z p))    (.-x p)  (- (.-y p))))])

(defn plus ^Point [^Point a ^Point b]
  (Point. (+ (.-x a) (.-x b)) (+ (.-y a) (.-y b)) (+ (.-z a) (.-z b))))

(defn minus ^Point [^Point a ^Point b]
  (Point. (- (.-x a) (.-x b)) (- (.-y a) (.-y b)) (- (.-z a) (.-z b))))

(defn can-match? [a b]
  (>= 
    (reduce-kv
      (fn [acc dist count]
        (+ acc (min ((:distances b) dist 0) count)))
      0
      (:distances a))
    66)) ;; 12 * (12 - 1) / 2

(defn match [a b]
  (when (can-match? a b)
    (let [known (:beacons a)
          beacons (:beacons b)]
      (try
        (doseq [k1 known
                k2 known
                :when (not (eq k1 k2))
                :let [dk (dist k1 k2)]
                b1 beacons
                b2 beacons
                :when (not (eq b1 b2))
                :when (= dk (dist b1 b2))
                rot rotations
                :let [d     (minus k1 (rot b1))
                      xform #(plus (rot %) d)]
                :when (eq (xform b2) k2)
                :let [beacons' (map xform beacons)]
                :when (>= (count (filter known beacons')) 12)]
            (throw (ex-info "Match"
                     {:origin (xform (Point. 0 0 0))
                      :beacons (set beacons')})))
        (catch clojure.lang.ExceptionInfo e
          (ex-data e))))))

; (let [[a b & _] (parse example)]
;   (match a b))

(defn solve [scanners]
  (let [pairs (for [a (range (count scanners))
                    b (range (count scanners))
                    :when (< a b)]
                [a b])]
    (loop [matched  #{0}
           scanners scanners
           result   {:beacons (:beacons (first scanners))
                     :origins #{(Point. 0 0 0)}}
           pairs    pairs]
      (cond+
        (empty? pairs)
        result

        :let [[a b :as pair] (first pairs)]

        (every? matched pair)
        (recur matched scanners result (remove #(= pair %) pairs))

        (not-any? matched pair)
        (recur matched scanners result (concat (next pairs) [pair]))

        (matched b)
        (recur matched scanners result (cons [b a] (next pairs)))

        :let [{:keys [beacons origin]} (match (nth scanners a) (nth scanners b))]

        (nil? beacons)
        (recur matched scanners result (concat (next pairs) [pair]))

        :else
        (recur
          (conj matched b)
          (update scanners b assoc :beacons beacons)
          (-> result
            (update :beacons into beacons)
            (update :origins conj origin))
          (next pairs))))))

(defn part1
  ([] (part1 input))
  ([input]
   (count (:beacons (solve (parse input))))))

(defn mdist [^Point a ^Point b]
  (+
    (core/abs (- (.-x a) (.-x b)))
    (core/abs (- (.-y a) (.-y b)))
    (core/abs (- (.-z a) (.-z b)))))

(defn part2
  ([] (part2 input))
  ([input]
   (let [{:keys [origins]} (solve (parse input))]
     (reduce max
       (for [a origins
             b origins]
         (mdist a b))))))

(comment
  (do
    (set! *warn-on-reflection* true)
    (require 'advent-of-code.year2021.day19 :reload))
  (part1 example)
  (part1 input)
  (solve (parse input))
  (part2 example)
  (part2 input)
  (require '[clj-async-profiler.core :as profiler])
  (profiler/profile (part1 example)))
