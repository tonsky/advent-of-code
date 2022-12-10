(ns advent-of-code.v2
  (:require
    [clojure.math :as math]))

(defrecord V2 [x y])

(defn v2 [x y]
  (V2. x y))

(def zero
  (v2 0 0))

(defn add [^V2 a ^V2 b]
  (v2
    (+ (.-x a) (.-x b))
    (+ (.-y a) (.-y b))))

(defn left [a]
  (add a (v2 -1 0)))

(defn right [a]
  (add a (v2 1 0)))

(defn up [a]
  (add a (v2 0 -1)))

(defn down [a]
  (add a (v2 0 1)))

(defn neighbours4 [a]
  [(left a)
   (right a)
   (up a)
   (down a)])

(defn neighbours8 [a]
  [(left a)
   (right a)
   (up a)
   (down a)
   (add a (v2 1 1))
   (add a (v2 1 -1))
   (add a (v2 -1 1))
   (add a (v2 -1 -1))])

(defn dist ^long [a b]
  (+ (long (abs (- (:x a) (:x b))))
    (long (abs (- (:y a) (:y b))))))