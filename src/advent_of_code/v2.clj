(ns advent-of-code.v2
  (:require
    [clojure.math :as math]))

(defrecord V2 [x y]
  java.lang.Object
  (toString [_]
    (str "(" x "," y ")"))

  clojure.lang.Indexed
  (nth [this i]
    (nth this i nil))
  (nth [_ i not-found]
    (case i 0 x 1 y not-found))
  
  java.lang.Comparable
  (compareTo [a b]
    (cond
      (identical? a b)  0
      (< (:x a) (:x b)) -1
      (> (:x a) (:x b)) 1
      (< (:y a) (:y b)) -1
      (> (:y a) (:y b)) 1
      :else             0)))

(def v2 ->V2)

(defmethod print-method V2 [c ^java.io.Writer w]
  (.write w "(")
  (.write w (str (:x c)))
  (.write w ",")
  (.write w (str (:y c)))
  (.write w ")"))

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