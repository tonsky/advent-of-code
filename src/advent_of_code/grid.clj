(ns advent-of-code.grid
  [:refer-clojure :exclude [get contains?]])

(defrecord Grid [arr w h])

(defn make [size]
  (let [{w :x, h :y} size]
    (Grid. (make-array Object (* w h)) w h)))

(defn fill [m v]
  (let [^objects arr (:arr m)
        len (count arr)]
    (doseq [i (range 0 len)]
      (aset arr i v))
    m))

(defn idx ^long [m p]
  (+ (:x p) (* (:y p) (:w m))))

(defn get [m p]
  (aget ^objects (:arr m) (idx m p)))

(defn set! [m p val]
  (aset ^objects (:arr m) (idx m p) val)
  m)

(defn contains? [m p]
  (let [{:keys [x y]} p
        {:keys [w h]} m]
    (and
      (<= 0 x)
      (< x w)
      (<= 0 y)
      (< y h))))