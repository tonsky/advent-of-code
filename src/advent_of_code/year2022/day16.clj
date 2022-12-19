(ns advent-of-code.year2022.day16
  (:require
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [clojure.set :as set]
    [instaparse.core :as insta]
    [advent-of-code.priority-queue :as priority-queue]
    [advent-of-code.v2 :as v2 :refer [v2]]
    [remote-require.core :as rr]
    [clj-async-profiler.core :as profiler]
    ; [advent-of-code.year2022.gui :as gui]
    ; [io.github.humbleui.canvas :as canvas]
    ; [io.github.humbleui.core :as core]
    ; [io.github.humbleui.paint :as paint]
    ; [io.github.humbleui.ui :as ui]
    ; [io.github.humbleui.window :as window]
    )
  (:import
    [java.util.concurrent CompletableFuture]))

(rr/from "https://twitter.com/nikitonsky/status/1584634959717097472"
  :require [cond+])

(rr/from "https://twitter.com/nikitonsky/status/1584629264909225984"
  :require [zip now])

(rr/from "https://twitter.com/nikitonsky/status/1602720736875057152"
  :require [index-of])

(def parser
  (insta/parser
    "Valves    = Valve+
     Valve     = <'Valve '> Id <' has flow rate='> Rate (<'; tunnels lead to valves '> | <'; tunnel leads to valve '>) Leads <newline>
     Id        = #'[A-Z]{2}'
     Rate      = number
     Leads     = Id (<', '> Id)*
     <newline> = '\n' ' '*
     number    = #'[0-9]+'"))

(defn paths [leads]
  (let [*paths (volatile! {})]
    (doseq [[start ends] leads
            end ends]
      (vswap! *paths assoc #{start end} 1))
    (doseq [middle (keys leads)
            start  (keys leads)
            :when  (not= start middle)
            end    (keys leads)
            :when  (not= end start)
            :when  (not= end middle)
            :let [start-end    (@*paths #{start end} 10000)
                  start-middle (@*paths #{start middle} 10000)
                  middle-end   (@*paths #{middle end} 10000)]]
      (if (< (+ start-middle middle-end) start-end)
        (vswap! *paths assoc #{start end} (+ start-middle middle-end))))
    (into {}
      (for [start  (keys leads)]
        [start (into {}
                 (for [end (keys leads)
                       :when (not= start end)]
                   [end (@*paths #{start end})]))]))))

(def transform
  {:number parse-long
   :Id     identity
   :Rate   identity
   :Leads  (fn [& args] (set args))
   :Valve  (fn [id rate ids]
             [id rate ids])
   :Valves (fn [& valves]
             (let [rates (into {} (map (juxt first second) valves))
                   leads (into {} (map (juxt first #(nth % 2)) valves))]
               [{:rates rates
                 :paths (paths leads)}
                {:open     []
                 :closed   (->> valves
                             (filter #(pos? (second %)))
                             (map first)
                             set)
                 :released 0
                 :time     0
                 :pos      "AA"}]))})

(def sample1
  "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
")

(def data
  (slurp (io/file "inputs/year2022/day16")))

(defn parse [data]
  (insta/transform transform
    (parser
      data)))

(comment
  (parse sample1)
  (parse data))

(defn best-estimate [{:keys [days rates paths]} {:keys [open closed released time pos] :as q}]
  (+ released
    (* (- days time) (reduce + 0 (map rates open)))
    (reduce + 0 (map #(* (rates %1) %2)
                  closed
                  (range (- days time) -1 -1)))))  

(defn step [{:keys [days rates paths *res] :as p} queue]
  (let [{:keys [open closed released time pos] :as q} (priority-queue/peek queue)
        best (best-estimate p q)]
    (reduce
      priority-queue/conj
      (priority-queue/pop queue)
      (cond
        (>= time days)
        (do
          (vswap! *res max released)
          [])
        
        (< best @*res)
        []
        
        (empty? closed)
        (do
          (vswap! *res max best)
          [])
        
        :else
        (let [extra (reduce + 0 (map rates open))]
          (concat
            (when (closed pos)
              [(-> q
                 (update :time inc)
                 (update :released + extra)
                 (update :open conj pos)
                 (update :closed disj pos))])
            (for [pos'   closed
                  :when  (not= pos' pos)
                  :let   [dist (min
                                 (-> paths (get pos) (get pos'))
                                 (- days time))]]
              (-> q
                (assoc :pos pos')
                (update :time + dist)
                (update :released + (* extra dist))))))))))

(defn part1
  ([data]
   (let [[p q] (parse data)
         *res  (volatile! 0)
         p     (assoc p
                 :*res *res
                 :days 30)]
     (part1 p q)
     @*res))
  ([p q]
   (part1 p q 0))
  ([p q at-least]
   (when (> (best-estimate p q) at-least)
     (loop [queue (priority-queue/priority-queue-by #(- (best-estimate p %)) q)]
       (when (not-empty queue)
         (recur (step p queue)))))))

(comment
  (part1 sample1)
  (part1 data)
  @*res)

(defn splits [s]
  (let [s    (vec s)
        len  (count s)]
    (into #{}
      (for [i (range 0 (math/pow 2 len))]
        (set
          (reduce
            (fn [[a b] j]
              (if (bit-test i j)
                [(conj a (nth s j)) b]
                [a (conj b (nth s j))]))
            [[] []] (range 0 len)))))))

(defn part2
  ([data]
   (let [[p q]   (parse data)
         *done   (atom 0)
         *res    (atom 0)
         p       (assoc p
                   :days 26)
         splits  (splits (:closed (second (parse data))))
         t0      (now)
         futures (mapv
                   (fn [pair]
                     (CompletableFuture/runAsync
                       (fn []
                         (part2 p q *res pair)
                         (swap! *done inc)
                         (println "time:" (- (now) t0) "ms, best:" @*res ", progress:" (-> (/ @*done (count splits)) (* 100) (int)) "%"))))
                   splits)
         future  (CompletableFuture/allOf (into-array CompletableFuture futures))]
     (.get future)
     @*res))
  ([p q *res pair]
   (let [a  (first pair)
         b  (second pair)
         *a (volatile! 0)
         *b (volatile! 0)]
     (part1 (assoc p :*res *a) (assoc q :closed (set a)))
     (part1 (assoc p :*res *b) (assoc q :closed (set b)) (- @*res @*a))
     (when (> (+ @*a @*b) @*res)
       (reset! *res (+ @*a @*b))))))

(comment
  (splits (:closed (second (parse sample1))))  
  (part2 sample1)
  (part2 data)
  @*res)
