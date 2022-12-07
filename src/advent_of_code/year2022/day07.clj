(ns advent-of-code.year2022.day07
  (:require
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]
    [clojure.set :as set]
    [remote-require.core :as rr]))

(rr/from "https://twitter.com/nikitonsky/status/1584634959717097472"
  :require [cond+])

(rr/from "https://twitter.com/nikitonsky/status/1584629264909225984"
  :require [zip])

(def sample1
  "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(def data
  (slurp (io/file "inputs/year2022/day07")))

(defn parse [data]
  (str/split-lines data))

(defn dir->path [dir]
  (str "/" (str/join "/" dir)))

(defn process [data]
  (loop [lines (parse data)
         sizes {}
         dir   []]
    (cond+
      (empty? lines)
      sizes
      
      :let [[line & lines'] lines]

      (= "$ cd /" line)
      (recur lines' sizes [])
      
      (= "$ cd .." line)
      (recur lines' sizes (pop dir))
      
      (re-matches #"\$ cd \w+" line)
      (let [[_ d] (re-matches #"\$ cd (\w+)" line)]
        (recur lines' sizes (conj dir d)))
      
      (re-matches #"\d+ [a-z.]+" line)
      (let [[_ size name] (re-matches #"(\d+) ([a-z.]+)" line)
            size   (parse-long size)
            sizes' (reduce
                     (fn [sizes n]
                       (update sizes (dir->path (drop-last n dir)) #(+ (or % 0) size)))
                     sizes
                     (range (inc (count dir))))]
        (recur lines' sizes' dir))
      
      :eles
      (recur lines' sizes dir))))

(defn part1 [data]
  (->> data
    process
    vals
    (filter #(<= % 100000))
    (reduce + 0)))

(comment
  (part1 sample1)
  (part1 data))

(defn part2 [data]
  (let [sizes    (process data)
        taken    (sizes "/")
        free     (- 70000000 taken)
        required (- 30000000 free)]
    (->> sizes
      vals
      sort
      (drop-while #(< % required))
      first)))

(comment
  (part2 sample1)
  (part2 data))
