(ns three
  (:require [clojure.string :as string]))

(def report
"00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

(defn entry->int-vector
  [entry]
  (map #(Integer/parseInt (str %) 10) entry))

(defn report->int-vectors
  [report]
  (map entry->int-vector (string/split-lines report)))

(defn report->counts
  "Returns
   [x, [a b c d e]]"
  [report]
  (let [int-vectors (report->int-vectors report)]
    [(count int-vectors) (reduce
                          (fn [counts n]
                            (map + counts (entry->int-vector n)))
                          int-vectors)]))

(defn counts->gamma
  [[total counts]]
  (let [threshold (/ total 2)]
    (map (fn [digit] (if (>= digit threshold) 1 0)) counts)))

(defn gamma->epsilon
  [gamma]
  (map (fn [digit] (if (= digit 1) 0 1)) gamma))

(defn gamma-or-epsilon->number
  [gamma-or-epsilon]
  (-> gamma-or-epsilon
      string/join
      (Integer/parseInt 2)))

(defn power-consumption
  [report]
  (let [counts (report->counts report)
        gamma (counts->gamma counts)
        epsilon (gamma->epsilon gamma)]
    (* (gamma-or-epsilon->number gamma)
       (gamma-or-epsilon->number epsilon))))

(power-consumption report)

;; Part 2
