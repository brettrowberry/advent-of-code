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

(defn flip-bits
  [gamma]
  (map (fn [digit] (if (= digit 1) 0 1)) gamma))

(defn int-vector->number
  [gamma-or-epsilon]
  (-> gamma-or-epsilon
      string/join
      (Integer/parseInt 2)))

(defn power-consumption
  [report]
  (let [counts (report->counts report)
        gamma (counts->gamma counts)
        epsilon (flip-bits gamma)]
    (* (int-vector->number gamma)
       (int-vector->number epsilon))))

(power-consumption report)

;; Part 2
(defn get-bigger-bucket
  [comparator index xs]
  (let [{ones 1 zeros 0} (group-by (fn [x] (nth x index)) xs)]
    (if (comparator (count ones) (count zeros))
      ones
      zeros)))

(defn report->o2-or-co2
  [report comparator]
  (let [int-vectors (report->int-vectors report)
        bit-string-length (count (first int-vectors))]
    (->
   (reduce
    (fn [int-vectors index]
      (if (= 1 (count int-vectors))
        (reduced int-vectors)
        (get-bigger-bucket comparator index int-vectors)))
    int-vectors
    (range bit-string-length))
   first
   int-vector->number)))

(defn report->oxygen
  [report]
  (report->o2-or-co2 report >=))

(defn report->co2
  [report]
  (report->o2-or-co2 report <))

(defn report->life-support
  [report]
  (*
   (report->oxygen report)
   (report->co2 report)))

(report->life-support report)
