(ns two
  (:require [clojure.string :as string]))

(def commands-string
  "forward 5
down 5
forward 8
up 3
down 8
forward 2")

(defn parse-command
  [s]
  (let [[command magnitude] (re-seq #"\w+" s)]
       [command (Integer/parseInt magnitude)]))

(defn parse-commands
  [commands-string]
  (let [lines (string/split-lines commands-string)]
    (map parse-command lines)))

(defn next-position
  [[x y] [command magnitude]]
  (case command
    "forward" [(+ x magnitude) y]
    "down"    [x (+ y magnitude)]
    "up"      [x (- y magnitude)]))

(defn final-position
  [commands-string]
  (let [commands (parse-commands commands-string)
        [x y] (reduce
                        (fn [position command]
                          (next-position position command))
                        [0 0]
                        commands)]
    (* x y)))

(final-position commands-string)