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
       [(keyword command) (Integer/parseInt magnitude)]))

(defn parse-commands
  [commands-string]
  (let [lines (string/split-lines commands-string)]
    (map parse-command lines)))

(defn next-position-part-1
  [[x y] [command magnitude]]
  (case command
    :forward [(+ x magnitude) y]
    :down    [x (+ y magnitude)]
    :up      [x (- y magnitude)]))

(defn final-position-part-1
  [commands-string]
  (let [commands (parse-commands commands-string)
        [x y] (reduce
                        (fn [position command]
                          (next-position-part-1 position command))
                        [0 0]
                        commands)]
    (* x y)))

(final-position-part-1 commands-string)

;;; Part 2
(defn next-position-part-2
  [[x y aim] [command magnitude]]
  (case command
    :forward [(+ x magnitude) (+ y (* aim magnitude)) aim]
    :down    [x y (+ aim magnitude)]
    :up      [x y (- aim magnitude)]))

(defn final-position-part-2
  [commands-string]
  (let [commands (parse-commands commands-string)
        [x y] (reduce
               (fn [position command]
                 (next-position-part-2 position command))
               [0 0 0]
               commands)]
    (* x y)))

(comment
  (= 900
     (final-position-part-2 commands-string)))
