(ns one)

(def depths
  [199
   200
   208
   210
   200
   207
   240
   269
   260
   263])

(defn count-depth-increases
  [depths]
  (loop [depths depths
       sum 0]
  (if (> (count depths) 1)
    (let [x (first depths)
          y (fnext depths)
          new-sum (if (> y x) (inc sum) sum)]
      (recur (next depths) new-sum))
    sum)))

(count-depth-increases depths)
