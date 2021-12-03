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

(defn count-depth-increases-loop
  [depths]
  (loop [depths depths
       sum 0]
  (if (> (count depths) 1)
    (let [x (first depths)
          y (fnext depths)
          new-sum (if (> y x) (inc sum) sum)]
      (recur (next depths) new-sum))
    sum)))

(defn count-depth-increases-reduce
  [depths]
  (second
   (reduce
    (fn [[remaining sum] current]
      (if (seq remaining)
        (if (> (first remaining) current)
          [(rest remaining) (inc sum)]
          [(rest remaining) sum])
        [[] sum]))
    [(rest depths) 0]
    depths)))

(comment
  (every?
   #(= 7 (% depths))
   [count-depth-increases-loop
    count-depth-increases-reduce])
  )