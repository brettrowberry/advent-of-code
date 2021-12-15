(ns five)

(def line-segment-string
  "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(defn line-segment->coordinates
  [s]
  (let [[x1 y1 x2 y2] (->> (re-seq #"\d+" s)
                           (map #(Integer/parseInt %)))]
    [[x1 y1] [x2 y2]]))

(defn vertical?
  [[x1 _] [x2 _]]
  (= x1 x2))

(defn horizontal?
  [[_ y1] [_ y2]]
  (= y1 y2))

(defn vertical-points
  [[x1 y1] [_ y2]]
  (mapv (partial vector x1) (range (min y1 y2) (inc (max y1 y2)))))

(defn horizontal-points
  [[x1 y1] [x2 _]]
  (mapv (fn [x] (vector x y1)) (range (min x1 x2) (inc (max x1 x2)))))

(defn make-range
  [p1 p2]
  (range (min p1 p2) (inc (max p1 p2))))

(defn diagonal-points
  [[x1 y1] [x2 y2]]
  (let [x-range (cond-> (make-range x1 x2)
                  (> x1 x2) reverse) 
        y-range (cond-> (make-range y1 y2)
                  (> y1 y2) reverse)]
    (mapv #(vector %1 %2) x-range y-range)))

(defn endpoints->points
  [[[x1 y1] [x2 y2]]]
  (cond
    (horizontal? [x1 y1] [x2 y2]) (horizontal-points [x1 y1] [x2 y2])
    (vertical? [x1 y1] [x2 y2]) (vertical-points [x1 y1] [x2 y2])
    :else (diagonal-points [x1 y1] [x2 y2])))

(defn line-segment-string->endpoints
  [s]
  (map line-segment->coordinates (clojure.string/split-lines s)))

;;; what if I told you we don't need a matrix?
(->> line-segment-string
     line-segment-string->endpoints
     (mapcat endpoints->points)
     frequencies
     vals
     (filter #(>= % 2))
     count)