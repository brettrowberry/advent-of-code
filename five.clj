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

;; .......1..
;; ..1....1..
;; ..1....1..
;; .......1..
;; .112111211
;; ..........
;; ..........
;; ..........
;; ..........
;; 222111....

;; determine the number of points where at least two lines overlap.
;; In the above example, this is anywhere in the diagram with a 2 or larger - a total of 5 points.
;; Consider only horizontal and vertical lines. At how many points do at least two lines overlap?

(defn empty-matrix [size] (vec (repeat size (vec (repeat size 0)))))

(defn line-segment->coordinates
  [s]
  (let [[x1 y1 x2 y2] (->> (re-seq #"\d+" s)
                           (map #(Integer/parseInt %)))]
    [[x1 y1] [x2 y2]]))

(defn point->matrix
  [m [x y]]
  (update-in m [x y] inc))

(point->matrix (empty-matrix 10) [1 1])

(defn points->matrix
  [m points]
  (reduce
   point->matrix
   m
   points))

(points->matrix
 (empty-matrix 10)
 [[0 1] [1 1]])

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

(defn diagonal-points
  [[x1 y1] [x2 y2]]
  (let [x-range (if (> x1 x2)
                  (reverse (range (min x1 x2) (inc (max x1 x2))))
                  (range (min x1 x2) (inc (max x1 x2))))
        y-range (if (> y1 y2)
                  (reverse (range (min y1 y2) (inc (max y1 y2))))
                  (range (min y1 y2) (inc (max y1 y2))))]
    #_(partition-all 2 (interleave x-range y-range))
    (mapv #(vector %1 %2) x-range y-range)))

(diagonal-points [1 1] [3 3]) ;; [1 1] [2 2] [3 3]
(diagonal-points [9 7] [7 9]) ;; [9 7] [8 8] [7 9]

(mapv #(vector %1 %2) [9 8 7] [1 2 3])

(defn endpoints->points
  [[[x1 y1] [x2 y2]]]
  (cond
    (horizontal? [x1 y1] [x2 y2]) (horizontal-points [x1 y1] [x2 y2])
    (vertical? [x1 y1] [x2 y2]) (vertical-points [x1 y1] [x2 y2])
    :else [] #_(diagonal-points [x1 y1] [x2 y2])))

(endpoints->points [[6 4] [2 0]])

(vertical-points [0 6] [0 3])
(endpoints->points [[0 6] [0 3]])

(horizontal-points [6 0] [3 0])
(endpoints->points [[6 0] [3 0]])

(endpoints->points [[3 0] [6 0]])
(endpoints->points [[6 0] [3 0]])

(endpoints->points [[1 1] [3 3]])
(endpoints->points [[9 7] [7 9]])

(defn line-segment-string->endpoints
  [s]
  (map line-segment->coordinates (clojure.string/split-lines s)))

(defn final-matrix
  [size line-segment-string]
  (let [starting-matrix (empty-matrix size)]
    (reduce
     (fn [m endpoints]
       (points->matrix m (endpoints->points endpoints)))
     starting-matrix
     (line-segment-string->endpoints line-segment-string))))

(defn part-one
  [line-segments]
  (let [threshold 2]
    (->>
     (final-matrix 1000 line-segments)
     flatten
     (filter #(>= % threshold))
     count)))

(assert (= 5 (part-one line-segment-string)) "should be 5")

;;; what if I told you we don't need a matrix?
(->> line-segment-string
     line-segment-string->endpoints
     (mapcat endpoints->points)
     frequencies
     vals
     (filter #(>= % 2))
     count)