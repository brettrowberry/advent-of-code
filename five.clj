(ns five)

(def line-segments
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
  (let [[x1 y1 x2 y2] (->> (re-seq #"\d" s)
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

(defn coordinates->points
  [[x1 y1] [x2 y2]]
  (cond
    (horizontal? [x1 y1] [x2 y2]) (horizontal-points [x1 y1] [x2 y2])
    (vertical? [x1 y1] [x2 y2]) (vertical-points [x1 y1] [x2 y2])
    :diagonal []))

(coordinates->points [6 4] [2 0])

(vertical-points [0 6] [0 3])
(coordinates->points [0 6] [0 3])

(horizontal-points [6 0] [3 0])
(coordinates->points [6 0] [3 0])

(coordinates->points [3 0] [6 0])
(coordinates->points [6 0] [3 0])

(defn part-one
  [size line-segments]
  (let [starting-matrix (empty-matrix size)]
    (reduce
     (fn [m [pt1 pt2]]
       (prn pt1)
       (points->matrix m (coordinates->points pt1 pt2)))
     starting-matrix
     (map line-segment->coordinates (clojure.string/split-lines line-segments)))))

(part-one 10 line-segments)
