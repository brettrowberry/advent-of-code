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

;; To avoid the most dangerous areas, you need to determine the number of points where at least two lines overlap.
;; In the above example, this is anywhere in the diagram with a 2 or larger - a total of 5 points.
;; Consider only horizontal and vertical lines. At how many points do at least two lines overlap?

(def size 10)
(def empty-matrix (vec (repeat size (vec (repeat size 0)))))

(update-in [[0 0]
            [0 0]] [0 0] inc)

(defn line-segment->coordinates
  [s]
  (let [[x1 y1 x2 y2] (->> (re-seq #"\d" s)
                           (map #(Integer/parseInt %)))]
    [[x1 y1] [x2 y2]]))

(defn point->matrix
  [m [x y]]
  (update-in m [x y] inc))

(point->matrix empty-matrix [1 1])

(defn points->matrix
  [m points]
  (reduce
   point->matrix
   m
   points))

(points->matrix
 empty-matrix
 [[0 1] [1 1]])

(defn coordinates->points
  [[x1 y1] [x2 y2]]
  (if (= x1 x2)
    (mapv (partial vector x1) (range (min y1 y2) (inc (max y1 y2))))
    (mapv (fn [x] (vector x y1)) (range (min x1 x2) (inc (max x1 x2))))))

(coordinates->points [0 3] [0 6])
(coordinates->points [0 6] [0 3])
(coordinates->points [3 0] [6 0])
(coordinates->points [6 0] [3 0])
