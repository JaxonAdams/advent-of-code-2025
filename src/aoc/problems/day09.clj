(ns aoc.problems.day09
  (:require [utils.files :refer [read-file-lines]]
            [clojure.string :refer [split]]))

(defn- rect-area [[x1 y1] [x2 y2]]
  (abs (* (inc (- x2 x1)) (inc (- y2 y1)))))

(defn- possible-rects-red-corners [red-tile-locations]
  (for [a red-tile-locations
        b red-tile-locations
        :when (not= a b)]
    {:corners [a b] :area (rect-area a b)}))

(defn largest-rect-red-corners [red-tile-locations]
  (->> red-tile-locations
       possible-rects-red-corners
       (sort-by :area >)
       first
       :area))

(defn- point-on-segment? [[px py] [x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        vx  (- px x1)
        vy  (- py y1)
        cross (- (* dx vy) (* dy vx))]
    (and (== cross 0.0)
         (<= (min x1 x2) px (max x1 x2))
         (<= (min y1 y2) py (max y1 y2)))))

(defn- point-in-polygon? [[px py] polygon]
  (let [n (count polygon)]
    (loop [i 0
           j (dec n)
           inside false]
      (if (= i n)
        inside
        (let [[x1 y1] (nth polygon i)
              [x2 y2] (nth polygon j)
              on-edge (or (point-on-segment? [px py] [x1 y1] [x2 y2])
                          (point-on-segment? [px py] [x2 y2] [x1 y1]))
              yi (> y1 py)
              yj (> y2 py)]
          (cond
            on-edge true
            (not= yi yj)
            (let [x-intersect (+ x1 (* (- x2 x1) (/ (- py y1) (- y2 y1))))]
              (if (< px x-intersect)
                (recur (inc i) i (not inside))
                (recur (inc i) i inside)))
            :else
            (recur (inc i) i inside)))))))

(defn- memoized-point-in-polygon
  [polygon]
  (let [f (fn [point] (point-in-polygon? point polygon))]
    (memoize f)))

(defn- rect-points [[[x1 y1] [x2 y2]]]
  (let [corners [[x1 y1] [x1 y2] [x2 y1] [x2 y2]]
        edge-midpoints [[(/ (+ x1 x2) 2) y1]
                        [(/ (+ x1 x2) 2) y2]
                        [x1 (/ (+ y1 y2) 2)]
                        [x2 (/ (+ y1 y2) 2)]]
        center [[(/ (+ x1 x2) 2) (/ (+ y1 y2) 2)]]]
    (concat corners edge-midpoints center)))

(defn- rect-in-polygon? [rect inside?]
  (every? inside? (rect-points rect)))

(defn- largest-rect-in-polygon [red-tile-locations]
  (let [inside? (memoized-point-in-polygon red-tile-locations)]
    (->> red-tile-locations
         possible-rects-red-corners
         (filter #(-> % :corners (rect-in-polygon? inside?)))
         (sort-by :area >)
         first
         :area)))

;; ----------------------------------------------------------------------------
;; PART ONE

(comment
  (def example-red-tile-locations [[7 1]
                                   [11 1]
                                   [11 7]
                                   [9 7]
                                   [9 5]
                                   [2 5]
                                   [2 3]
                                   [7 3]])

  ;; 24
  (rect-area [2 5] [9 7])
  ;; 35
  (rect-area [7 1] [11 7])

  (possible-rects-red-corners example-red-tile-locations)

  ;; 50
  (largest-rect-red-corners example-red-tile-locations))

;; For the solution...
(comment
  (->> "input/day09/input.txt"
       read-file-lines
       (map #(split % #","))
       (map #(map Long/parseLong %))
       largest-rect-red-corners))

;; ----------------------------------------------------------------------------
;; PART TWO

(comment

  (nth example-red-tile-locations 1)

  (def simple-square [[0 0]
                      [10 0]
                      [10 10]
                      [0 10]])

  ;; 24
  (largest-rect-in-polygon example-red-tile-locations))

;; For the solution...
;; NOT 4604012490
;; NOT 4603079700
(comment
  (->> "input/day09/input.txt"
       read-file-lines
       (map #(split % #","))
       (map #(map Long/parseLong %))
       largest-rect-in-polygon))
