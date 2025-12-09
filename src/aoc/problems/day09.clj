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

(defn- midpoint [[r1 c1] [r2 c2]]
  [(/ (+ r1 r2) 2.0)
   (/ (+ c1 c2) 2.0)])

(defn rect-in-polygon? [[p1 p2] polygon]
  (let [[px py] (midpoint p1 p2)
        n (count polygon)]
    (loop [i 0
           j (dec n)
           inside false]
      (if (= i n)
        inside
        (let [[x1 y1] (nth polygon i)
              [x2 y2] (nth polygon j)
              intersect (and (not= y1 y2)
                             (<= (min y1 y2) py (max y1 y2))
                             (> px (+ x1 (* (- x2 x1) (/ (- py y1) (- y2 y1))))))]
          (recur (inc i) i (if intersect (not inside) inside)))))))

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

  (rect-in-polygon? [[0 0] [1 10]] simple-square)

  (-> example-red-tile-locations
      possible-rects-red-corners
      first
      :corners
      (rect-in-polygon? example-red-tile-locations)))
