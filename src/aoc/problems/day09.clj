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

(defn- full-rect-area [x1 y1 x2 y2]
  (* (inc (abs (- x2 x1))) (inc (abs (- y2 y1)))))

(defn- manhattan-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn- intersects? [edges min-x min-y max-x max-y]
  (some (fn [{:keys [x1 y1 x2 y2]}]
          (let [[i-min-x i-max-x] (sort [x1 x2])
                [i-min-y i-max-y] (sort [y1 y2])]
            (and (< min-x i-max-x)
                 (> max-x i-min-x)
                 (< min-y i-max-y)
                 (> max-y i-min-y))))
        edges))

(defn- largest-rect-in-polygon [polygon]
  (let [[init-x init-y] (first polygon)
        [last-x last-y] (last polygon)
        edges (conj (mapv (fn [[from to]]
                            {:x1 (first from) :y1 (second from)
                             :x2 (first to) :y2 (second to)})
                          (partition 2 1 polygon))
                    {:x1 init-x :y1 init-y :x2 last-x :y2 last-y})
        red-tiles (vec (mapcat (fn [[from to]] [from to])
                               (partition 2 1 polygon)))]

    (reduce
     (fn [result [f-idx t-idx]]
       (let [from-tile (nth red-tiles f-idx)
             to-tile (nth red-tiles t-idx)
             [min-x max-x] (sort [(first from-tile) (first to-tile)])
             [min-y max-y] (sort [(second from-tile) (second to-tile)])
             m-dist (manhattan-distance from-tile to-tile)]
         (if (> (* m-dist m-dist) result)
           (if-not (intersects? edges min-x min-y max-x max-y)
             (let [area (full-rect-area (first from-tile) (second from-tile)
                                        (first to-tile) (second to-tile))]
               (max result area))
             result)
           result)))
     0
     (for [f-idx (range (dec (count red-tiles)))
           t-idx (range f-idx (count red-tiles))]
       [f-idx t-idx]))))

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
(comment
  (->> "input/day09/input.txt"
       read-file-lines
       (map #(split % #","))
       (map #(map Long/parseLong %))
       largest-rect-in-polygon))
