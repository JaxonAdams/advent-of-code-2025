(ns aoc.problems.day09)

(defn- rect-area [[x1 y1] [x2 y2]]
  (abs (* (inc (- x2 x1)) (inc (- y2 y1)))))

(defn- possible-rects-red-corners [red-tile-locations]
  (for [a red-tile-locations
        b red-tile-locations
        :when (not= a b)]
    {:corners [a b] :area (rect-area a b)}))

(defn- largest-rect-red-corners [red-tile-locations]
  (->> red-tile-locations
       possible-rects-red-corners
       (sort-by :area >)
       first
       :area))

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
