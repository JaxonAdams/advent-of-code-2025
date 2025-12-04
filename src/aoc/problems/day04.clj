(ns aoc.problems.day04
  (:require [clojure.string :as string]))

(defn get-neighbors-in-bounds [[x y] diagram-rows]
  (let [rows (count diagram-rows)
        cols (count (first diagram-rows))]
    (for [dx [-1 0 1]
          dy [-1 0 1]
          :let [nx (+ x dx)
                ny (+ y dy)]
          :when (and
                 (not (and (zero? dx) (zero? dy)))
                 (<= 0 nx (dec rows))
                 (<= 0 ny (dec cols)))]
      [nx ny])))

(defn get-character-from-pos [diagram-rows [x y]]
  (-> diagram-rows (nth x) (nth y)))

(defn- is-roll-accessible? [pos diagram-rows]
  (if (-> diagram-rows (get-character-from-pos pos) (not= "@"))
    false
    (let [adjacent-positions (get-neighbors-in-bounds pos diagram-rows)
          adjacent-chars (map (partial get-character-from-pos diagram-rows) adjacent-positions)
          counted (reduce (fn [counts item]
                            (assoc counts item (inc (get counts item 0))))
                          {}
                          adjacent-chars)]
      (-> counted (get "@") (< 4)))))

(defn get-num-of-accessible-rolls [diagram-rows]
  (let [diagram-matrix (map #(string/split % #"") diagram-rows)
        accessible-matrix (map
                           (fn [row-idx]
                             (map
                              (fn [col-idx]
                                (is-roll-accessible? [row-idx col-idx] diagram-matrix))
                              (range 0 (count (nth diagram-matrix row-idx)))))
                           (range 0 (count diagram-rows)))]
    (->> accessible-matrix
         flatten
         (filter identity)
         count)))

;; ----------------------------------------------------------------------------
;; PART ONE

;; '@' = roll of paper
(def example-diagram ["..@@.@@@@."
                      "@@@.@.@.@@"
                      "@@@@@.@.@@"
                      "@.@@@@..@."
                      "@@.@@@@.@@"
                      ".@@@@@@@.@"
                      ".@.@.@.@@@"
                      "@.@@@.@@@@"
                      ".@@@@@@@@."
                      "@.@.@@@.@."]) ;; 13 can be accessed

(def example-diagram-matrix (map #(string/split % #"") example-diagram))

(comment
  ;; false
  (is-roll-accessible? [0 7] example-diagram-matrix)
  ;; true
  (is-roll-accessible? [2 6] example-diagram-matrix)
  ;; true
  (is-roll-accessible? [9 0] example-diagram-matrix)
  ;; true
  (is-roll-accessible? [4 9] example-diagram-matrix)
  ;; false
  (is-roll-accessible? [5 9] example-diagram-matrix))

(comment
  ;; 13
  (get-num-of-accessible-rolls example-diagram))

