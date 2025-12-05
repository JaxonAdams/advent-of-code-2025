(ns aoc.problems.day05
  (:require [clojure.string :as string]
            [utils.files :refer [read-file-lines]]))

(defn- bool->int [b]
  (if b 1 0))

(defn split-at-value [value coll]
  (let [[left right] (split-with (complement #{value}) coll)]
    [left (drop-while #{value} right)]))

(defn- parse-input [lines]
  (let [[fresh-ranges ids] (split-at-value "" lines)]
    {:fresh-ranges fresh-ranges
     :ingredient-ids ids}))

(defn- parse-range [fresh-range]
  (let [[start stop] (string/split fresh-range #"-")]
    [(Long/parseLong start) (Long/parseLong stop)]))

(defn- parse-ranges [range-strings]
  (->> range-strings
       (mapv parse-range)))

(defn merge-ingredient-ranges
  [ingredient-ranges]
  (let [sorted-ranges (sort-by first ingredient-ranges)]
    (reduce
     (fn [merged-ranges [range-start range-end]]
       (if (empty? merged-ranges)
          ;; first range
         [[range-start range-end]]
         (let [[merged-start merged-end] (peek merged-ranges)]
           (if (<= range-start (inc merged-end))
              ;; ranges overlap or touch → extend previous range
             (conj (pop merged-ranges)
                   [merged-start (max merged-end range-end)])
              ;; ranges separate → add new independent range
             (conj merged-ranges [range-start range-end])))))
     []
     sorted-ranges)))

(defn- ingredient-is-in-range [ingredient-id fresh-range]
  (let [[start stop] (-> fresh-range
                         (string/split #"-")
                         (->> (map Long/parseLong)))]
    (<= start ingredient-id stop)))

(defn- is-fresh? [ingredient-id fresh-ranges]
  (->> fresh-ranges
       (filter (partial ingredient-is-in-range ingredient-id))
       count
       pos?))

(defn total-fresh-from-available [ingredient-ids fresh-ranges]
  (->> ingredient-ids
       (map Long/parseLong)
       (map #(is-fresh? % fresh-ranges))
       (map bool->int)
       (reduce +)))

(defn total-fresh
  [fresh-range-strings]
  (let [ingredient-ranges (parse-ranges fresh-range-strings)
        merged-ranges (merge-ingredient-ranges ingredient-ranges)]
    (reduce (fn [count-so-far [range-start range-end]]
              (+ count-so-far (inc (- range-end range-start))))
            0
            merged-ranges)))

;; ----------------------------------------------------------------------------
;; PART ONE

(comment
  (def fresh-ranges ["3-5"
                     "10-14"
                     "16-20"
                     "12-18"])

  ;; false
  (is-fresh? 1 fresh-ranges)
  ;; true
  (is-fresh? 5 fresh-ranges)
  ;; false
  (is-fresh? 8 fresh-ranges)
  ;; true
  (is-fresh? 11 fresh-ranges)
  ;; false
  (is-fresh? 32 fresh-ranges)

  ;; 3
  (total-fresh-from-available ["1" "5" "8" "11" "17" "32"] fresh-ranges)

  (parse-input ["3-5"
                "10-14"
                "16-20"
                "12-18"
                ""
                "1"
                "5"
                "8"
                "11"
                "17"
                "32"]))

;; For the solution...
(comment
  (-> "input/day05/input.txt"
      read-file-lines
      parse-input
      ((fn [x] (total-fresh-from-available (:ingredient-ids x) (:fresh-ranges x))))))

;; ----------------------------------------------------------------------------
;; PART TWO

(comment
  (def fresh-ranges ["3-5"
                     "10-14"
                     "16-20"
                     "12-18"])

  ;; 14
  (total-fresh fresh-ranges))

;; For the solution...
(comment
  (-> "input/day05/input.txt"
      read-file-lines
      parse-input
      ((fn [x] (total-fresh (:fresh-ranges x))))))
