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

(defn total-fresh [ingredient-ids fresh-ranges]
  (->> ingredient-ids
       (map Long/parseLong)
       (map #(is-fresh? % fresh-ranges))
       (map bool->int)
       (reduce +)))

;; ----------------------------------------------------------------------------
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
  (total-fresh ["1" "5" "8" "11" "17" "32"] fresh-ranges)

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
(comment)
