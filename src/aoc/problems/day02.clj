(ns aoc.problems.day02
  (:require [clojure.string :as string]))

(defn- str-partition [n s]
  (->> s
       (partition n n "X")
       (map (partial apply str))))

(defn parse-range [range-str]
  (let [range-str-split (string/split range-str #"-" 2)
        [start stop] (map Long/parseLong range-str-split)]
    (range start (inc stop))))

(defn is-twice-repeated-sequence? [num]
  (if (-> num str count (mod 2) zero? not)
    false
    (let [n-str (str num)
          first-half (apply str (take (/ (count n-str) 2) n-str))
          second-half (apply str (take-last (/ (count n-str) 2) n-str))]
      (= first-half second-half))))

(defn is-repeated-sequence? [num]
  (let [n-str (str num)
        max-times-to-partition (count n-str)
        partitions (map #(str-partition % n-str) (range 1 max-times-to-partition))
        with-repeats (filter (partial apply =) partitions)]
    (boolean (seq with-repeats))))

(defn get-invalid-ids
  [validator range-str]
  (let [range-to-check (parse-range range-str)]
    (filter validator range-to-check)))

(defn get-invalid-id-sum
  [validator range-strs]
  (->> range-strs
       (map (partial get-invalid-ids validator))
       flatten
       (reduce +)))

;; ----------------------------------------------------------------------------
;; PROBLEM 1
(comment
  (let [ranges (-> "input/day02/day02-input.txt"
                   slurp
                   (string/split #",")
                   (->> (map string/trim)))]
    (get-invalid-id-sum is-twice-repeated-sequence? ranges)))

;; ----------------------------------------------------------------------------
;; PROBLEM 2
(comment
  (let [ranges (-> "input/day02/day02-input.txt"
                   slurp
                   (string/split #",")
                   (->> (map string/trim)))]
    (get-invalid-id-sum is-repeated-sequence? ranges)))
