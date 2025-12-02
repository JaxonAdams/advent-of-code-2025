(ns aoc.problems.day02
  (:require [clojure.string :as string]))

(defn- parse-range [range-str]
  (let [range-str-split (string/split range-str #"-" 2)
        [start stop] (map Integer/parseInt range-str-split)]
    (range start (inc stop))))

(defn- is-invalid-id? [num]
  (if (-> num str count (mod 2) zero? not)
    false
    (let [n-str (str num)
          first-half (apply str (take (/ (count n-str) 2) n-str))
          second-half (apply str (take-last (/ (count n-str) 2) n-str))]
      (= first-half second-half))))

(defn- get-invalid-ids
  "Get all 'invalid' IDs in a given range [START, STOP] (formated START-STOP).
   An ID is invalid if it consists solely of some sequence of digits repeated twice;
   e.g. 55 (five twice), 6464 (64 twice), and 123123 (123 twice)."
  [range-str]
  (let [range-to-check (parse-range range-str)]
    (filter is-invalid-id? range-to-check)))

;; Examples from AOC
(comment
  (get-invalid-ids "11-22"))
