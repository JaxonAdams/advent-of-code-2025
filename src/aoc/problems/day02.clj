(ns aoc.problems.day02
  (:require [clojure.string :as string]))

(defn- parse-range [range-str]
  (let [range-str-split (string/split range-str #"-" 2)
        [start stop] (map Long/parseLong range-str-split)]
    (range start (inc stop))))

(defn- str-partition [n s]
  (->> s
       (partition n n "X")
       (map (partial apply str))))

(defn- is-invalid-id? [num]
  (let [n-str (str num)
        max-times-to-partition (count n-str)
        partitions (map #(str-partition % n-str) (range 1 max-times-to-partition))
        with-repeats (filter (partial apply =) partitions)]
    (boolean (seq with-repeats))))

(defn- get-invalid-ids
  "Get all 'invalid' IDs in a given range [START, STOP] (formated START-STOP).
   An ID is invalid if it consists solely of some sequence of digits repeated twice;
   e.g. 55 (five twice), 6464 (64 twice), and 123123 (123 twice)."
  [range-str]
  (let [range-to-check (parse-range range-str)]
    (filter is-invalid-id? range-to-check)))

(defn get-invalid-id-sum
  [range-strs]
  (->> range-strs
       (map get-invalid-ids)
       flatten
       (reduce +)))

;; ----------------------------------------------------------------------------

;; Examples from AOC
(comment
  ;; 11 and 22
  (get-invalid-ids "11-22")
  ;; 99 and 111
  (get-invalid-ids "95-115")
  ;; 1010 and 1010
  (get-invalid-ids "998-1012")
  ;; 1188511885
  (get-invalid-ids "1188511880-1188511890"))

;; Solution example from AOC
(comment
  ;; 4174379265
  (get-invalid-id-sum ["11-22"
                       "95-115"
                       "998-1012"
                       "1188511880-1188511890"
                       "222220-222224"
                       "1698522-1698528"
                       "446443-446449"
                       "38592856-38593862"
                       "565653-565659"
                       "824824821-824824827"
                       "2121212118-2121212124"]))

;; Check using the actual input...
(comment
  (let [ranges (-> "input/day02/day02-input.txt"
                   slurp
                   (string/split #",")
                   (->> (map string/trim)))]
    (get-invalid-id-sum ranges)))
