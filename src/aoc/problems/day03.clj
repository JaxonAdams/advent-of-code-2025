(ns aoc.problems.day03
  (:require [clojure.string :as string]
            [utils.files :refer [read-file-lines]]))

(defn- max-voltage [battery-bank]
  (let [voltages (-> battery-bank
                     (string/split #"")
                     (->> (map Integer/parseInt)
                          (map-indexed vector)))
        tens-candidates (reverse (sort-by second (take (dec (count battery-bank)) voltages)))
        max-voltages-tens (sort-by first (filter #(-> tens-candidates first second (= (second %))) tens-candidates))
        max-voltage (first max-voltages-tens)
        ones-candidates (reverse (sort-by second (second (split-at (-> max-voltage first inc) voltages))))
        next-max-voltage (first ones-candidates)]
    (->> [max-voltage next-max-voltage]
         (map second)
         (apply str)
         Integer/parseInt)))

(defn sum-of-max-volts [battery-banks]
  (->> battery-banks
       (map max-voltage)
       (reduce +)))

(comment
  ;; 98
  (max-voltage "987654321111111")
  ;; 89
  (max-voltage "811111111111119")
  ;; 78
  (max-voltage "234234234234278")
  ;; 92
  (max-voltage "818181911112111"))

;; EDGE CASES
(comment
  ;; 99
  (max-voltage "997654321111111"))

(comment
  ;; 357
  (sum-of-max-volts ["987654321111111"
                     "811111111111119"
                     "234234234234278"
                     "818181911112111"]))

;; Part 1 Solution...
(comment
  ;; NOT 17323
  (->  "input/day03/input.txt"
       read-file-lines
       sum-of-max-volts))
