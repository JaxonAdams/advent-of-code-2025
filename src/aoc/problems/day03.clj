(ns aoc.problems.day03
  (:require [clojure.string :as string]))

(defn- max-voltage [battery-bank]
  (let [voltages (-> battery-bank
                     (string/split #"")
                     (->> (map Integer/parseInt)
                          (map-indexed vector)))
        tens-candidates (take (dec (count battery-bank)) voltages)
        ones-candidates voltages
        sorted-tens-by-voltage (reverse (sort-by second tens-candidates))
        sorted-ones-by-voltage (reverse (sort-by second ones-candidates))
        max-voltage (first sorted-tens-by-voltage)
        next-max-voltage (first (filter #(-> % first (> (first max-voltage))) sorted-ones-by-voltage))]
    (->> [max-voltage next-max-voltage]
         (map second)
         (apply str)
         Integer/parseInt)))

(comment
  ;; 98
  (max-voltage "987654321111111")
  ;; 89
  (max-voltage "811111111111119")
  ;; 78
  (max-voltage "234234234234278")
  ;; 92
  (max-voltage "818181911112111"))
