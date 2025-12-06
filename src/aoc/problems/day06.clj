(ns aoc.problems.day06
  (:require [clojure.string :as string]))

(defn- parse-homework [homework-lines]
  (let [split-on-spaces (->> homework-lines
                             (map string/trim)
                             (map #(string/split % #"\s+")))
        operands (->> split-on-spaces
                      butlast
                      (map #(map Long/parseLong %)))
        operations (last split-on-spaces)]
    (map-indexed
     (fn [idx operation]
       (into [operation]
             (map #(nth % idx) operands)))
     operations)))

;; ----------------------------------------------------------------------------
;; PART ONE

(comment
  (def problems ["123 328  51 64 "
                 " 45 64  387 23 "
                 "  6 98  215 314"
                 "*   +   *   +  "])

  (parse-homework problems))
