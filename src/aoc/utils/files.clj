(ns aoc.utils.files
  (:require [clojure.string :as string]))

(defn read-file-lines [file-path]
  (-> file-path
      slurp
      (string/split #"\n")
      (->> (map string/trim))))
