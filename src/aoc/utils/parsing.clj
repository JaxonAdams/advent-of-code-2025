(ns aoc.utils.parsing)

(defn split-at-value [value coll]
  (let [[left right] (split-with (complement #{value}) coll)]
    [left (drop-while #{value} right)]))
