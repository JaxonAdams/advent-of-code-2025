(ns aoc.utils.parsing)

(defn- split-by [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [[xs ys] (split-with pred s)]
       (if (seq xs)
         (cons xs (split-by pred ys))
         (let [!pred (complement pred)
               skip (take-while !pred s)
               others (drop-while !pred s)
               [xs ys] (split-with pred others)]
           (cons (concat skip xs)
                 (split-by pred ys))))))))

(defn split-at-value [value coll]
  (let [split (split-by (complement #{value}) coll)]
    (into [(first split)] (map rest (rest split)))))
