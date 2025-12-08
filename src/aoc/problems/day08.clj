(ns aoc.problems.day08)

(defn- straight-line-distance
  "https://en.wikipedia.org/wiki/Euclidean_distance"
  [p1 p2]
  (Math/sqrt
   (reduce + (map #(Math/pow (- %2 %1) 2) p1 p2))))

(defn- get-all-distances [junction-boxes]
  (->> (for [a junction-boxes
             b junction-boxes
             :when (not= a b)]
         [(sort [a b]) (straight-line-distance a b)])
       (sort-by second)
       dedupe))

;; ----------------------------------------------------------------------------
;; PART ONE

(comment

  (def example-distances [[162 817 812]
                          [57 618 57]
                          [906 360 560]
                          [592 479 940]
                          [352 342 300]
                          [466 668 158]
                          [542 29 236]
                          [431 825 988]
                          [739 650 466]
                          [52 470 668]
                          [216 146 977]
                          [819 987 18]
                          [117 168 530]
                          [805 96 715]
                          [346 949 466]
                          [970 615 88]
                          [941 993 340]
                          [862 61 35]
                          [984 92 344]
                          [425 690 689]])

  (->> example-distances
       get-all-distances))
