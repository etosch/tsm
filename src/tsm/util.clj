(ns tsm.util)

;; vector splitting util
(defn vec-split
  "Returns a vector of two vectors. If the pivot is 0, the first item is the full vector. If the pivot is greater than the size of the vector, the second item is the full vector."
  [v pivot]
  (cond (> (if (neg? pivot) (- pivot) pivot) (count v)) [[] v]
	(pos? pivot) [(subvec v 0 pivot) (subvec v pivot)]
	:else [(subvec v 0 (+ (count v) pivot)) (subvec v (+ (count v) pivot))]))

(defn exists
  [coll & {pred :pred}]
  (when (not (empty? coll))
    (or ((or pred identity) (first coll))
	(recur (rest coll) {:pred pred}))))

(defn forall
  [coll & {pred :pred}]
  (or (empty? coll)
      (and ((or pred identity) (first coll))
	   (recur (rest coll) {:pred pred}))))

(defn veclist-to-map
  "Takes as its argument a list of vectors of size 2 and returns a sorted map where the first elements of the vectors are keys and the second elements are values.
e.g. (veclist-to-map '([:a 0][:b 1][:c 2])) returns {:a 0, :b 1, :c 2}."
  [vec-list]
  (reduce #(assoc %1 (%2 0) (%2 1)) (sorted-map) vec-list))