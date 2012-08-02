(ns tsm.util)

;; vector splitting util
(defn vec-split
  "Returns a vector of two vectors. If the pivot is 0, the first item is the full vector. If the pivot is greater than the size of the vector, the second item is the full vector."
  [v pivot]
  (cond (> (if (neg? pivot) (- pivot) pivot) (count v)) [[] v]
	(pos? pivot) [(subvec v 0 pivot) (subvec v pivot)]
	:else [(subvec v 0 (+ (count v) pivot)) (subvec v (+ (count v) pivot))]))
