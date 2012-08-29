(ns tsm.evolve.random
  (:require [clojush.pushstate :as push]
	    [clojush.random :as pushrand])
  (:use [tsm.core]
	[tsm.config]))

(defn is-rand-inst? [thing] (cond (= thing 'rand-int) @max-int
				  (= thing 'rand-float) @max-float
				  :else false))
(defn is-tag-inst? [thing] (contains? (disj (set (push/registered-for-type 'ts))
					    'ts_pop 'ts_new)
				      thing))

(defn random-instructions [code-size atom-generators]
  (loop [x '()
	 remaining-points code-size]
    (if (= 0 remaining-points) x
	(let [[n v] (pushrand/lrand-nth atom-generators)
	      points (dec remaining-points)]
	    (if-let [max-num (is-rand-inst? n)]
	      (recur (conj x (v max-num)) points)
	      (cond (is-tag-inst? n) (recur (conj x {:imap n
						     :tag (pushrand/lrand @tag-limit)
						     :ith (inc (pushrand/lrand-int 2))})
					    points)
		    (meta v) (recur (conj x (reduce #(assoc %1 (%2 0) ((%2 1)))
						    {:imap n}
						    (seq (meta v))))
				    points)
		    :else (recur (conj x n) points)))))))
  
(defn group-pairs
  "Groups a list of instructions into pairs according to some probability. If no probability is supplied, all instructions are grouped into pairs."
  ([instruction-list atom-generators probability]
     (loop [before '() after instruction-list]
       (cond (empty? after) (vec before)
	     (and (< (pushrand/lrand) probability)
		  (> (count after) 1))
	     (recur (conj before (vec (take 2 after)))
		    (drop 2 after))
	     :else (recur (conj before (first after)) (rest after)))))
  ([instruction-list atom-generators]
     (group-pairs instruction-list atom-generators 1.0)))

(defn make-ts [instruction-list]
  (loop [ilist instruction-list ts (sorted-map)]
    (if (empty? ilist) [ts]
	(recur (rest ilist)
	       (assoc ts (pushrand/lrand @tag-limit) (first ilist))))))

(defn random-tsm [code-size atom-generators]
  (ensure-state {:ts (-> (random-instructions code-size atom-generators)
			 (group-pairs atom-generators)
			 (make-ts))}))