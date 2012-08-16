(ns tsm.evolve.random
  (:require [clojush.pushstate :as push]
	    [clojush.random :as pushrand])
  (:use [tsm.core]
	[tsm.config]))

(def instructions (seq (assoc @push/instruction-table
			 :noop :noop
			 'rand-int pushrand/lrand-int
			 'rand-float pushrand/lrand)))

(defn is-rand-inst? [thing] (cond (= thing 'rand-int) @max-int
				  (= thing 'rand-float) @max-float
				  :else false))
(defn is-tag-inst? [thing] (or (= 'ts_tag thing) (= 'ts_tagged thing)))

(defn random-code [code-size]
  (let [sans-pair (loop [x '() remaining-points code-size]
		    (if (= 0 remaining-points) x
			(let [[n v] (pushrand/lrand-nth instructions)
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
				  :else (recur (conj x n) points))))))]
    ;; now pair according to uniform probability of all instructions
    (loop [before '() after sans-pair]
      (cond (empty? after) (vec before)
	    (and (= 0 (pushrand/lrand-int (inc (count instructions))))
		 (> (count after) 1))
	           (recur (conj before (vec (take 2 after)))
			  (drop 2 after))
	    :else (recur (conj before (first after)) (rest after))))))