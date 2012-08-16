(ns tsm.evolve.random
  (:require [clojush.pushstate :as push]
	    [clojush.random :as pushrand])
  (:use [tsm.core]
	[tsm.config]))

(def instructions (assoc @push/instruction-table
		    :noop :noop
		    'rand-int pushrand/lrand-int
		    'rand-float pushrand/lrand))

;; rather ugly, since i wrote this at the pub; should be cleaned up when sober
(defn random-code [code-size inst]
  (let [sans-pair (loop [x '() remaining-points code-size]
		    (if (= 0 remaining-points)
		      x
		      (let [[n v] (pushrand/lrand-nth (seq inst))]
			(cond (or (= n 'rand-int) (= n 'rand-float)) (recur (conj x (v 10)) (dec remaining-points))
			      (or (= 'ts_tag n) (= 'ts_tagged n)) (recur (conj x {:imap n :tag (pushrand/lrand 10) :ith (inc (pushrand/lrand-int 2))}) (dec remaining-points))
			      (meta v) (recur (conj x (reduce #(assoc %1 (%2 0) ((%2 1))) {:imap n} (seq (meta v)))) (dec remaining-points))
			      :else (recur (conj x n) (dec remaining-points))))))]
    (loop [before '() after sans-pair]
      (cond (empty? after) (vec before)
	    (and (= 0 (pushrand/lrand-int @max-int)) (> (count after) 1)) (recur (conj before (vec (take 2 after))) (drop 2 after))
	    :else (recur (conj before (first after)) (rest after))))))