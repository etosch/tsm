(ns tsm.instructions.ts
  (:use [tsm core util])
  (:require [clojush.pushstate :as push]))

;; These instructions are executed as vanilla instructions

(push/define-registered ts_pop
  (fn [state]
    (if-not (empty? (state :ts))
      (assoc state :ts (pop (get state :ts)))
      state)))

(push/define-registered ts_new
  (fn [{ts :ts, :as state}]
    (assoc state :ts (conj ts (sorted-map)))))


;; These instructions all take a state and a tag as arguments

(push/define-registered ts_tag
  (fn [{x :x, ts :ts, :as state} tag pop? ith]
    (if-let [tagged-cmd (last x)]
      (let [[rest-ts [current-ts & top-ts]] (vec-split ts (* -1 ith))]
	(assoc (if (= pop? :nopop)
		 state
		 (assoc state :x (pop x)))
	  :ts
	  (reduce #(conj %1 %2)
		  (conj rest-ts (assoc current-ts tag tagged-cmd))
		  top-ts))))))

(push/define-registered ts_tag_pair
  (fn [{x :x, ts :ts, :as state} tag pop? ith]
    (if (> (count x) 1)
      (let [[p1 p2 & _] x
	    [rest-ts [current-ts & top-ts]] (vec-split ts (* -1 ith))]
	(assoc (if (= pop? :nopop)
		 state
		 (assoc state :x (pop (pop x))))
	  :ts
	  (reduce conj
		  (conj rest-ts (assoc current-ts tag [p1 p2]))
		  top-ts))))))

;; what happens if the tag space is empty?
(push/define-registered ts_tagged
  (fn [{x :x, ts :ts, :as state} tag pop? ith]
    (assoc state :x (conj x (match-tag (nth ts (- (count ts) ith)) tag)))))

(push/define-registered ts_tagged_under
  (fn [{x :x, ts :ts, :as state} tag pop? ith]
    (assoc state :x (conj (pop x) (match-tag (nth ts (- (count ts) ith)) tag) (last x)))))
	