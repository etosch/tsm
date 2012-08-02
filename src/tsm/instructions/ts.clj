(ns tsm.instructions.ts
  (:use [tsm.core])
  (:require [clojush.pushstate :as push]))

(push/define-registered ts_pop
  (fn [state]
    (if-not (empty? (state :ts))
      (assoc state :ts (pop (get state :ts)))
      state)))

;; default behavior is pop -- I suppose a nopop could be implemented with "baked-in" args at a later point
(push/define-registered ts_tag
  (fn [{x :x, ts :ts, :as state} tag & {pop? :pop}]
    (if-let [tagged-cmd (last x)]
      (let [[rest-ts [current-ts & _]] (vec-split ts -1)]
	(assoc (if (= pop? :nopop)
		 state
		 (assoc state :x (pop x)))
	  :ts
	  (conj rest-ts (assoc current-ts tag tagged-cmd)))))))

(push/define-registered ts_tag_pair
  (fn [{x :x, ts :ts, :as state} tag & {pop? :pop}]
    (if (> (count x) 1)
      (let [[p1 p2 & _] x
	    [rest-ts [current-ts & _]] (vec-split ts -1)]
	(assoc (if (= pop? :nopop)
		 state
		 (assoc state :x (pop (pop x))))
	  :ts
	  (conj rest-ts (assoc current-ts tag [p1 p2])))))))

;; what happens if the tag space is empty?
(push/define-registered ts_tagged
  (fn [{x :x, ts :ts, :as state} tag & {pop? :pop}]
    (assoc state :x (conj x (match-tag (first ts) tag)))))

(push/define-registered ts_tagged_under
  (fn [{x :x, ts :ts, :as state} tag & {pop? :pop}]
    (assoc state :x (conj (pop x) (match-tag (first ts) tag) (last x)))))

(push/define-registered ts_new
  (fn [{ts :ts, :as state}]
    (assoc state :ts (conj ts {}))))

;; still need to define the 2-versions of the tag spaces - would suggest having this as an argument
;; in the imap, since the imap instructions just apply the function to the appropriate arguments.
;; the ts instructions above would then just need to pick out the relevent tag space
;; (if we're using arguments in the imap anyway, why not leverage this?)

	