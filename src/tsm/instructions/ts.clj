(ns tsm.instructions.ts
  (:use [tsm core util instructions]))

;; These instructions are executed as vanilla instructions

(define-registered ts_pop
  (fn [{ts :ts, :as state}]
    (if (empty? ts) state
	(assoc state :ts (pop ts)))))

(define-registered ts_new
  (fn [{ts :ts, :as state}]
    (assoc state :ts (conj ts (sorted-map)))))


;; These instructions all take a state and a tag as arguments

(define-registered ts_tag
  (fn [{x :x, ts :ts, :as state} tag pop? ith]
    (if (> (count x) 0)
      (let [tagged-cmd (last x) 
	    [rest-ts [current-ts & top-ts]] (vec-split ts (* -1 ith))]
	(assoc (if (= pop? :nopop) state (assoc state :x (pop x)))
	  :ts
	  (reduce conj
		  (conj rest-ts (assoc current-ts tag tagged-cmd))
		  top-ts)))
      state)))

(define-registered ts_tag_pair
  (fn [{x :x, ts :ts, :as state} tag pop? ith]
    (if (> (count x) 1)
      (let [[p1 p2 & _] x
	    [rest-ts [current-ts & top-ts]] (vec-split ts (* -1 ith))]
	(assoc (if (= pop? :nopop) state (assoc state :x (pop (pop x))))
	  :ts
	  (reduce conj
		  (conj rest-ts (assoc current-ts tag [p1 p2]))
		  top-ts)))
      state)))

(define-registered ts_tagged
  (fn [{x :x, ts :ts, :as state} tag pop? ith]
    (if-not (> ith (count ts))
      (let [current-ts (nth ts (- (count ts) ith))]
	(if-not (empty? current-ts)
	  (add-to-stack state :x (match-tag current-ts tag))
	  state))
      state)))