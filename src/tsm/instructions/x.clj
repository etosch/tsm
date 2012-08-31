(ns tsm.instructions.x
  (:use [tsm core util instructions]))

(define-registered x_dup
  (fn [{x :x, :as state}]
    (if (> (count x) 0)
      (add-to-stack state :x (last x))
      state)))

(define-registered x_swap
  (fn [{x :x, :as state}]
    (if (> (count x) 1)
      (let [[more [x1 x2]] (vec-split x -2)]
	(add-to-stack (assoc state :x more) :x x2 :x x1))
      state)))

(define-registered x_rot
  (fn [{x :x, :as state}]
    (if (> (count x) 2)
      (let [[more [x1 x2 x3]] (vec-split x -3)]
	(add-to-stack (assoc state :x more) :x x3 :x x1 :x x2))
      state)))

(define-registered x_if
  (fn [{x :x, boolean :boolean, :as state}]
    (if (and (> (count x) 1) (not (empty? boolean)))
      (let [[boolstack [pred]] (vec-split boolean -1)
	    [xstack [consequent subsequent]] (vec-split x -2)]
	(add-to-stack (assoc state :boolean boolstack :x xstack) :x (if pred consequent subsequent)))
      state)))