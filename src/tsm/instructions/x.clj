(ns tsm.instructions.x
  (:use [tsm.core])
  (:require [clojush.pushstate :as push]))

(push/define-registered x_dup
  (fn [{x :x, :as state}]
    (if-let [duped (last x)]
      (add-to-stack state :x duped)
      state)))

(push/define-registered x_swap
  (fn [{x :x, :as state}]
    (if (> (count x) 1)
      (let [[x1 x2 & more] x]
	(add-to-stack (assoc state :x more) :x x2 :x x1)))))

(push/define-registered x_rot
  (fn [{x :x, :as state}]
    (if (> (count x) 2)
      (let [[x1 x2 x3 & more] x]
	(add-to-stack (assoc state :x more) :x x3 :x x1 :x x2)))))

(push/define-registered x_if
  (fn [{x :x, boolean :boolean, :as state}]
    (if (and (> (count x) 1) (not (empty? boolean)))
      (let [[pred & boolstack] boolean
	    [consequent subsequent & xstack] x]
	(add-to-stack (assoc state :boolean boolstack :x xstack) :x (if pred consequent subsequent))))))