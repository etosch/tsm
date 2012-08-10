(ns tsm.instructions.integer
  (:use [tsm.core]
	[tsm.util])
  (:require [clojush.pushstate :as push]))


;; temporary instructions due to incompatabilities with clojush state implementation

(push/define-registered integer_add
  (fn [{intstack :integer, :as state}]
    (if (> (count intstack) 1)
      (let [[stack [x y]] (vec-split intstack -2)]
	(add-to-stack (assoc state :integer stack) :integer (+ x y)))
      state)))

(push/define-registered integer_mult
  (fn [{intstack :integer, :as state}]
    (if (> (count intstack) 1)
      (let [[stack [x y]] (vec-split intstack -2)]
	(add-to-stack (assoc state :integer stack) :integer (* x y)))
      state)))

(push/define-registered integer_add_const
  (fn [{intstack :integer, :as state} & {const :const}]
    (if-let [[stack [x]] (vec-split intstack -1)]
      (add-to-stack (assoc state :integer stack) :integer (+ x const))
      state)))