(ns tsm.instructions.boolean
  (:use [tsm core util])
  (:require [clojush.pushstate :as push]))

(push/define-registered boolean_and
  (fn [{boolstack :boolean, :as state}]
    (if (> (count boolstack) 1)
      (let [[boolstack [phi psi]] (vec-split boolstack -2)]
	(add-to-stack (assoc state :boolean boolstack) :boolean (and phi psi)))
      state)))

(push/define-registered boolean_or
  (fn [{boolstack :boolean, :as state}]
    (if (> (count boolstack) 1)
      (let [[boolstack [phi psi]] (vec-split boolstack -2)]
	(add-to-stack (assoc state :boolean boolstack) :boolstack (or phi psi)))
      state)))

(push/define-registered boolean_not
  (fn [{boolstack :boolean, :as state}]
    (if (> (count boolstack) 0)
      (let [[boolstack [phi]] (vec-split boolstack -1)]
	(add-to-stack (assoc state :boolean boolstack) :boolstack (not phi)))
      state)))
							   