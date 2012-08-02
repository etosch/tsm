(ns tsm.instructions.float
  (:use [tsm.core]
	[tsm.util])
  (:require [clojush.pushstate :as push]))

;; temporary instructions due to incompatabilities with clojush state implementation

(push/define-registered float_add
  (fn [{floatstack :float, :as state}]
    (if (> (count floatstack) 1)
      (let [[stack [x y]] (vec-split floatstack -2)]
	(add-to-stack (assoc state :float stack) :float (+ x y))))))

(push/define-registered float_mult
  (fn [{floatstack :float, :as state}]
    (if (> (count floatstack) 1)
      (let [[stack [x y]] (vec-split floatstack -2)]
	(add-to-stack (assoc state :float stack) :float (* x y))))))


	