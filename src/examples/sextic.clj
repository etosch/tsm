(ns tsm.examples.sextic
  (:use [tsm.core]
	[tsm.instructions ts x float])
  (:require [clojush.pushstate :as push]))

(def sextic-samples
     (let [rands (take 10 (repeatedly #(rand 100)))]
       (for [r rands]
	 [r (+ (* r r r r r r)
	       (* r r r r)
	       (* r r))])))

(defn error-function [state]
  (reduce + (for [[x f-at-x] sextic-samples]
	      (let [err (- f-at-x (last ((eval-tsm (add-to-stack state :float x)) :float)))]
		(* err err)))))
