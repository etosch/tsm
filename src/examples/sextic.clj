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

(defn fitness-function [state]
  (reduce + (for [[x f-at-x] sextic-samples]
	      (- f-at-z (
     