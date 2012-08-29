(ns examples.sextic
  (:use [tsm core config]
	[tsm.instructions ts x float])
  (:require [clojush.pushstate :as push]
	    [clojush.random :as pushrand]
	    [clojure.math.numeric-tower :as math]))

;; After initially provding input, I never want to push anything onto the auxiliary stack
(swap! literals assoc :auxiliary (fn [thing] (and thing (not thing))))

#_(push/define-registered noop identity)
(push/define-registered in
  (fn [{auxstack :auxiliary, :as state}]
    (if (> 0 (count auxstack))
      (add-to-stack state :float (last auxstack))
      state)))

(def sextic-samples
     (let [rands (take 10 (repeatedly #(rand 100)))]
       (for [r rands]
	 [r (+ (* r r r r r r)
	       (* r r r r)
	       (* r r))])))

;; right now this is just pushing the example number onto the float stack and seeding the x stack
;; with a {:imap ts_tagged :tag 0.0} init
(reset! error-function
	(fn [state]
	  (try	    
	    (reduce + (for [[x f-at-x] sextic-samples]
			(let [evaled-state (eval-tsm (-> (add-to-stack state :auxiliary x)
							 (add-to-stack :x {:imap 'ts_tagged :tag 0.0})))]
			  (- f-at-x (last (evaled-state :float))))))
	    (catch Exception e Float/MAX_VALUE))))


