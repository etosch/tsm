(ns examples.sextic
  (:use [tsm core config instructions]
	[tsm.instructions ts x float])
  (:require [clojush.random :as pushrand]
	    [clojure.math.numeric-tower :as math]))

(define-registered noop identity)
(define-registered in
  (fn [{auxstack :auxiliary, :as state}]
    (if (> (count auxstack) 0)
      (add-to-stack state :float (last auxstack))
      state)))

(doseq [instruction (registered-for-type 'ts)
	_ (range 10)]
  (swap! registered-instructions conj instruction))

(reset! training-data
	(let [rands (take 10 (repeatedly #(rand 100)))]
	  (for [r rands]
	    [r (+ (* r r r r r r)
		  (* r r r r)
		  (* r r))])))

;; right now this is just pushing the example number onto the float stack and seeding the x stack
;; with a {:imap ts_tagged :tag 0.0} init
(let [ps @prep-state td @training-data]
  (reset! error-function
	  (fn [state]
	    (for [[x f-at-x] td]
		(- f-at-x (try (last (get (eval-tsm (ps state x)) :float))
			       (catch IndexOutOfBoundsException ioobe (/ Float/MAX_VALUE 2.0))
			       (catch Exception time-limit-exception Float/MAX_VALUE)))))))

#_(do (use 'tsm.evolve.gp)
      (in-ns 'tsm.evolve.gp)
      (evolve :problem-name 'examples.sextic
	      :population-size 10
	      :max-points 10
	      :max-generation 50
	      :crossover-probability 0.5
	      :mutation-probability 0.1
	      :atom-generator '(flatten (concat (seq @registered-instructions) 
						(repeat 5 (registered-for-type 'ts))
						(repeat 10 'in))))
      )



		      