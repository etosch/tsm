(ns tsm.config
  (:require [clojure.math.numeric-tower :as math]))

;; things you might want to configure:
(def literals
     (atom {:string string?
	    :boolean (fn [thing] (or (= false thing) (= true thing)))
	    :float float?
	    :integer integer?
	    :auxiliary (fn [thing] (and thing (not thing)))
	    }))

(def debug (atom nil))

(def max-int (atom 10))
(def max-float (atom 10.0))
(def tag-limit (atom 1.0))
(def time-limit (atom 10000))

(def distances
     "For each entry in the map, the first element of the value's vector is the distance function and the second element is the distance comparator."
     (atom
      {:L1Norm [(fn [vector-of-errors] (reduce + (map math/abs vector-of-errors))) <]		
       :L2Norm [(fn [vector-of-errors] (math/expt (reduce + (map #(math/expt % 2)
								 vector-of-errors))
						  (/ 1 2)))
		<]
       }))
(def error-function (atom (fn [] (throw (Exception. "error-function not set")))))
(def prep-state (atom (fn [state training-datum-x]
			(-> (assoc state :auxiliary [training-datum-x])
			    (assoc :x [{:imap 'ts_tagged :tag 0.0}])))))
(def training-data (atom nil))