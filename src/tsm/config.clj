(ns tsm.config)

;; things you might want to configure:
(def literals
     (atom {:string string?
	    :boolean (fn [thing] (or (= false thing) (= true thing)))
	    :float float?
	    :integer integer?}))

(def debug (atom nil))

(def max-int (atom 10))
(def max-float (atom 10.0))
(def tag-limit (atom 1.0))
(def time-limit (atom 10000))

(def error-function (atom (fn [] (throw (Exception. "error-function not set")))))