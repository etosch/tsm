(ns tsm.config)

;; things you might want to configure:
(def literals
     (atom {:string string?
	    :boolean (fn [thing] (or (= false thing) (= true thing)))
	    :float float?
	    :integer integer?}))

(def debug (atom true))
