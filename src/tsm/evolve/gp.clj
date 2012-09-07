(ns tsm.evolve.gp
  (:use [tsm core config util instructions])
  (:require [tsm.evolve.random :as rand]
	    [clojure.string :as s])
  )


;; right now xover only operates on the top tag space
;; returns a vec because it had previously been implemented to return two children;
;; this way we don't try to call first somewhere on something that's not a collection
(defn crossover [{mts-stack :ts, :as mom} {pts-stack :ts, :as pop}]
  (when @debug (println "crossover"))
  (let [[mom-rest-ts [mts]] (vec-split mts-stack -1)
	[pop-rest-ts [pts]] (vec-split pts-stack -1)
	child (concat (take (inc (rand-int (count mts))) (shuffle (seq mts)))
		      (take (inc (rand-int (count pts))) (shuffle (seq pts))))]
    [(assoc mom :ts (conj mom-rest-ts (veclist-to-map child)))]
    )
  )

(defn mutate [{ts-stack :ts, :as state} atom-generators]
  (when @debug (println "mutate"))
  (let [[ts-rest [ts]] (vec-split ts-stack -1)
	[target-tag target-val] (nth (seq ts) (rand-int (count ts)))
	popped-ts (dissoc ts target-tag)]
    (assoc state
      :ts
      (conj ts-rest
	    (get {:tag (assoc popped-ts (rand @tag-limit) target-val)
		  :value (assoc popped-ts target-tag (let [[i1 i2] (rand/random-instructions 2 atom-generators)]
						       (get {:first [i1 (target-val 1)]
							     :second [(target-val 0) i2]
							     :both [i1 i2]
							     :neither i1}
							    (if (is-pair? target-val)
							      (rand-nth '(:first :second :both))
							      (rand-nth '(:both :neither))))))}
		 (rand-nth '(:tag :value)))))))

;; basic tournament selection
;; maybe set this up as a multimethod to implement the various types of selection
(defn select [error-function distance-function distance-comparator
	      population selection-type tourney-size]
  (when @debug (println "select"))
  (cond (= selection-type :basic-tourney) (reduce #(if (distance-comparator (distance-function (error-function %1))
									    (distance-function (error-function %2)))
						     %1 %2)
						  (take tourney-size (shuffle population)))
	:else (throw (Exception. (str "Selection type: " selection-type " not implemented.")))))

(defn breed [population error-function distance-function distance-comparator
	     mutation-probability crossover-probability
	     selection-type tourney-size atom-generators]
  (when @debug (println "breed"))
  (let [n (rand)
	i1 (select error-function distance-function distance-comparator population selection-type tourney-size)
	i2 (select error-function distance-function distance-comparator population selection-type tourney-size)]
    (cond (< n mutation-probability) (mutate i1 atom-generators)
	  (< n (+ mutation-probability crossover-probability)) ((crossover i1 i2) 0)
	  :else i1)))

(defn partition-population [population feature value]
  (loop [pop population us '() them '()]
    (cond (empty? pop) [us them]
	  (= value (get (first pop) feature)) (recur (rest pop) (cons (first pop) us) them)
	  :else (recur (rest pop) us (cons (first pop) them)))))

(defn frequency-and-err-stats [individuals feature]
  (loop [result {} guys individuals]
    (if (empty? guys) result
	(let [this-val (get (first guys) feature)
	      [these-guys those-guys] (partition-population guys feature this-val)]
	  (recur (assoc result this-val (let [us (vec (sort-by :error < these-guys)) ct (count us)]
					  {:ct ct
 					   :min-err ((first us) :error)
 					   :max-err ((last us) :error)
 					   :med-err (if (odd? ct)
 						      ((nth us (int (/ ct 2))) :error)
						      (let [[a b] (subvec us (dec (/ ct 2)) (inc (/ ct 2)))]
							(/ (+ (a :error) (b :error)) 2)))
					   }
					  ))
		 those-guys)))))

(defn print-report [population]
  (when @debug (println "report"))
  (let [fmap (frequencies (map :error population))
	best-program (first population)
	best-program-pretty (str "\n\tTop tag space size: " (first (best-program :ts-size))
				 "\n\tError: " (best-program :error)
				 "\n\tNumber of executed steps: " (doall (seq (best-program :step-vector)))
				 "\n\tIndividual:\n\t\t{" (reduce str (for [[k v] (best-program :individual)] (str k " " v "\n\t\t")))
				 "}")
	smap ;;(frequency-and-err-stats population :ts-size)
	(veclist-to-map (seq (frequencies (map #(first (:ts-size %)) population))))
	copies (frequencies (sort (vals (frequencies (map :individual population)))))
	execution-steps ;;(frequency-and-err-stats population :step-vector)
	(frequencies (map :step-vector population))
	]
    (printf "Frequency Map: %s\nBest Program%s\nSize Map: %s\nCopy Map: %s\nExecution Steps Map: %s\n"
	    fmap best-program-pretty smap copies execution-steps)))

(defn evolve [& {:keys [population-size problem-name max-points max-generation
			mutation-probability crossover-probability selection-type tournament-size
			error-function atom-generator distance-metric
			], :or {population-size 10
				max-points 10
				max-generation 10
				mutation-probability 0.1
				crossover-probability 0.8
				selection-type :basic-tourney
				tournament-size 3
				distance-metric :L1Norm
				}, :as args}
	      ]
  (doseq [[k v] args]
    (when v (println (str (s/capitalize (s/replace (name k) #"-" " ")) ":") v)))
  (use problem-name)
  (let [atom-generators (if atom-generator (eval atom-generator) (seq @registered-instructions))
	time-limit @time-limit
	err @@(ns-resolve problem-name 'error-function)
	prep-state @@(ns-resolve problem-name 'prep-state)
	training-data @@(ns-resolve problem-name 'training-data)
	[distance-function distance-comparator] (get @distances distance-metric)
	gen-0 (repeatedly population-size #(rand/random-tsm max-points atom-generators))]
    (printf "Instruction Map: %s\nMax Evaluations: %s\nTraining Data: %s\nDistance Metric: %s\n"
	    (frequencies atom-generators) time-limit (doall (seq training-data)) distance-metric)
    (loop [individuals gen-0 gen 0]
      (let [individuals-with-fitnesses (sort-by :error distance-comparator
						(map #(let [error-vector (try (err %)
									      (catch Exception e Float/MAX_VALUE))
							    step-vector (->> training-data
									     (map first)
									     (map (fn [td] (prep-state % td)))
									     (map (fn [tsm] (try (eval-tsm tsm)
												 (catch Exception e (with-meta tsm {:steps time-limit})))))
									     (map meta)
									     (map :steps))]
							(hash-map :individual %
								  :error-vector error-vector
								  :error (distance-function error-vector)
								  :ts-size (map count (% :ts))
								  :step-vector step-vector))
						     individuals))]
	(println "GENERATION:" gen) 
	(print-report individuals-with-fitnesses)
	(cond (> gen max-generation) (println "FAILURE")
	      (= 0 ((first individuals-with-fitnesses) 0)) (println "SUCCESS")
	      :else (recur (repeatedly population-size #(breed individuals
							       err distance-function distance-comparator
							       mutation-probability crossover-probability
							       selection-type
							       tournament-size
							       atom-generators))
			   (inc gen)))))))

(defn -main [& args]
  (apply evolve (map read-string args)))