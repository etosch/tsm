(ns tsm.evolve.gp
  (:use [tsm core config util])
  (:require [tsm.evolve.random :as rand]
	    [clojush.random :as pushrand]
	    [clojush.pushstate :as push])
  )

;; right now xover only operates on the top tag space
(defn crossover [{mts-stack :ts, :as mom} {pts-stack :ts, :as pop}
		 & {:keys [momseed popseed]}]
  (when @debug (println "crossover"))
  (let [[mom-rest-ts [mts]] (vec-split mts-stack -1)
	[pop-rest-ts [pts]] (vec-split pts-stack -1)
	mompoint (or momseed (pushrand/lrand-int (count mts)))
	poppoint (or popseed (pushrand/lrand-int (count pts)))
	kid1-tags-and-vals (concat (take mompoint (seq mts))
				   (drop poppoint (seq pts)))
	kid2-tags-and-vals (concat (take poppoint (seq pts))
				   (drop mompoint (seq mts)))]
    [(assoc mom :ts (conj mom-rest-ts (veclist-to-map kid1-tags-and-vals)))
     (assoc pop :ts (conj pop-rest-ts (veclist-to-map kid2-tags-and-vals)))]
    )
  )

(defn mutate [{ts-stack :ts, :as state} atom-generators]
  (when @debug (println "mutate"))
  (let [[ts-rest [ts]] (vec-split ts-stack -1)
	[target-tag target-val] (nth (seq ts) (pushrand/lrand-int (count ts)))
	popped-ts (dissoc ts target-tag)]
    (assoc state
      :ts
      (conj ts-rest
	    (get {:tag (assoc popped-ts (pushrand/lrand @tag-limit) target-val)
		  :value (assoc popped-ts target-tag (let [[i1 i2] (rand/random-instructions 2 atom-generators)]
						       (get {:first [i1 (target-val 1)]
							     :second [(target-val 0) i2]
							     :both [i1 i2]
							     :neither i1}
							    (if (is-pair? target-val)
							      (pushrand/lrand-nth '(:first :second :both))
							      (pushrand/lrand-nth '(:both :neither))))))}
		 (pushrand/lrand-nth '(:tag :value)))))))

;; basic tournament selection
;; maybe set this up as a multimethod to implement the various types of selection
(defn select [error-function population selection-type tourney-size]
  (when @debug (println "select"))
  (cond (= selection-type :basic-tourney) (reduce #(if (< (error-function %1) (error-function %2)) %1 %2)
						  (take tourney-size (pushrand/lshuffle population)))
	:else (throw (Exception. (str "Selection type: " selection-type " not implemented.")))))

(defn breed [population error-function mutation-probability crossover-probability
	     selection-type tourney-size atom-generators]
  (when @debug (println "breed"))
  (let [n (pushrand/lrand)
	i1 (select error-function population selection-type tourney-size)
	i2 (select error-function population selection-type tourney-size)]
    (cond (< n mutation-probability) (mutate i1 atom-generators)
	  (< n (+ mutation-probability crossover-probability)) ((crossover i1 i2) 0)
	  :else i1)))

(defn print-report [population]
  (when @debug (println "report"))
  (let [fmap (frequencies (map :fitness population))
	best-program (first population)
	best-program-pretty (str "\n\tTag space size vector: " (doall (seq (best-program :ts-size)))
				 "\n\tFitness: " (best-program :fitness)
				 "\n\tIndividual:\n\t\t{" (reduce str (for [[k v] (best-program :individual)] (str k " " v "\n\t\t")))
				 "}")
	smap (frequencies (map :ts-size population))
	copies (frequencies (sort (vals (frequencies (map :individual population)))))]
    (printf "Frequency Map: %s\nBest Program%s\nSize Map: %s\nCopy Map: %s\n" fmap best-program-pretty smap copies)))

(defn evolve [& {:keys [population-size problem-name max-points max-generation
			mutation-probability crossover-probability selection-type tournament-size
			error-function 
			], :or {population-size 10
				max-points 10
				max-generation 10
				mutation-probability 0.1
				crossover-probability 0.8
				selection-type :basic-tourney
				tournament-size 3
				}
		 }]
  (use problem-name)
  (let [atom-generators (seq @push/instruction-table)
	err @@(ns-resolve problem-name 'error-function)
	gen-0 (repeatedly population-size #(rand/random-tsm max-points atom-generators))]
    (loop [individuals gen-0 gen 0]
      (let [individuals-with-fitnesses (sort-by :fitness < (map #(hash-map :individual %
									   :fitness (try (err %)
											 (catch Exception e Float/MAX_VALUE))
									   :ts-size (map count (% :ts)))
								individuals))]
	(println "GENERATION: " gen) 
	(print-report individuals-with-fitnesses)
	(flush)
	(cond (> gen max-generation) (println "FAILURE")
	      (= 0 ((first individuals-with-fitnesses) 0)) (println "SUCCESS")
	      :else (recur (repeatedly population-size #(breed individuals
							       err
							       mutation-probability
							       crossover-probability
							       selection-type
							       tournament-size
							       atom-generators))
			   (inc gen)))))))

(defn -main [& args]
  (apply evolve (map read-string args)))