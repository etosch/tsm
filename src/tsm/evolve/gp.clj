(ns tsm.evolve.gp
  (:use [tsm core config util]
	[tsm.evolve random])
  (:require [clojush.random :as pushrand])
  )

;; right now xover only operates on the top tag space
(defn crossover [{mts-stack :ts, :as mom} {pts-stack :ts, :as pop}]
  (let [[mom-rest-ts [mts]] (vec-split mts-stack -1)
	[pop-rest-ts [pts]] (vec-split pts-stack -1)
	mompoint (pushrand/lrand-int (count mts))
	poppoint (pushrand/lrand-int (count pts))
	kid1-tags-and-vals (concat (take mompoint (seq mts))
				   (drop poppoint (seq mts)))
	kid2-tags-and-vals (concat (take poppoint (seq pts))
				   (drop mompoint (seq pts)))]
    [(assoc mom :ts (conj mom-rest-ts (reduce #(assoc %1 (%2 0) (%2 1)) (sorted-map) kid1-tags-and-vals)))
     (assoc pop :ts (conj pop-rest-ts (reduce #(assoc %1 (%2 0) (%2 1)) (sorted-map) kid2-tags-and-vals)))]
    )
  )

(defn mutate [{ts-stack :ts, :as ind}]
  (let [[ts-rest [ts]] (vec-split ts-stack -1)
	[target-tag target-val] (nth (seq ts) (pushrand/lrand-int (count ts)))
	tag-or-val? (pushrand/lrand-int 2)]
    (assoc ind :ts (conj ts-rest (assoc (dissoc ts target-tag)
				   (if (= 0 tag-or-val?) (pushrand/lrand @tag-limit) target-tag)
				   (if (= 0 tag-or-val?) target-tag (let [i (random-instructions 2)]
								      (if (= 0 (pushrand/lrand-int 2))
									(vec i)
									(first i)))))))
    )
  )

;; basic tournament selection
;; maybe set this up as a multimethod to implement the various types of selection
(defn select [error-function population selection-type tourney-size]
  (cond (= selection-type :basic-tourney) (reduce #(if (< (error-function %1) (error-function %2)) %1 %2)
						  (take tourney-size (pushrand/lshuffle population)))
	:else (throw (Exception. (str "Selection type: " selection-type " not implemented.")))))

(defn breed [population error-function mutation-probability crossover-probability selection-type tourney-size]
  (let [n (pushrand/lrand)
	i1 (select error-function population selection-type tourney-size)
	i2 (select error-function population selection-type tourney-size)]
    (cond (< n mutation-probability) (mutate i1)
	  (< n (+ mutation-probability crossover-probability)) (first (crossover i1 i2))
	  :else i1)))

(defn report [population]
  (let [fmap (frequencies (map :fitness population))
	best-program (first population)
	smap (frequencies (map :ts-size population))
	copies (sort (vals (frequencies (map :individual population))))]
    (printf "Frequency Map: %s\nBest Program: %s\nSize Map: %s\nCopy Vector: %s\n" fmap best-program smap copies)))

(defn evolve [& {:keys [population-size problem-name max-points time-limit max-generation
			mutation-probability crossover-probability selection-type tournament-size
			error-function
			], :or {population-size 1000
				max-points 50
				time-limit (* 50 50)
				max-generation 1000
				mutation-probability 0.1
				crossover-probability 0.8
				selection-type :basic-tourney
				tournament-size 10
				}
		 }]
  (let [gen-0 (repeatedly population-size #(random-tsm max-points))]
    (loop [individuals gen-0
	   gen 0]
      (let [individuals-with-fitnesses (sort-by :fitness > (map #(hash-map :indvidual %
									   :fitness (try (error-function %)
											 (catch Exception e 1000000000))
									   :ts-size (map count (% :ts)))
								individuals))]
	(report individuals-with-fitnesses)
	(flush)
	(cond (> gen max-generation) (println "FAILURE")
	      (= 0 ((first individuals-with-fitnesses) 0)) (println "SUCCESS")
	      :else (recur (for [_ (range population-size)]
			     (breed individuals error-function mutation-probability crossover-probability selection-type tournament-size))
			   (inc gen)))))))

(defn -main [& args]
  (apply evolve args))