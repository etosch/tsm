(ns tsm.core
  (:require [clojure.string :as s]
	    [clojush.pushstate :as push])
  (:use [tsm.util]
	[tsm.config]))

;; Override the default last operation to make it O(1) for vectors
(defn last [v]
  (nth v (dec (count v))))

(defn recognize-literal [thing]
  (loop [lits (seq @literals)]
    (if-let [[type pred] (first lits)]
      (if (pred thing) type
	  (recur (rest lits))))))

(defn is-pair? [thing] (and (vector? thing) (= 2 (count thing))))
(defn is-inst? [thing] (symbol? thing))
(defn is-imap? [thing] (and (map? thing) (thing :imap)))
(defn is-ts? [thing] (and (is-imap? thing) (= (first (s/split (str (thing :imap)) #"_")) "ts")))
(defn is-composite? [thing]
  (or (is-imap? thing) (is-pair? thing)))

;; Used for debugging
;; ------------------------------------------------------------------------------------------------
(defn is-state? [m]
  (and (m :x) (m :ts)
       (reduce #(and %1 %2) (map #(get m %) (keys @literals)))))
;; ------------------------------------------------------------------------------------------------

(defn ensure-state [m]
  (reduce #(if-not (get %1 %2) (assoc %1 %2 (if (= %2 :ts) [(sorted-map)] [])) %1)
	  m
	  (concat '(:ts :x) (keys @literals))))

(defn add-to-stack [state & [stack val & others]]
  (assert (even? (count others)) "Odd number of args supplied. Please specify stack-val pairs")
  (let [new-state (assoc state stack (conj (get state stack []) val))]
    (if others (recur new-state others) new-state)))

;; I'm keeping tag evaluation in core.clj, since this is, after all, a tag space machine
(defn match-tag [ts tag]
  (let [[_ default-val] (first ts)]
    (loop [ts-seq (seq ts)]
      (if-let [[[t v] & more] ts-seq]
	(if (>= t tag) v
	    (recur more))
	default-val))))

(defn eval-inst [state inst]
  (if-let [cmd (get @push/instruction-table inst)]
    (cmd state)
    (throw (Exception. (str "Unregistered instruction: " inst)))))

(defn eval-pair [{x :x, :as state} [p1 p2]]
  (assoc state :x (conj x p2 p1)))

(defn eval-ts [state {:keys [imap tag pop ith], :or {pop :pop, ith 1}}]
  (if-let [cmd (get @push/instruction-table imap)]
    (cmd state tag pop ith)
    state))

(defn eval-imap [state inst]
  (if-let [cmd (get @push/instruction-table (inst :imap))]
    (apply cmd (cons state (flatten (seq (dissoc inst :imap)))))
    state))

(defn eval-composite [state i]
  (cond (is-pair? i) (eval-pair state i)
	(is-ts? i) (eval-ts state i)
	(is-imap? i) (eval-imap state i)
	:else (throw (Exception. "Unrecognized composite"))))
   
(defn eval-x [{x :x, :as state}]
  (when @debug (println state))
  (let [i (last x)
	new-state (assoc state :x (pop x))
	stack (recognize-literal i)]
    (cond (is-composite? i) (eval-composite new-state i)
	  stack (add-to-stack new-state stack i)
	  (is-inst? i) (eval-inst new-state i)
	  :else (throw (Exception. (str "Unrecognized value on x stack: " i))))))

(defn eval-tsm [state]
  (let [s (ensure-state state)]
    (if (empty? (s :x)) s (recur (eval-x s)))))