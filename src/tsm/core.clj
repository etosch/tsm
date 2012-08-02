(ns tsm.core
  (:require [clojure.string :as s]
	    [clojush.pushstate :as push]))

;; vector splitting util
(defn vec-split
  "Returns a vector of two vectors. If the pivot is 0, the first item is the full vector. If the pivot is greater than the size of the vector, the second item is the full vector."
  [v pivot]
  (cond (> (if (neg? pivot) (- pivot) pivot) (count v)) [[] v]
	(pos? pivot) [(subvec v 0 pivot) (subvec v pivot)]
	:else [(subvec v 0 (+ (count v) pivot)) (subvec v (+ (count v) pivot))]))

;; things you might want to configure:
(def literals
     (atom {:string string?
	    :boolean (fn [thing] (or (= false thing) (= true thing)))
	    :float float?
	    :integer integer?}))

(def debug (atom true))

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

(defn is-state? [m]
  (and (m :x) (m :ts)
       (reduce #(and %1 %2) (map #(get m %) (keys @literals)))))

(defn ensure-state [m]
  (reduce #(if-not (get %1 %2) (assoc %1 %2 []) %1)
	  m
	  (concat '(:ts :x) (keys @literals))))

(defn add-to-stack [state & [stack val & others]]
  (assert (even? (count others)) "Odd number of args supplied. Please specify stack-val pairs")
  (let [new-state (assoc state stack (conj (get state stack []) val))]
    (if others (recur new-state others) new-state)))

(defn match-tag [ts tag]
  (let [sorted-ts (sort #(< (%1 0) (%2 0)) ts)
	[_ default-val] (first sorted-ts)]
    (loop [tsp (seq sorted-ts)]
      (if-let [[[t v] & more] tsp]
	(if (>= t tag) v
	    (recur more))
	default-val))))

(defn eval-inst [state inst]
  (if-let [cmd (get @push/instruction-table inst)]
    (cmd state)
    state))

(defn eval-pair [{x :x, :as state} [p1 p2]]
  (assoc state :x (conj x p2 p1)))

(defn eval-ts [state inst]
  (if-let [cmd (get @push/instruction-table (inst :imap))]
    (apply cmd (cons state (vals (dissoc inst :imap))))
    state))

(defn eval-composite [state i]
  (cond (is-pair? i) (eval-pair state i)
	(is-ts? i) (eval-ts state i)
	:else (throw (Exception. "Unrecognized composite"))))
   
(defn eval-x [{x :x, :as state}]
  (when @debug (println state))
  (let [i (last x)
	new-state (assoc state :x (pop x))]
    (if (is-composite? i)
      (eval-composite new-state i)
      (if-let [stack (recognize-literal i)]
	(add-to-stack new-state stack i)
	(eval-inst new-state i)))))

(defn eval-tsm [state & {direction :direction}]
  (let [s (cond (or (nil? direction) (= direction :rl)) state
		(= direction :lr) (zipmap (keys state) (map #(vec (reverse %)) (vals state)))
		:else (throw (Exception. "Unrecognized direction. Please choose from :rl, :lr, or nil")))]
    (if (empty? (s :x)) s (recur (eval-x s) nil))))


(push/define-registered ts_pop
  (fn [state]
    (if-not (empty? (state :ts))
      (assoc state :ts (pop (get state :ts)))
      state)))

;; default behavior is pop -- I suppose a nopop could be implemented with "baked-in" args at a later point
(push/define-registered ts_tag
  (fn [{x :x, ts :ts, :as state} tag & {pop? :pop}]
    (if-let [tagged-cmd (last x)]
      (let [[rest-ts [current-ts & _]] (vec-split ts -1)]
	(assoc (if (= pop? :nopop)
		 state
		 (assoc state :x (pop x)))
	  :ts
	  (conj rest-ts (assoc current-ts tag tagged-cmd)))))))

(push/define-registered ts_tag_pair
  (fn [{x :x, ts :ts, :as state} tag & {pop? :pop}]
    (if (> (count x) 1)
      (let [[p1 p2 & _] x
	    [rest-ts [current-ts & _]] (vec-split ts -1)]
	(assoc (if (= pop? :nopop)
		 state
		 (assoc state :x (pop (pop x))))
	  :ts
	  (conj rest-ts (assoc current-ts tag [p1 p2])))))))

;; what happens if the tag space is empty?
(push/define-registered ts_tagged
  (fn [{x :x, ts :ts, :as state} tag & {pop? :pop}]
    (assoc state :x (conj x (match-tag (first ts) tag)))))

(push/define-registered ts_tagged_under
  (fn [{x :x, ts :ts, :as state} tag & {pop? :pop}]
    (assoc state :x (conj (pop x) (match-tag (first ts) tag) (last x)))))

(push/define-registered ts_new
  (fn [{ts :ts, :as state}]
    (assoc state :ts (conj ts {}))))

;; still need to define the 2-versions of the tag spaces - would suggest having this as an argument
;; in the imap, since the imap instructions just apply the function to the appropriate arguments.
;; the ts instructions above would then just need to pick out the relevent tag space
;; (if we're using arguments in the imap anyway, why not leverage this?)



;; temporary instructions due to incompatabilities with clojush state implementation
(push/define-registered integer_add
  (fn [{intstack :integer, :as state}]
    (if (> (count intstack) 1)
      (let [[stack [x y]] (vec-split intstack -2)]
	(add-to-stack (assoc state :integer stack) :integer (+ x y)))
      state)))

(push/define-registered integer_mult
  (fn [{intstack :integer, :as state}]
    (if (> (count intstack) 1)
      (let [[stack [x y]] (vec-split intstack -2)]
	(add-to-stack (assoc state :integer stack) :integer (* x y)))
      state)))

(push/define-registered float_add
  (fn [{floatstack :float, :as state}]
    (if (> (count floatstack) 1)
      (let [[stack [x y]] (vec-split floatstack -2)]
	(add-to-stack (assoc state :float stack) :float (+ x y))))))

(push/define-registered float_mult
  (fn [{floatstack :float, :as state}]
    (if (> (count floatstack) 1)
      (let [[stack [x y]] (vec-split floatstack -2)]
	(add-to-stack (assoc state :float stack) :float (* x y))))))

(push/define-registered x_dup
  (fn [{x :x, :as state}]
    (if-let [duped (last x)]
      (add-to-stack state :x duped)
      state)))

(push/define-registered x_swap
  (fn [{x :x, :as state}]
    (if (> (count x) 1)
      (let [[x1 x2 & more] x]
	(add-to-stack (assoc state :x more) :x x2 :x x1)))))

(push/define-registered x_rot
  (fn [{x :x, :as state}]
    (if (> (count x) 2)
      (let [[x1 x2 x3 & more] x]
	(add-to-stack (assoc state :x more) :x x3 :x x1 :x x2)))))

(push/define-registered x_if
  (fn [{x :x, boolean :boolean, :as state}]
    (if (and (> (count x) 1) (not (empty? boolean)))
      (let [[pred & boolstack] boolean
	    [consequent subsequent & xstack] x]
	(add-to-stack (assoc state :boolean boolstack :x xstack) :x (if pred consequent subsequent))))))