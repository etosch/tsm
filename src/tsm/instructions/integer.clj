(ns tsm.instructions.integer
  (:use [tsm core util config instructions])
  (:require [clojush.random :as pushrand]))

(define-registered integer_add
  (fn [{intstack :integer, :as state}]
    (if (> (count intstack) 1)
      (let [[stack [x y]] (vec-split intstack -2)]
	(add-to-stack (assoc state :integer stack) :integer (+ x y)))
      state)))

(define-registered integer_mult
  (fn [{intstack :integer, :as state}]
    (if (> (count intstack) 1)
      (let [[stack [x y]] (vec-split intstack -2)]
	(add-to-stack (assoc state :integer stack) :integer (* x y)))
      state)))

(define-registered int_sub
  (fn [{intstack :integer, :as state}]
    (if (> (count intstack) 1)
      (let [[stack [x y]] (vec-split intstack -2)]
	(add-to-stack (assoc state :integer stack) :integer (- x y)))
      state)))
(define-registered integer_divide
  (fn [{integerstack :integer , :as state}]
    (if (> (count integerstack) 1)
      (let [[stack [x y]] (vec-split integerstack -2)]
	(add-to-stack (assoc state :integer stack) :integer (/ x y)))
      state)))

(define-registered integer_pop
  (fn [{integerstack :integer, :as state}]
    (if (> (count integerstack) 0)
      (assoc state :integer (pop integerstack))
      state)))

(define-registered integer_dup
  (fn [{integerstack :integer, :as state}]
    (if (> (count integerstack) 0)
      (add-to-stack state :integer (last integerstack))
      state)))

(define-registered integer_swap
  (fn [{integerstack :integer, :as state}]
    (if (> (count integerstack) 1)
      (let [[more [x1 x2]] (vec-split integerstack -2)]
	(add-to-stack (assoc state :integer more) :integer x2 :integer x1))
      state)))

(define-registered integer_rot
  (fn [{integerstack :integer, :as state}]
    (if (> (count integerstack) 2)
      (let [[more [x1 x2 x3]] (vec-split integerstack -3)]
	(add-to-stack (assoc state :integer more) :integer x2 :integer x3 :integer x1))
      state)))

(define-registered integer_eq
  (fn [{integerstack :integer, :as state}]
    (if (> (count integerstack) 1)
      (let [[more [x1 x2]] (vec-split integerstack -2)]
	(add-to-stack (assoc state :integer more) :boolean (= x1 x2)))
      state)))
		      
(define-registered integer_lt
  (fn [{integerstack :integer, :as state}]
    (if (> (count integerstack) 1)
      (let [[more [x1 x2]] (vec-split integerstack -2)]
	(add-to-stack (assoc state :integer more) :boolean (< x2 x1)))
      state)))

(define-registered integer_gt
  (fn [{integerstack :integer, :as state}]
    (if (> (count integerstack) 1)
      (let [[more [x1 x2]] (vec-split integerstack -2)]
	(add-to-stack (assoc state :integer more) :boolean (> x2 x1))))))

;; if part of an imap, then there will be a constant
;; if there is not an imap, then create one and push on the execution stack
(define-registered integer_add_const
  (with-meta  
    (fn [{intstack :integer, :as state} & {const :const}]
      (if-let [[stack [x]] (vec-split intstack -1)]
	(if const
	  (add-to-stack (assoc state :integer stack) :integer (+ x const))
	  (add-to-stack (assoc state :integer stack) :x {:imap 'integer_add_const :const x}))
	state))
    {:const (fn [] (pushrand/lrand-int @max-int))}))
