(ns tsm.instructions.float
  (:use [tsm core util instructions]))

;; temporary instructions due to incompatabilities with clojush state implementation

(define-registered float_add
  (fn [{floatstack :float, :as state}]
    (if (> (count floatstack) 1)
      (let [[stack [x y]] (vec-split floatstack -2)]
	(add-to-stack (assoc state :float stack) :float (+ x y)))
      state)))

(define-registered float_sub
  (fn [{floatstack :float, :as state}]
    (if (> (count floatstack) 1)
      (let [[stack [x y]] (vec-split floatstack -2)]
	(add-to-stack (assoc state :float stack) :float (- x y)))
      state)))

(define-registered float_mult
  (fn [{floatstack :float, :as state}]
    (if (> (count floatstack) 1)
      (let [[stack [x y]] (vec-split floatstack -2)]
	(add-to-stack (assoc state :float stack) :float (* x y)))
      state)))

(define-registered float_divide
  (fn [{floatstack :float , :as state}]
    (if (> (count floatstack) 1)
      (let [[stack [x y]] (vec-split floatstack -2)]
	(add-to-stack (assoc state :float stack) :float (/ x y)))
      state)))

(define-registered float_pop
  (fn [{floatstack :float, :as state}]
    (if (> (count floatstack) 0)
      (assoc state :float (pop floatstack))
      state)))

(define-registered float_dup
  (fn [{floatstack :float, :as state}]
    (if (> (count floatstack) 0)
      (add-to-stack state :float (last floatstack))
      state)))

(define-registered float_swap
  (fn [{floatstack :float, :as state}]
    (if (> (count floatstack) 1)
      (let [[more [x1 x2]] (vec-split floatstack -2)]
	(add-to-stack (assoc state :float more) :float x2 :float x1))
      state)))

(define-registered float_rot
  (fn [{floatstack :float, :as state}]
    (if (> (count floatstack) 2)
      (let [[more [x1 x2 x3]] (vec-split floatstack -3)]
	(add-to-stack (assoc state :float more) :float x2 :float x3 :float x1))
      state)))

(define-registered float_eq
  (fn [{floatstack :float, :as state}]
    (if (> (count floatstack) 1)
      (let [[more [x1 x2]] (vec-split floatstack -2)]
	(add-to-stack (assoc state :float more) :boolean (= x1 x2)))
      state)))
		      
(define-registered float_lt
  (fn [{floatstack :float, :as state}]
    (if (> (count floatstack) 1)
      (let [[more [x1 x2]] (vec-split floatstack -2)]
	(add-to-stack (assoc state :float more) :boolean (< x2 x1)))
      state)))

(define-registered float_gt
  (fn [{floatstack :float, :as state}]
    (if (> (count floatstack) 1)
      (let [[more [x1 x2]] (vec-split floatstack -2)]
	(add-to-stack (assoc state :float more) :boolean (> x2 x1))))))

(define-registered float_sin
  (fn [{floatstack :float, :as state}]
    (if (> (count floatstack) 0)
      (let [[more [x]] (vec-split floatstack -1)]
	(add-to-stack (assoc state :float more) :float (Math/sin x)))
      state)))

(define-registered float_cos
  (fn [{floatstack :float, :as state}]
    (if (> (count floatstack) 0)
      (let [[more [x]] (vec-split floatstack -1)]
	(add-to-stack (assoc state :float more) :float (Math/cos x)))
      state)))

(define-registered float_tan
  (fn [{floatstack :float, :as state}]
    (if (> (count floatstack) 0)
      (let [[more [x]] (vec-split floatstack -1)]
	(add-to-stack (assoc state :float more) :Float (Math/tan x)))
      state)))