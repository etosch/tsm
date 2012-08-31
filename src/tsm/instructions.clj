(ns tsm.instructions)

;; Instruction management.
;; Copied from Clojush (with the exception that registered-instructions is a list and not a set)
(def registered-instructions (atom '()))

(defn register-instruction
  "Add the provided name to the global list of registered instructions."
  [name]
  (swap! registered-instructions conj name))

(def instruction-table (atom (hash-map)))

(defmacro define-registered
  [instruction definition]
  `(do (register-instruction '~instruction)
       (swap! instruction-table assoc '~instruction ~definition)))

(defn registered-for-type
  "Returns a list of all registered instructions with the given type name as a prefix."
  [type & {:keys [include-randoms] :or {include-randoms true}}]
  (let [for-type (filter #(.startsWith (name %) (name type)) @registered-instructions)]
    (if include-randoms
      for-type
            (filter #(not (.endsWith (name %) "_rand")) for-type))))