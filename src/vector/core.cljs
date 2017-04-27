(ns vector.core
  (:refer-clojure :exclude [vector + - mod])
  (:require-macros
    [vector.core]))

(defrecord Vector [x y])

(defn vector
  [x y]
  (->Vector x y))

(def zero (->Vector 0 0))

(defn +
  ([] zero)
  ([v1] v1)
  ([v1 v2]
    (vector.core/+ v1 v2))
  ([v1 v2 & more]
    (reduce + (+ v1 v2) more)))

(defn -
  ([v1]
    (vector.core/- v1))
  ([v1 v2]
   (vector.core/- v1 v2))
  ([v1 v2 & more]
   (reduce - (- v1 v2) more)))

(defn scale
  [k v]
  (vector.core/scale k v))

(defn length
  [v]
  (vector.core/length v))

(defn mod
  [v1 v2]
  (vector.core/mod v1 v2))
