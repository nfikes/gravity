(ns vector.core
  (:refer-clojure :exclude [vector + -]))

(defrecord Vector [x y])

(defn vector
  [x y]
  (->Vector x y))

(def zero (->Vector 0 0))

(defn +
  ([] zero)
  ([v1] v1)
  ([v1 v2]
   (->Vector (cljs.core/+ (.-x v1) (.-x v2))
             (cljs.core/+ (.-y v1) (.-y v2))))
  ([v1 v2 & more]
    (reduce + (+ v1 v2) more)))

(defn -
  ([v1] (->Vector (cljs.core/- (.-x v1)) (cljs.core/- (.-y v1))))
  ([v1 v2]
   (->Vector (cljs.core/- (.-x v1) (.-x v2))
             (cljs.core/- (.-y v1) (.-y v2))))
  ([v1 v2 & more]
   (reduce - (- v1 v2) more)))

(defn scale
  [k v]
  (->Vector (* k (.-x v)) (* k (.-y v))))

(defn length
  [v]
  (Math/sqrt (cljs.core/+ (* (.-x v) (.-x v))
                          (* (.-y v) (.-y v)))))
