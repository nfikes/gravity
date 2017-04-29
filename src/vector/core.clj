(ns vector.core
  (:refer-clojure :exclude [+ - mod]))

(defmacro +
  ([] 'vector.core/zero)
  ([v1] v1)
  ([v1 v2]
   (if (and (symbol? v1)
            (symbol? v2))
     `(->Vector (cljs.core/+ (.-x ~v1) (.-x ~v2))
                (cljs.core/+ (.-y ~v1) (.-y ~v2)))
     `(let [a# ~v1
            b# ~v2]
        (->Vector (cljs.core/+ (.-x a#) (.-x b#))
                  (cljs.core/+ (.-y a#) (.-y b#))))))
  ([v1 v2 & more]
   `(+ (+ ~v1 ~v2) ~@more)))

(defmacro -
  ([v1]
   (if (symbol? v1)
     `(->Vector (cljs.core/- (.-x ~v1))
                (cljs.core/- (.-y ~v1)))
     `(let [a# ~v1]
        (->Vector (cljs.core/- (.-x a#))
                  (cljs.core/- (.-y a#))))))
  ([v1 v2]
   (if (and (symbol? v1)
            (symbol? v2))
     `(->Vector (cljs.core/- (.-x ~v1) (.-x ~v2))
                (cljs.core/- (.-y ~v1) (.-y ~v2)))
     `(let [a# ~v1
            b# ~v2]
        (->Vector (cljs.core/- (.-x a#) (.-x b#))
                  (cljs.core/- (.-y a#) (.-y b#))))))
  ([v1 v2 & more]
   `(- (- ~v1 ~v2) ~@more)))

(defmacro scale
  [k v]
  (if (and (or (number? k)
               (symbol? k))
           (symbol? v))
    `(->Vector (* ~k (.-x ~v)) (* ~k (.-y ~v)))
    `(let [k# ~k
           v# ~v]
       (->Vector (* k# (.-x v#)) (* k# (.-y v#))))))

(defmacro length
  [v]
  (if (symbol? v)
    `(~'Math/sqrt (cljs.core/+ (* (.-x ~v) (.-x ~v))
                               (* (.-y ~v) (.-y ~v))))
    `(let [v# ~v]
       (~'Math/sqrt (cljs.core/+ (* (.-x v#) (.-x v#))
                                 (* (.-y v#) (.-y v#)))))))

(defmacro mod
  [v1 v2]
  `(let [v1# ~v1
         v2# ~v2]
     (->Vector (cljs.core/mod (.-x v1#) (.-x v2#))
               (cljs.core/mod (.-y v1#) (.-y v2#)))))
