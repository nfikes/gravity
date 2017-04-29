(ns canvas.core
  (:require-macros
    [canvas.core])
  (:require
    [vector.core :as v]))

(def TWO-PI (* 2 Math/PI))

(defn get-context
  []
  (let [canvas (.getElementById js/document "canvas")]
    (.getContext canvas "2d")))

(defn fill-style
  [ctx fs]
  (set! (.-fillStyle ctx) fs))

(defn stroke-style
  [ctx ss]
  (set! (.-strokeStyle ctx) ss))

(defn clear-rect
  [ctx pos size]
  (.clearRect ctx (.-x pos) (.-y pos) (.-x size) (.-y size)))

(defn fill-rect
  [ctx pos size]
  (.fillRect ctx (.-x pos) (.-y pos) (.-x size) (.-y size)))

(defn stroke-rect
  [ctx pos size]
  (.strokeRect ctx (.-x pos) (.-y pos) (.-x size) (.-y size)))

(defn fill-circle
  [ctx pos radius]
  (.beginPath ctx)
  (.arc ctx (.-x pos) (.-y pos) radius 0 TWO-PI)
  (.fill ctx))

(defn stroke-circle
  [ctx pos radius]
  (.beginPath ctx)
  (.arc ctx (.-x pos) (.-y pos) radius 0 TWO-PI)
  (.stroke ctx))

(defn register-handler
  [event-name listener registered-handler-atom]
  (swap! registered-handler-atom
         (fn [old-listener]
           (when old-listener
             (.removeEventListener js/document event-name old-listener))
           (.addEventListener js/document event-name listener false)
           listener)))

(defn make-touch-handler
  [f]
  (fn [ev]
    (let [t (aget (.-changedTouches ev) 0)]
      (f (v/->Vector (.-pageX t) (.-pageY t))))))

(defonce touch-move-listener-atom (atom nil))

(defn register-touch-move
  [f]
  (register-handler "touchmove" (make-touch-handler f) touch-move-listener-atom))

(defonce touch-start-listener-atom (atom nil))

(defn register-touch-start
  [f]
  (register-handler "touchstart" (make-touch-handler f) touch-start-listener-atom))

(defonce touch-end-listener-atom (atom nil))

(defn register-touch-end
  [f]
  (register-handler "touchend" (make-touch-handler f) touch-end-listener-atom))
