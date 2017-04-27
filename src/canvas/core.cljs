(ns canvas.core)

(def TWO-PI (* 2 Math/PI))

(defn get-context
  []
  (let [canvas (.getElementById js/document "canvas")]
    (.getContext canvas "2d")))

(defonce ctx (get-context))

(defn fill-style
  [fs]
  (set! (.-fillStyle ctx) fs))

(defn stroke-style
  [ss]
  (set! (.-strokeStyle ctx) ss))

(defn clear-rect [x y w h]
  (.clearRect ctx x y w h))

(defn fill-rect
  [x y w h]
  (.fillRect ctx x y w h))

(defn stroke-rect
  [x y w h]
  (.strokeRect ctx x y w h))

(defn fill-circle
  [x y radius]
  (.beginPath ctx)
  (.arc ctx x y radius 0 TWO-PI)
  (.fill ctx))

(defn stroke-circle
  [x y radius]
  (.beginPath ctx)
  (.arc ctx x y radius 0 TWO-PI)
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
      (f {:x (.-pageX t) :y (.-pageY t)}))))

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
