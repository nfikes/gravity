(ns gravity.canvas)

(def TWO-PI (* 2 Math/PI))

(defn get-context
  []
  (let [canvas (.getElementById js/document "canvas")]
    (.getContext canvas "2d")))

(defonce ctx (get-context))

(defn fill-style
  [fs]
  (set! (.-fillStyle ctx) fs))

(defn clear-rect [x y w h]
  (.clearRect ctx x y w h))

(defn fill-rect
  [x y w h]
  (.fillRect ctx x y w h))

(defn fill-circle
  [x y radius]
  (.beginPath ctx)
  (.arc ctx x y radius 0 TWO-PI)
  (.fill ctx))

(defn register-touch-begin
  [f]
  )

(defn register-touch-move
  [f]
  )

(defonce touch-end-listener-atom (atom nil))

(defn register-touch-end
  [f]
  (swap! touch-end-listener-atom
         (fn [old-listener]
           (when old-listener
             (.removeEventListener "touchend" old-listener))
           (let [listener (fn [ev]
                            (let [t (aget (.-changedTouches ev) 0)]
                              (f (.-pageX t) (.-pageY t))))]
             (.addEventListener js/document "touchend" listener false)
             listener))))
