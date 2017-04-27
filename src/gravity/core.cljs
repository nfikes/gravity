(ns gravity.core
  (:require
    [canvas.core :as c]))

(def spaceship
  {:pos   {:x 10
           :y 500}
   :vel   {:x 2
           :y 0}
   :color :red})

(def star1
  {:pos    {:x 600
            :y 600}
   :vel    {:x 0
            :y 1}
   :radius 100
   :color  :yellow})

(def star2
  {:pos    {:x 210
            :y 1300}
   :vel    {:x 0
            :y -1}
   :radius 100
   :color  :yellow})

(def controller
  {:box    {:x 40
            :y 800
            :w 190
            :h 190}
   :stick  {:x 0
            :y 0}
   :active false})

(defn square
  [x]
  (* x x))

;; Vector functions

(defn sum
  [a b]
  {:x (+ (:x a) (:x b))
   :y (+ (:y a) (:y b))})

(defn difference
  [a b]
  {:x (- (:x a) (:x b))
   :y (- (:y a) (:y b))})

(defn scale
  [k v]
  (-> v
      (update :x * k)
      (update :y * k)))

(defn length
  [v]
  (Math/sqrt (+ (square (:x v)) (square (:y v)))))

(defn limit
  [v r]
  (let [l (length v)]
    (if (< l r)
      v
      (scale (/ r l) v))))

(defn calc-distance
  [a b]
  (max 60 (length (difference b a))))

(defn calc-acceleration
  [a b]
  (let [distance (calc-distance a b)
        delta-x (- (:x a) (:x b))
        delta-y (- (:y a) (:y b))
        accel-strength (/ -500 (square distance))
        ratio (/ accel-strength distance)
        a-x (* ratio delta-x)
        a-y (* ratio delta-y)]
    {:x a-x
     :y a-y}))

(defn collide?
  [spaceship stars]
  false

  #_(some (fn [star]
          (let [size (:radius star)]
            (< (length (difference star spaceship)) size)))
        stars))

(declare add-accelerations)

(defn center-of-controller
  [controller]
  {:x (+ (-> controller :box :x) (/ (-> controller :box :w) 2))
   :y (+ (-> controller :box :y) (/ (-> controller :box :h) 2))})

(defn update-spaceship
  [spaceship stars controller]
  (let [accelerations (map (fn [star]
                             (calc-acceleration (merge spaceship
                                                       {:pos {:x (+ (-> spaceship :pos :x) (/ (-> spaceship :vel :x) 2))
                                                              :y (+ (-> spaceship :pos :y) (/ (-> spaceship :vel :y) 2))}})
                                                star))
                           stars)
        thrust (scale 0.005 (:stick controller))
        acceleration (-> (add-accelerations accelerations)
                         (update :x + (:x thrust))
                         (update :y + (:y thrust)))]
    {:pos   {:x (mod (+ (-> spaceship :pos :x) (-> spaceship :vel :x) (/ (:x acceleration) 2)) 768)
             :y (mod (+ (-> spaceship :pos :y) (-> spaceship :vel :y) (/ (:y acceleration) 2)) 1024)}
     :vel   {:x (+ (-> spaceship :vel :x) (:x acceleration))
             :y (+ (-> spaceship :vel :y) (:y acceleration))}
     :color (:color spaceship)
     :test  (:test spaceship)}))

(defn add-accelerations
  [accelerations]
  (reduce (fn [acc acceleration]
            {:x (+ (:x acc) (:x acceleration))
             :y (+ (:y acc) (:y acceleration))})
          {:x 0
           :y 0}
          accelerations))

(defn weaken
  [acceleration]
  {:x (/ (:x acceleration) 1000)
   :y (/ (:y acceleration) 1000)})

(defn update-star
  [star spaceships other-stars]
  (let [accelerations-spaceships (map (fn [spaceship]
                                        (calc-acceleration star spaceship))
                                      spaceships)
        acceleration-spaceships (weaken (add-accelerations accelerations-spaceships))
        accelerations-stars (map (fn [other-star]
                                   (calc-acceleration star other-star))
                                 other-stars)
        acceleration-stars (add-accelerations accelerations-stars)
        acceleration (add-accelerations [acceleration-spaceships acceleration-stars])]
    (assoc star :color (case (:color star)
                         :yellow :light-blue
                         :light-blue :yellow)
                :pos {:x (mod (+ (-> star :pos :x) (-> star :vel :x) (/ (:x acceleration) 2)) 768)
                      :y (mod (+ (-> star :pos :y) (-> star :vel :y) (/ (:y acceleration) 2)) 1024)}
                :vel {:x (+ (-> star :vel :x) (:x acceleration))
                      :y (+ (-> star :vel :y) (:y acceleration))}
                :radius (min 43 (max 42 (+ (:radius star) (- (rand-int 3) 1)))))))


(defn update-universe
  [universe]
  (-> universe
      (update :spaceships (fn [spaceships]
                            (map (fn [spaceship]
                                   (update-spaceship spaceship (:stars universe) (:controller universe)))
                                 (remove (fn [spaceship] (collide? spaceship (:stars universe))) spaceships))))
      (update :stars (fn [stars]
                       (map (fn [star]
                              (update-star star (:spaceships universe) (remove #{star} (:stars universe))))
                            stars)))
      (update :controller (fn [controller]
                            (cond-> controller
                                    (not (:active controller))
                                    (assoc :stick {:x 0 :y 0}))))))


(def initial-state {:spaceships [spaceship]
                    :stars      [star1 star2]
                    :controller controller})

(defonce universe (atom initial-state))

(defn reset
  []
  (reset! universe initial-state))

(defn in-controller?
  [controller pos]
  (let [box (:box controller)]
    (and (< (:x box) (:x pos) (+ (:x box) (:w box)))
         (< (:y box) (:y pos) (+ (:y box) (:h box))))))

(defn controller-limit
  [controller]
  (/ (:w (:box controller)) 2))

(defn touch-pos->stick
  [pos controller]
  (limit (difference pos (center-of-controller controller))
         (controller-limit controller)))

(defn touch-start
  [pos]
  (swap! universe
         (fn [{:keys [controller] :as u}]
           (cond
             (in-controller? controller pos)
             (assoc u :controller (-> controller
                                      (assoc :active true)
                                      (assoc :stick (touch-pos->stick pos controller))))
             :else
             u))))

(c/register-touch-start #(touch-start %))

(defn touch-move
  [pos]
  (swap! universe
         (fn [{:keys [controller] :as u}]
           (cond
             (:active controller)
             (assoc-in u [:controller :stick] (touch-pos->stick pos controller))
             :else
             u))))

(c/register-touch-move #(touch-move %))

(defn touch-end
  [pos]
  (swap! universe
         (fn [{:keys [controller] :as u}]
           (cond
             (:active controller)
             (assoc-in u [:controller :active] false)
             :else
             u))))

(c/register-touch-end #(touch-end %))

(defn update-state
  []
  (swap! universe update-universe))

(defn draw-controller
  []
  (let [{:keys [box stick]} (:controller @universe)]
    (c/stroke-style "#B0E0E6")
    (c/stroke-rect (:x box) (:y box) (:w box) (:h box))
    (c/stroke-rect (+ (:x box) 5) (+ (:y box) 5) (- (:w box) 10) (- (:h box) 10))
    (c/stroke-circle (+ (:x box) (/ (:w box) 2) (:x stick))
                     (+ (:y box) (/ (:h box) 2) (:y stick)) 30)))

(defn draw-state
  []
  (c/clear-rect 0 0 768 1024)
  (c/stroke-style "#ff0000")
  (doseq [spaceship (:spaceships @universe)]
    (c/stroke-rect (-> spaceship :pos :x) (-> spaceship :pos :y) 20 20))
  (doseq [star (:stars @universe)]
    (c/stroke-style "#ffff00")
    (c/stroke-circle (-> star :pos :x) (-> star :pos :y) (:radius star)))
  (c/fill-style "#B0E0E6")
  (c/fill-rect 0 0 768 24)
  (draw-controller))


(defonce interval-id-atom (atom nil))

(defn run
  ([] (run 30))
  ([fps]
   (swap! interval-id-atom
          (fn [interval-id]
            (when interval-id
              (.clearInterval js/window interval-id))
            (.setInterval js/window (fn []
                                      (update-state)
                                      (draw-state))
                          (/ 1000 fps))))))

(defn stop
  []
  (swap! interval-id-atom
         (fn [interval-id]
           (when interval-id
             (.clearInterval js/window interval-id))
           nil)))
