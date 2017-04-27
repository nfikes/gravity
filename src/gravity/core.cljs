(ns gravity.core
  (:require
    [canvas.core :as c]
    [vector.core :as v]))

(def spaceship
  {:pos   (v/vector 10 300)
   :vel   (v/vector 2 0)
   :color :red})

(def star1
  {:pos    (v/vector 300 600)
   :vel    v/zero
   :radius 100
   :color  :yellow})

(def controller
  {:box    {:pos (v/vector 40 800)
            :size (v/vector 190 190)}
   :stick  v/zero
   :active false})

(defn square
  [x]
  (* x x))

(defn limit
  [v r]
  (let [l (v/length v)]
    (if (< l r)
      v
      (v/scale (/ r l) v))))

(defn calc-distance
  [a b]
  (max 60 (v/length (v/- b a))))

(defn calc-acceleration
  [a b]
  (let [distance (calc-distance (:pos a) (:pos b))
        accel-strength (/ -1000 (square distance))
        ratio (/ accel-strength distance)]
    (v/scale ratio (v/- (:pos a) (:pos b)))))

(defn collide?
  [spaceship stars]
  (some (fn [star]
          (let [size (:radius star)]
            (< (v/length (v/- (:pos star) (:pos spaceship))) size)))
        stars))

(defn center-of-controller
  [controller]
  (v/+ (-> controller :box :pos) (v/scale 0.5 (-> controller :box :size))))

(defn update-spaceship
  [spaceship stars controller]
  (let [accelerations (map (fn [star]
                             (calc-acceleration (merge spaceship
                                                       {:pos (v/+ (:pos spaceship) (v/scale 0.5 (:vel spaceship)))})
                                                star))
                           stars)
        thrust (v/scale 0.001 (:stick controller))
        acceleration (-> (apply v/+ accelerations)
                         (v/+ thrust))]
    {:pos   (v/mod (v/+ (:pos spaceship) (:vel spaceship) (v/scale 0.5 acceleration)) (v/vector 768 1024))
     :vel   (limit (v/+ (:vel spaceship) acceleration) 3)
     :color (:color spaceship)
     :test  (:test spaceship)}))

(defn weaken
  [acceleration]
  (v/scale 0.001 acceleration))

(defn update-star
  [star spaceships other-stars]
  (let [accelerations-spaceships (map (fn [spaceship]
                                        (calc-acceleration star spaceship))
                                      spaceships)
        acceleration-spaceships (weaken (apply v/+ accelerations-spaceships))
        accelerations-stars (map (fn [other-star]
                                   (calc-acceleration star other-star))
                                 other-stars)
        acceleration-stars (apply v/+ accelerations-stars)
        acceleration (v/+ acceleration-spaceships acceleration-stars)]
    (assoc star :color (case (:color star)
                         :yellow :light-blue
                         :light-blue :yellow)
                :pos (v/mod (v/+ (:pos star) (:vel star) (v/scale 0.5 acceleration)) (v/vector 768 1024))
                :vel (limit (v/+ (:vel star) acceleration) 2)
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
                                    (assoc :stick v/zero))))))


(def initial-state {:spaceships [spaceship]
                    :stars      [star1]
                    :controller controller})

(defonce universe (atom initial-state))

(defn reset
  []
  (reset! universe initial-state))

(defn in-controller?
  [controller pos]
  (let [box (:box controller)]
    (and (< (:x (:pos box)) (:x pos) (+ (:x (:pos box)) (:x (:size box))))
         (< (:y (:pos box)) (:y pos) (+ (:y (:pos box)) (:y (:size box)))))))

(defn controller-limit
  [controller]
  (/ (:x (:size (:box controller))) 2))

(defn touch-pos->stick
  [pos controller]
  (limit (v/- pos (center-of-controller controller))
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
  (let [{:keys [box stick] :as controller} (:controller @universe)]
    (c/stroke-style "#B0E0E6")
    (c/stroke-rect (:pos box) (:size box))
    #_(c/stroke-rect (+ (:x box) 5) (+ (:y box) 5) (- (:w box) 10) (- (:h box) 10))
    (c/stroke-circle (v/+ (center-of-controller controller) (:stick controller)) 30)))

(defn draw-state
  []
  (c/clear-rect v/zero (v/vector 768 1024))
  (c/stroke-style "#ff0000")
  (doseq [spaceship (:spaceships @universe)]
    (c/stroke-rect (v/- (:pos spaceship) (v/vector 10 10)) (v/vector 20 20)))
  (doseq [star (:stars @universe)]
    (c/stroke-style "#ffff00")
    (c/stroke-circle (:pos star) (:radius star)))
  (c/fill-style "#B0E0E6")
  (c/fill-rect v/zero (v/vector 768 24))
  (draw-controller))


(defonce interval-id-atom (atom nil))

(defn run
  ([] (run 60))
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
