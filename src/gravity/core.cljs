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


(def touch-event-queue (atom []))

(declare in-controller? touch-pos->stick)

(defn process-touch-start
  [universe pos]
  (let [controller (:controller universe)]
    (cond
      (in-controller? controller pos)
      (assoc universe :controller (-> controller
                               (assoc :active true)
                               (assoc :stick (touch-pos->stick pos controller))))
      :else
      universe)))

(defn process-touch-move
  [universe pos]
  (let [controller (:controller universe)]
    (cond
      (:active controller)
      (assoc-in universe [:controller :stick] (touch-pos->stick pos controller))
      :else
      universe)))

(defn process-touch-end
  [universe pos]
  (let [controller (:controller universe)]
    (cond
      (:active controller)
      (assoc-in universe [:controller :active] false)
      :else
      universe)))

(defn process-touch-event
  [universe [kind pos]]
  (case kind
    :start (process-touch-start universe pos)
    :move (process-touch-move universe pos)
    :end (process-touch-end universe pos)))

(defn process-touch-event-queue
  [universe]
  (let [touch-events @touch-event-queue]
    (reset! touch-event-queue [])
    (reduce
      process-touch-event
      universe
      touch-events)))

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
      (process-touch-event-queue)
      (update :controller (fn [controller]
                            (cond-> controller
                                    (not (:active controller))
                                    (assoc :stick v/zero))))))


(def initial-state {:ctx (c/get-context)
                    :spaceships [spaceship]
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
  (swap! touch-event-queue conj [:start pos]))

(c/register-touch-start #(touch-start %))

(defn touch-move
  [pos]
  (swap! touch-event-queue (fn [queue]
                             (let [debounced (if (= :move (first (peek queue)))
                                               (pop queue)
                                               queue)]
                               (conj debounced [:move pos])))))

(c/register-touch-move #(touch-move %))

(defn touch-end
  [pos]
  (swap! touch-event-queue conj [:end pos]))

(c/register-touch-end #(touch-end %))

(defn update-state
  []
  (swap! universe update-universe))

(defn clear-controller
  [ctx controller]
  (let [{:keys [box stick]} controller]
    (c/clear-rect ctx (:pos box) (:size box))
    (c/clear-rect ctx (v/+ (center-of-controller controller) (:stick controller) (v/- (v/vector 32 32)))
                  (v/vector 64 64))))

(defn draw-controller
  [ctx controller]
  (let [{:keys [box stick]} controller]
    (c/stroke-style ctx "#B0E0E6")
    (c/stroke-rect ctx (:pos box) (:size box))
    (c/stroke-rect ctx (v/+ (:pos box) (v/vector 5 5)) (v/- (:size box) (v/vector 10 10)))
    (c/stroke-circle ctx (v/+ (center-of-controller controller) (:stick controller)) 30)))

(defn clear-spaceship
  [ctx spaceship]
  (c/clear-rect ctx (v/- (:pos spaceship) (v/vector 11 11)) (v/vector 22 22)))

(defn draw-spaceship
  [ctx spaceship]
  (c/stroke-style ctx "#FF0000")
  (c/stroke-rect ctx (v/- (:pos spaceship) (v/vector 10 10)) (v/vector 20 20)))

(defn clear-star
  [ctx star]
  (let [rv (v/scale 1.2 (v/vector (:radius star) (:radius star)))]
    (c/clear-rect ctx (v/- (:pos star) rv) (v/scale 2 rv))))

(defn draw-star
  [ctx star]
  (c/stroke-style ctx "#ffff00")
  (c/stroke-circle ctx (:pos star) (:radius star)))

(defn draw-bar
  [ctx]
  (c/fill-style ctx "#B0E0E6")
  (c/fill-rect ctx v/zero (v/vector 768 24)))

(defn clear-state
  []
  (let [universe @universe
        ctx (:ctx universe)]
    #_(c/clear-rect ctx v/zero (v/vector 768 1024))
    (doseq [star (:stars universe)]
      (clear-star ctx star))
    (doseq [spaceship (:spaceships universe)]
      (clear-spaceship ctx spaceship))
    (clear-controller ctx (:controller universe))))

(defn draw-state
  []
  (let [universe @universe
        ctx (:ctx universe)]
    (doseq [spaceship (:spaceships universe)]
      (draw-spaceship ctx spaceship))
    (doseq [star (:stars universe)]
      (draw-star ctx star))
    (draw-controller ctx (:controller universe))
    (draw-bar ctx)))

(defonce interval-id-atom (atom nil))

(defn run
  ([] (run 60))
  ([fps]
   (swap! interval-id-atom
          (fn [interval-id]
            (when interval-id
              (.clearInterval js/window interval-id))
            (.setInterval js/window (fn []
                                      (clear-state)
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
