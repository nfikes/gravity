(ns gravity.core
  (:require
    [canvas.core :as c]))

(def spaceship
  {:x     300
   :y     500
   :v-x   6
   :v-y   0
   :color :red})

(def star1
  {:x     600
   :y     600
   :v-x   0
   :v-y   1
   :radius 100
   :color :yellow})


(def star2
  {:x     210
   :y     1300
   :v-x   0
   :v-y   -1
   :radius 100
   :color :yellow})

(def controller
  {:box {:x 40
         :y 700
         :w 200
         :h 200}
   :stick {:x 140
           :y 800}
   :active false})

(defn square
  [x]
  (* x x))

(defn length
  [a-x a-y b-x b-y]
  (Math/sqrt (+ (square (- a-x b-x))
                (square (- a-y b-y)))))

(defn calc-distance
  [a b]
  (max 60 (length (:x a) (:y a) (:x b) (:y b))))

(defn calc-acceleration
  [a b]
  (let [distance (calc-distance a b)
        delta-x (- (:x a) (:x b))
        delta-y (- (:y a) (:y b))
        accel-strength (/ -1000 (square distance))
        ratio (/ accel-strength distance)
        a-x (* ratio delta-x)
        a-y (* ratio delta-y)]
    {:a-x a-x
     :a-y a-y}))

(defn collide?
  [spaceship stars]
  (some (fn [star]
          (let [size (:radius star)]
            (< (length (:x star) (:y star) (:x spaceship) (:y spaceship)) size)))
        stars))

(declare add-accelerations)

(defn update-spaceship
  [spaceship stars]
  (let [accelerations (map (fn [star]
                             (calc-acceleration (merge spaceship
                                                       {:x (+ (:x spaceship) (/ (:v-x spaceship) 2))
                                                        :y (+ (:y spaceship) (/ (:v-y spaceship) 2))})
                                                star))
                           stars)
        acceleration (add-accelerations accelerations)]
    {:x     (mod (+ (:x spaceship) (:v-x spaceship) (/ (:a-x acceleration) 2)) 768)
     :y     (mod (+ (:y spaceship) (:v-y spaceship) (/ (:a-y acceleration) 2)) 1024)
     :v-x   (+ (:v-x spaceship) (:a-x acceleration))
     :v-y   (+ (:v-y spaceship) (:a-y acceleration))
     :color (:color spaceship)
     :test  (:test spaceship)}))

(defn add-accelerations
  [accelerations]
  (reduce (fn [acc acceleration]
            {:a-x (+ (:a-x acc) (:a-x acceleration))
             :a-y (+ (:a-y acc) (:a-y acceleration))})
          {:a-x 0
           :a-y 0}
          accelerations))

(defn weaken
  [acceleration]
  {:a-x (/ (:a-x acceleration) 100)
   :a-y (/ (:a-y acceleration) 100)})

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
                :x (mod (+ (:x star) (:v-x star) (/ (:a-x acceleration) 2)) 768)
                :y (mod (+ (:y star) (:v-y star) (/ (:a-y acceleration) 2)) 1024)
                :v-x (+ (:v-x star) (:a-x acceleration))
                :v-y (+ (:v-y star) (:a-y acceleration))
                :radius (min 43 (max 42 (+ (:radius star) (- (rand-int 3) 1)))))))

(defn update-universe
  [universe]
  (-> universe
      (update :spaceships (fn [spaceships]
                            (map (fn [spaceship]
                                   (update-spaceship spaceship (:stars universe)))
                                 (remove (fn [spaceship] (collide? spaceship (:stars universe))) spaceships))))
      (update :stars (fn [stars]
                       (map (fn [star]
                              (update-star star (:spaceships universe) (remove #{star} (:stars universe))))
                            stars)))
      (update :controller (fn [controller]
                            (cond-> controller
                                    (not (:active controller))
                                    (assoc :stick {:x (+ (-> controller :box :x) (/ (-> controller :box :w) 2))
                                                   :y (+ (-> controller :box :y) (/ (-> controller :box :h) 2))}))))))


(def initial-state {:spaceships (map (fn [n]
                                       (update spaceship :x + -500 (* 100 n)))
                                     (range 7))
                    :stars      [star1 star2]
                    :controller controller})

(defonce universe (atom initial-state))

(defonce older-universes (atom ()))

(defn reset
  []
  (reset! universe initial-state)
  (reset! older-universes ()))

(defn in-controller?
  [controller x y]
  (let [box (:box controller)]
    (and (< (:x box) x (+ (:x box) (:w box)))
         (< (:y box) y (+ (:y box) (:h box))))))

(defn touch-start
  [x y]
  (swap! universe
         (fn [{:keys [controller] :as u}]
           (cond
             (in-controller? controller x y)
             (assoc u :controller (-> controller
                                      (assoc :active true)
                                      (assoc :stick {:x x :y y})))
             :else
             u))))

(c/register-touch-start #(touch-start %1 %2))

(defn touch-move
  [x y]
  (swap! universe
         (fn [{:keys [controller] :as u}]
           (cond
             (:active controller)
             (cond-> u
                (in-controller? controller x y)
                     (assoc-in [:controller :stick] {:x x :y y}))
             :else
             u))))

(c/register-touch-move #(touch-move %1 %2))

(defn touch-end
  [x y]
  (swap! universe
         (fn [{:keys [controller] :as u}]
           (cond
             (:active controller)
             (assoc-in u [:controller :active] false)
             :else
             u))))

(c/register-touch-end #(touch-end %1 %2))

(defn update-state
  []
  (swap! older-universes conj @universe)
  (swap! older-universes #(take 10 %))
  (swap! universe update-universe))

(defn draw-controller
  []
  (let [{:keys [box stick]} (:controller @universe)]
    (c/stroke-style "#B0E0E6")
    (c/stroke-rect (:x box) (:y box) (:w box) (:h box))
    (c/stroke-circle (:x stick) (:y stick) 30)))

(defn draw-state
  []
  (c/clear-rect 0 0 768 1024)
  (let [ndx (atom 0)]
    (doseq [older-universe (reverse @older-universes)]
      (doseq [spaceship (:spaceships older-universe)]
        (let [stroke-styles ["#000000" "#110000" "#220000" "#330000"
                           "#440000" "#550000" "#660000" "#770000"
                           "#880000" "#990000" "#aa0000" "#bb0000"
                           "#cc0000" "#dd0000" "#ee0000" "#ff0000"]
              rect-size (- 20 (/ @ndx 2))]
          (c/stroke-style (stroke-styles @ndx))
          (c/stroke-rect (:x spaceship) (:y spaceship) rect-size rect-size)))
      (swap! ndx inc)))
  (doseq [star (:stars @universe)]
    (c/stroke-style "#ffff00")
    (c/stroke-circle (:x star) (:y star) (:radius star)))
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
