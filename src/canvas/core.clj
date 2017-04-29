(ns canvas.core)

(defmacro fill-style
  [ctx fs]
  `(set! (.-fillStyle ~ctx) ~fs))

(defmacro stroke-style
  [ctx ss]
  `(set! (.-strokeStyle ~ctx) ~ss))

(defmacro clear-rect
  [ctx pos size]
  `(let [pos# ~pos
         size# ~size]
     (.clearRect ~ctx (.-x pos#) (.-y pos#) (.-x size#) (.-y size#))))

(defmacro fill-rect
  [ctx pos size]
  `(let [pos# ~pos
         size# ~size]
     (.fillRect ~ctx (.-x pos#) (.-y pos#) (.-x size#) (.-y size#))))

(defmacro stroke-rect
  [ctx pos size]
  `(let [pos# ~pos
         size# ~size]
     (.strokeRect ~ctx (.-x pos#) (.-y pos#) (.-x size#) (.-y size#))))

(defmacro fill-circle
  [ctx pos radius]
  `(let [pos# ~pos]
     (.beginPath ~ctx)
     (.arc ~ctx (.-x pos#) (.-y pos#) ~radius 0 ~'canvas.core/TWO-PI)
     (.fill ~ctx)))

(defmacro stroke-circle
  [ctx pos radius]
  `(let [pos# ~pos]
     (.beginPath ~ctx)
     (.arc ~ctx (.-x pos#) (.-y pos#) ~radius 0 ~'canvas.core/TWO-PI)
     (.stroke ~ctx)))
