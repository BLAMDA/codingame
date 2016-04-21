(ns Player
  (:gen-class))

(def acc-gravity -3.711)
(def ^:const max-v-speed -40)
(def ^:const max-h-speed 20)
(def ^:const max-thrust 4)

(defn landing-zone
  [surface]
  (->>
    (partition-by second surface)
    (filter #(> (count %) 1))
    first))

(defn highest-point
  "Called with the subsection of the surface between the
  craft's current position and the nearest point on the
  landing zone, determine the highest altitude point."
  [surface]
  (->
    (sort-by second surface)
    first))

(defn distance
  [[pos-x pos-y] [target-x target-y]]
  [(Math/abs (- pos-x target-x))
   (Math/abs (- pos-y target-y))])

(defn midway-point
  [[left-x _] [right-x _]]
  (->
    (+ left-x right-x)
    (/ 2)
    int))

(defn closest-point
  [curr-pos & points]
  (->>
    (sort-by 
      #(first (distance curr-pos %))
      <
      points)
    (first)))

(defn subsurface
  [x-left x-right surface]
  (let [[x-left x-right] (sort [x-left x-right])]
    (->>
      (drop-while #(< (first %) x-left) surface)
      (take-while #(<= (first %) x-right)))))

(defn intersect-with-surface
  [[[x1 y1] [x2 y2]] surface]
  (let [territory (subsurface x1 x2 surface)]
    ))

(defn plot-ascent
  [curr-x curr-y surface]
  (let [pos [curr-x curr-y]
        zone (landing-zone surface)
        closest-zone (closest-point pos (first zone) (last zone))
        target-x (midway-point (first zone) (last zone))
        target-y (second (first zone))
        territory (subsurface curr-x (first closest-zone) surface)
        peak (highest-point territory)]
    ))

(defn delta-distance
  [[x1 y1] [x2 y2]]
  (binding [*out* *err*]
    (println "pos" [x1 y1])
    (println "dest" [x2 y2]))
  [(- x2 x1) (- y2 y1)])

(defn direction
  "Given a sequence of axis, returns a vector
  of 1, 0, or -1 to reflect the direction of
  each axis"
  [axis]
  (->
    (map
      #(cond
         (not (number? %)) %
         (zero? %) 0
         :else (/ (Math/abs %) %))
      axis)
    vec))

(defn magnitude
  "Given a sequence of axis, returns a vector 
  the absolute value of each axis."
  [axis]
  (->
    (map #(Math/abs %) axis)
    vec))

(defn scale-axis
  [x dx dy]
  (if-not (zero? dx)
    (-> (* x dy) (/ dx) int)
    0))

(defn scale
  [[delta-x delta-y] [vx vy]]
  (let [[mx my] (magnitude [delta-x delta-y])
        dir (direction [delta-x delta-y])]
    (->>
      (if (nil? vx)
        [(scale-axis vy mx my) vy]
        [vx (scale-axis vx my mx)])
      (map * dir))))

(defn delta-velocity
  [[delta-x delta-y] v-speed h-speed]
  (binding [*out* *err*]
    (println "delta direction" [delta-x delta-y])
    (println "velocity" [h-speed v-speed])
  (let [[dx dy] (direction [delta-x delta-y])
        ; [mx my] (magnitude [delta-x delta-y])
        [vx vy] (scale [dx dy] [max-h-speed nil])
        [vx vy] (if (< v-speed max-v-speed)
                  (scale [vx vy] [nil (- max-v-speed)])
                  [vx vy])]
        (println "target velocity" [vx vy])
        [(* dx (- h-speed vx)) (* dy (- v-speed vy))])))

(defn acceleration
  [[delta-vx delta-vy]]
  (binding [*out* *err*]
    (println "delta velocity" [delta-vx delta-vy]))
  [delta-vx (- delta-vy acc-gravity)])

(defn thrust
  [[accel-x accel-y]]
  {:pre []
   :post [(<= % 4) (>= % 0)]}
  (let [accel-y (if (neg? accel-y) 0 accel-y)]
    (->
      (* accel-x accel-x)
      (+ (* accel-y accel-y))
      (Math/sqrt)
      (Math/ceil)
      (min max-thrust)
      int)))

(defn angle
  [[accel-x accel-y]]
  (let [[dir-x dir-y] (direction [accel-x accel-y])]
    (if (pos? accel-y)
      (->
        (/ accel-x accel-y)
        (Math/atan)
        (Math/toDegrees)
        (* dir-x -1)
        int)
      (* -90 dir-x))))

(defn flight-parameters
  [accel]
  (binding [*out* *err*]
    (println "acceleration" accel))
  (let [ang (angle accel)
        t (thrust accel)]
    {:thrust t :rotation ang}))

(defn over-zone?
  [x zone]
  (let [[left _] (first zone)
        [right _] (last zone)]
    (<= left x right)))

(defn calculate-direction
  [x y surface zone]
  (if (over-zone? x zone)
    [0 -1]
    (let [left-x (ffirst zone)
          dist (- left-x y)]
      [(if (pos? dist) 1 -1) 0])))


(defn go-horizontally
  "Return a map of the change in thrust and rotation."
  [dir]
  {:thrust 4
   :rotation (* dir 35)})

;; TODO: conserve fuel!
(defn descend-vertically
  [v-speed power]
  (let [thrust (if (>= (- v-speed) max-v-speed)
                 (inc power)
                 (dec power))]
  {:thrust (-> (max thrust 0) (min 4))
   :rotation 0}))

(defn calculate-thrust
  [pos dest v-speed h-speed]
  (->
    (delta-distance pos dest)
    (delta-velocity v-speed h-speed)
    (acceleration)
    (flight-parameters)))

(defn -main [& args]
  (let [surface-n (int (read))
        surface (->>
                  (into [] (repeatedly (* surface-n 2) read))
                  (partition 2))
        zone (landing-zone surface)
        [x1 y1] (first zone)
        [x2 y2] (last zone)
        dest [(/ (+ x1 x2) 2) y1] 
        [x _ _ _ _ _ _] (repeatedly 7 (comp int read)) ]
    (println "0" "0")
    (while true
      (let [[x y h-speed v-speed fuel rotate power] (repeatedly 7 (comp int read))
            {:keys [rotation thrust]} (calculate-thrust [x y] dest v-speed h-speed)]
        (println rotation thrust)))))
