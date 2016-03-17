(ns Player
  (:gen-class))

(def acc-gravity -3.711)
(def ^:const max-v-speed 40)
(def ^:const max-h-speed 20)

(defn landing-zone
  [surface]
  (->>
    (partition-by second surface)
    (filter #(> (count %) 1))
    first))

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
  [x y midway-point surface zone v-speed h-speed power]
  (let [left-x (ffirst zone)
        right-x (-> zone second first)
        dist-right (- x right-x)
        dist-left (- x left-x)
        dist-zone (cond 
                    (over-zone? x zone)
                      0
                    (< (Math/abs dist-right) (Math/abs dist-left))
                      dist-right
                    :else 
                      dist-left)
        dir (if-not (zero? dist-zone)
              (/ dist-zone (Math/abs dist-zone))
              0)
        dist-midway (- midway-point left-x)
        abs-h-speed (Math/abs h-speed)
        abs-v-speed (Math/abs v-speed)]
    (cond
      (and (over-zone? x zone) (<= abs-h-speed max-h-speed))
        (descend-vertically v-speed power)
      (and (over-zone? x zone) (> abs-h-speed max-h-speed))
        (go-horizontally (/ h-speed abs-h-speed))
      (or (> abs-h-speed (+ 10 max-h-speed)) (> abs-v-speed max-v-speed))
        (descend-vertically v-speed power)
      :else
        (go-horizontally dir))))

(defn -main [& args]
  (let [surface-n (int (read))
        surface (->>
                  (into [] (repeatedly (* surface-n 2) read))
                  (partition 2))
        zone (landing-zone surface)
        [x _ _ _ _ _ _] (repeatedly 7 (comp int read))
        midway-point (/ (+ x (ffirst zone)) 2)]
    (println "0" "0")
    (while true
      (let [[x y h-speed v-speed fuel rotate power] (repeatedly 7 (comp int read))
            {:keys [rotation thrust]} (calculate-thrust x y midway-point surface zone v-speed h-speed power)]
        (println rotation thrust)))))
