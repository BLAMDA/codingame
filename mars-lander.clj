(ns Player
  (:gen-class))

(def acc-gravity -56)
(def ^:const max-v-speed 20)
(def ^:const max-h-speed 40)

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
   :rotation (* dir 15)})

(defn descend-vertically
  [v-speed power]
  (let [thrust (if (>= (- v-speed) max-v-speed)
                 (inc power)
                 (dec power))]
  {:thrust (-> (max thrust 0) (min 4))
   :rotation 0}))

(defn calculate-thrust
  [x y midway-point surface zone v-speed power]
  (let [left-x (ffirst zone)
        dist-zone (- x left-x)
        dir (/ dist-zone (Math/abs dist-zone))
        dist-midway (- midway-point left-x)]
    (cond
      (over-zone? x zone)
        (descend-vertically v-speed power)
      (<= (Math/abs dist-zone) (Math/abs dist-midway))
        (go-horizontally (- dir))
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
            {:keys [rotation thrust]} (calculate-thrust x y midway-point surface zone v-speed power)]
        (println rotation thrust)))))
