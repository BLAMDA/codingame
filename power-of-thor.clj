(ns Player
  (:gen-class))

(defn- go-north?
  [lightY thorY]
  (and (< lightY thorY) (> thorY 0)))

(defn- go-south?
  [lightY thorY]
  (and (> lightY thorY) (< thorY 18)))

(defn- go-east?
  [lightX thorX]
  (and (> lightX thorX) (< thorX 40)))

(defn- go-west?
  [lightX thorX]
  (and (< lightX thorX) (> thorX 0)))

(defn -main [& args]
  (let [lightX (read)
        lightY (read)
        initialTX (read)
        initialTY (read)
        thorY (atom initialTY)
        thorX (atom initialTX)]
    (while true
      (let [remainingTurns (read)
            y (cond
                (go-north? lightY @thorY) (do (swap! thorY dec) "N")
                (go-south? lightY @thorY) (do (swap! thorY inc) "S")
                :else "")
            x (cond
                (go-east? lightX @thorX) (do (swap! thorX inc) "E")
                (go-west? lightX @thorX) (do (swap! thorX dec) "W")
                :else "")]
        (binding [*out* *err*]
          (println "thorY=" @thorY)
          (println "thorX=" @thorX))
        (println (clojure.string/join "" [y x]))))))