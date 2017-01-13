(ns Player
  (:gen-class))

(defn -main [& args]
  (while true
    (let [spaceX (read) 
          spaceY (read)
          xMax 8
          xOfHighest (atom 0)
          highestHeight (atom 0)
          action (atom "HOLD")]
      (loop [i xMax]
        (when (> i 0)
          (let [mountainH (read)]
           (when (> mountainH @highestHeight)
               (reset! xOfHighest (- xMax i))
               (reset! highestHeight mountainH))
            (recur (dec i)))))
      (when (= spaceX @xOfHighest)
          (reset! action "FIRE"))
      (println @action))))