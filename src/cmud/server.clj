(ns cmud.server
  (:require [clojure.string :as string]
            [cmud.cmd :refer [handle-cmd]]
            [cmud.world :refer [make-world]]))

(defn main-loop
  [world]
  (let [input (string/trim (read-line))
        [cmd & args] (string/split input #"\s")]
    (cond
      (= cmd "") (recur world)
      (not= cmd "quit") (recur (handle-cmd world cmd args))
      :else (println "goodbye"))))

(defn -main
  []
  (println "Welcome to the game!")
  (main-loop (make-world)))
