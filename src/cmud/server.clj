(ns cmud.server
  (:require [clojure.string :as string]
            [cmud.input :refer [handle-input]]
            [cmud.world :refer [make-world add-entity]]))

(defn main-loop
  [world entity]
  (let [input (string/trim (read-line))
        [cmd & args] (string/split input #"\s")]
    (cond
      (= cmd "") (recur world entity)
      (not= cmd "quit") (let [world' (or (handle-input world entity cmd args) world)]
                          (recur world' entity))
      :else (println "goodbye"))))

(defn -main
  []
  (println "Welcome to the game!")
  (let [world (make-world)
        {:keys [entity world]} (add-entity world)]
    (main-loop world entity)))
