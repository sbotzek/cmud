(ns cmud.cmd
  (:require [clojure.string :as string]
            [cmud.world]))

(def cmd-table
  [{:cmd "rooms" :fn (fn cmd-rooms
                       [world cmd-input args]
                       (println "Rooms:")
                       (doseq [room (:rooms world)]
                         (println (str (:title room) " (" (:id room) ")"))))}
   ])

(defn handle-cmd
  [world cmd-input args]
  (let [cmd (first (filter (fn cmd?
                             [cmd]
                             (string/starts-with? (:cmd cmd) cmd-input))
                           cmd-table))]
    (if cmd
      ((:fn cmd) world cmd-input args)
      (do (println (str "Unknown command '" cmd-input "'"))
          world))))
