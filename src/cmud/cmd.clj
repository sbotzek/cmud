(ns cmud.cmd
  (:require [clojure.string :as string]
            [cmud.world]))

(def cmd-table
  [{:cmd "rooms" :fn (fn cmd-rooms
                       [world cmd-input args]
                       (println "Rooms:")
                       (doseq [room (:rooms world)]
                         (println (str (:title room) " (" (:id room) ")"))))}
   {:cmd "convert" :fn (fn cmd-convert
                         [world cmd-input args]
                         (try
                           (cmud.world/convert-raw-zone (first args))
                           (println "Converted zone file.")
                           (catch Exception e
                             (println (str "Error converting zone file: " (.getMessage e))))))}
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
