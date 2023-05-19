(ns cmud.cmd
  (:require [clojure.string :as string]
            [cmud.world]))

(declare handle-cmd)

(defn cmd-move
  [world entity dir]
  (let [room (cmud.world/get-entity-room world entity)
        exit (get-in room [:exits dir])]
    (if exit
      (let [to-room (cmud.world/get-room world (:to-room-id exit))]
        (if to-room
          (let [world' (assoc-in world [:entities entity :in-room-id] (:to-room-id exit))]
            (handle-cmd world' entity "look" [])
            world')
          (println (str "Could not find room " (:to-room-id exit)))))
      (println (str "You cannot go " (name dir))))))


(def cmd-table
  [{:cmd "north" :fn (fn cmd-north [world entity _ _] (cmd-move world entity :north))}
   {:cmd "south" :fn (fn cmd-south [world entity _ _] (cmd-move world entity :south))}
   {:cmd "east" :fn (fn cmd-east [world entity _ _] (cmd-move world entity :east))}
   {:cmd "west" :fn (fn cmd-west [world entity _ _] (cmd-move world entity :west))}
   {:cmd "up" :fn (fn cmd-up [world entity _ _] (cmd-move world entity :up))}
   {:cmd "down" :fn (fn cmd-down [world entity _ _] (cmd-move world entity :down))}
   {:cmd "rooms" :fn (fn cmd-rooms
                       [world entity cmd-input args]
                       (println "Rooms:")
                       (doseq [room (:rooms world)]
                         (println (str (:title room) " (" (:id room) ")"))))}
   {:cmd "convert" :fn (fn cmd-convert
                         [world entity cmd-input args]
                         (try
                           (cmud.world/convert-raw-zone (first args))
                           (println "Converted zone file.")
                           (catch Exception e
                             (println (str "Error converting zone file: " (.getMessage e))))))}
   {:cmd "look" :fn (fn cmd-look
                      [world entity cmd-input args]
                      (let [room (cmud.world/get-entity-room world entity)]
                        (if room
                          (do
                            (println (str (:title room) " (" (:id room) ")"))
                            (println (:description room))
                            (let [visible-exits (filter (fn [[dir exit]]
                                                          (not (:hidden (:flags exit))))
                                                        (:exits room))]
                              (when (seq visible-exits)
                                (println (str "Exits: " (string/join " " (map #(name (first %)) visible-exits)))))))
                          (println "You are nowhere."))))}
   {:cmd "goto" :fn (fn cmd-goto
                      [world entity cmd-input args]
                      (let [room-id (Integer/parseInt (first args))
                            room (cmud.world/get-room world room-id)]
                        (if room
                          (let [world' (assoc-in world [:entities entity :in-room-id] room-id)]
                            (handle-cmd world' entity "look" [])
                            world')
                          (println (str "Could not find room " room-id)))))}
   ])

(defn handle-cmd
  [world entity cmd-input args]
  (let [cmd (first (filter (fn cmd?
                             [cmd]
                             (string/starts-with? (:cmd cmd) cmd-input))
                           cmd-table))]
    (if cmd
      ((:fn cmd) world entity cmd-input args)
      (println (str "Unknown command '" cmd-input "'")))))
