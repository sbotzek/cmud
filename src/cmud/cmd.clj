(ns cmud.cmd
  (:require [clojure.string :as string]
            [cmud.world :as world]
            [cmud.cmd :as cmd]))

(declare handle-cmd)

(defn parse-keyword
  [keywords s]
  (->> keywords
       (filter #(string/starts-with? (name %) s))
       (first)))

(defn parse-room-id
  [world entity s]
  (cond
    (nil? s)
    nil

    (string/includes? s "-")
    (try
      (let [idx (string/last-index-of s "-")
            prefix (subs s 0 idx)
            suffix (subs s (inc idx))]
        (let [zone-id prefix
              room-num (Integer/parseInt suffix)]
          {:zone-id zone-id :room-num room-num}))
      (catch NumberFormatException e
        nil))

    :else
    (try
      (let [in-room-id (get-in world [:entities entity :in-room-id])]
        (if in-room-id
          {:zone-id (:zone-id in-room-id) :room-num (Integer/parseInt s)}
          {:zone-id (get-in world [:zones 0 :id]) :room-num (Integer/parseInt s)}))
      (catch NullPointerException e
        nil))))

(defn cmd-move
  [world entity dir]
  (let [room (world/get-entity-room world entity)
        exit (get-in room [:exits dir])]
    (if exit
      (let [to-room (world/get-room world (:to-room-id exit))]
        (if to-room
          (let [world' (assoc-in world [:entities entity :in-room-id] (:to-room-id exit))]
            (handle-cmd world' entity "look" [])
            world')
          (println (str "Could not find room " (:to-room-id exit)))))
      (println (str "You cannot go " (name dir))))))

(defn cmd-setting
  [m setting keywords s]
  (if s
    (let [value (parse-keyword keywords s)]
      (if value
        (do
          (println (str "Setting " (name setting) " to " (name value)))
          (assoc m setting value))
        (println (str "Unknown " (name setting) ": " s ", valid ones are: " (string/join ", " (map name keywords))))))
    (println (str "Current " (name setting) ": " (name (get m setting))))))

(def cmd-table
  [{:cmd "north" :fn (fn cmd-north [world entity _ _] (cmd-move world entity :north))}
   {:cmd "south" :fn (fn cmd-south [world entity _ _] (cmd-move world entity :south))}
   {:cmd "east" :fn (fn cmd-east [world entity _ _] (cmd-move world entity :east))}
   {:cmd "west" :fn (fn cmd-west [world entity _ _] (cmd-move world entity :west))}
   {:cmd "up" :fn (fn cmd-up [world entity _ _] (cmd-move world entity :up))}
   {:cmd "down" :fn (fn cmd-down [world entity _ _] (cmd-move world entity :down))}
   {:cmd "season" :fn (fn cmd-season
                        [world entity cmd-input args]
                        (cmd-setting world :season world/seasons (first args)))}
   {:cmd "time" :fn (fn cmd-time
                      [world entity cmd-input args]
                        (cmd-setting world :time world/times (first args)))}
   {:cmd "weather" :fn (fn cmd-weather
                         [world entity cmd-input args]
                        (cmd-setting world :weather world/weathers (first args)))}
  {:cmd "rooms" :fn (fn cmd-rooms
                       [world entity cmd-input args]
                       (doseq [zone (:zones world)]
                         (println "Zone:" (:id zone))
                         (doseq [room (:rooms zone)]
                           (println (str "  " (:title room) " (" (world/room-id->str (:id room)) ")")))))}
   {:cmd "convert" :fn (fn cmd-convert
                         [world entity cmd-input args]
                         (try
                           (world/convert-raw-zone (first args))
                           (println "Converted zone file.")
                           (world/reload-zones world)
                           (catch Exception e
                             (println (str "Error converting zone file: " (.getMessage e)))
                             (.printStackTrace e))))}
   {:cmd "look" :fn (fn cmd-look
                      [world entity cmd-input args]
                      (let [room (world/get-entity-room world entity)]
                        (if room
                          (do
                            (println (str (:title room) " (" (world/room-id->str (:id room)) ")" " [" (:season world) " " (:time world) " " (:weather world) "]"))
                            (println (world/room-description room world entity))
                            (let [visible-exits (filter (fn [[dir exit]]
                                                          (not (:hidden (:flags exit))))
                                                        (:exits room))]
                              (when (seq visible-exits)
                                (println (str "Exits: " (string/join " " (map #(name (first %)) visible-exits)))))))
                          (println "You are nowhere."))))}
   {:cmd "goto" :fn (fn cmd-goto
                      [world entity cmd-input args]
                      (let [room-id (parse-room-id world entity (first args))
                            room (world/get-room world room-id)]
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
