(ns cmud.world
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defonce next-entity (atom 1))

(def seasons [:winter :summer :spring :fall])
(def times [:day :night :dusk :dawn])
(def weathers [:rain :snow :fog :sunny :cloudy :partly-cloudy])

(defn- load-zones
  []
  (let [zone-files (file-seq (io/file "data/zones/edn"))]
    (reduce (fn [zones zone-file]
              (if (or (.isDirectory zone-file)
                      (not (string/ends-with? (.getName zone-file) ".edn")))
                zones
                (let [zone (read-string (slurp zone-file))
                      zone-id (subs (.getName zone-file)
                                    0
                                    (- (count (.getName zone-file)) 4))]
                  (conj zones {:id zone-id
                               :rooms zone}))))
            []
            zone-files)))

(defn make-world
  []
  (let [zones (load-zones)
        rooms (mapcat :rooms zones)]
    {:rooms rooms
     :weather :sunny
     :time :day
     :season :summer
     :zones zones
     :entities {}}))

(defn room-description
  [room world entity]
  (let [desc (:description room)
        exits-desc (:exits-description room)
        time-desc (get-in room [:special-descriptions :time (:time world)])
        season-desc (get-in room [:special-descriptions :season (:season world)])
        weather-desc (get-in room [:special-descriptions :weather (:weather world)])]
    (cond-> desc
      season-desc (str " " season-desc)
      time-desc (str " " time-desc)
      weather-desc (str " " weather-desc)
      exits-desc (str " " exits-desc))))

(defn get-room
  [world room-id]
  (first (filter #(= (:id %) room-id) (:rooms world))))

(defn get-entity-room
  [world entity]
  (let [in-room-id (get-in world [:entities entity :in-room-id])
        room (get-room world in-room-id)]
    room))

(defn room-id->str
  ([room-id]
   (room-id->str room-id nil))
  ([room-id context-zone-id]
   (if (= (:zone-id room-id) context-zone-id)
     (str (:room-num room-id))
     (str (:zone-id room-id) "-" (:room-num room-id)))))

(defn add-entity
  [world]
  (let [entity (swap! next-entity inc)
        world (assoc-in world [:entities entity] {:in-room-id nil})]
    {:entity entity
     :world world}))

(defn raw-data->exit
  [raw-data zone-id]
  (let [[to-room-id rest] (string/split raw-data #", " 2)
        flags (if rest
                (set (map #(keyword (string/trim %)) (string/split rest #", ")))
                #{})]
    {:to-room-id {:zone-id zone-id :room-num (Integer/parseInt to-room-id)}
     :flags flags}))

(defn raw-data->zone
  [raw-data zone-id]
  (loop [lines (string/split-lines raw-data)
         room nil
         rooms []]
    (let [[line & rest] lines
          rooms (if (and room (= "" (string/trim-newline line)))
                  (conj rooms room)
                  rooms)
          room (cond
                 (string/starts-with? line "ID: ") {:id {:zone-id zone-id :room-num (Integer/parseInt (subs line 4))}}
                 (string/starts-with? line "Title: ") (assoc room :title (subs line 7))
                 (string/starts-with? line "Desc: ") (assoc room :description (subs line 6))
                 (string/starts-with? line "Exits Desc: ") (assoc room :exits-description (subs line 12))

                 (string/starts-with? line "Day Desc: ") (assoc-in room [:special-descriptions :time :day] (subs line 10))
                 (string/starts-with? line "Night Desc: ") (assoc-in room [:special-descriptions :time :night] (subs line 12))
                 (string/starts-with? line "Dusk Desc: ") (assoc-in room [:special-descriptions :time :dusk] (subs line 11))
                 (string/starts-with? line "Dawn Desc: ") (assoc-in room [:special-descriptions :time :dawn] (subs line 11))

                 (string/starts-with? line "Summer Desc: ") (assoc-in room [:special-descriptions :season :summer] (subs line 13))
                 (string/starts-with? line "Winter Desc: ") (assoc-in room [:special-descriptions :season :winter] (subs line 13))
                 (string/starts-with? line "Spring Desc: ") (assoc-in room [:special-descriptions :season :spring] (subs line 13))
                 (string/starts-with? line "Fall Desc: ")   (assoc-in room [:special-descriptions :season :fall]   (subs line 11))

                 (string/starts-with? line "Rain Desc: ")          (assoc-in room [:special-descriptions :weather :rain]          (subs line 11))
                 (string/starts-with? line "Snow Desc: ")          (assoc-in room [:special-descriptions :weather :snow]          (subs line 11))
                 (string/starts-with? line "Foggy Desc: ")         (assoc-in room [:special-descriptions :weather :fog]           (subs line 12))
                 (string/starts-with? line "Sunny Desc: ")         (assoc-in room [:special-descriptions :weather :sunny]         (subs line 12))
                 (string/starts-with? line "Cloudy Desc: ")        (assoc-in room [:special-descriptions :weather :cloudy]        (subs line 13))
                 (string/starts-with? line "Partly Cloudy Desc: ") (assoc-in room [:special-descriptions :weather :partly-cloudy] (subs line 20))

                 (string/starts-with? line "North Exit: ") (assoc-in room [:exits :north] (raw-data->exit (subs line 12) zone-id))
                 (string/starts-with? line "South Exit: ") (assoc-in room [:exits :south] (raw-data->exit (subs line 12) zone-id))
                 (string/starts-with? line "East Exit: ") (assoc-in room [:exits :east] (raw-data->exit (subs line 11) zone-id))
                 (string/starts-with? line "West Exit: ") (assoc-in room [:exits :west] (raw-data->exit (subs line 11) zone-id))
                 (string/starts-with? line "Up Exit: ") (assoc-in room [:exits :up] (raw-data->exit (subs line 9) zone-id))
                 (string/starts-with? line "Down Exit: ") (assoc-in room [:exits :down] (raw-data->exit (subs line 11) zone-id)))]
      (if (seq rest)
        (recur rest room rooms)
        (sort-by (comp :room-num :id)
                 (map last (vals (group-by :id rooms)))))))) ; removes duplicates

(defn convert-raw-zone
  [zone-name]
  (let [full-path (str "data/zones/raw/" zone-name ".txt")
        zone-id zone-name
        zone (raw-data->zone (slurp full-path) zone-id)
        out-path (str "data/zones/edn/" zone-name ".edn")]
    (io/make-parents out-path)
    (spit out-path (pr-str zone))))
