(ns cmud.world
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.string :as str]))

(defn- load-zones
  []
  (let [zone-files (file-seq (io/file "data/zones/edn"))]
    (reduce (fn [zones zone-file]
              (if (.isDirectory zone-file)
                zones
                (let [zone (read-string (slurp zone-file))]
                  (conj zones zone))))
            []
            zone-files)))

(defn make-world
  []
  (let [zones (load-zones)
        rooms (mapcat identity zones)]
    {:rooms rooms
     :zones zones}))

(defn raw-data->exit
  [raw-data]
  (let [[to-room-id rest] (string/split raw-data #", " 2)
        flags (if rest
                (set (map #(keyword (string/trim %)) (string/split rest #", ")))
                #{})]
    {:to-room-id (Integer/parseInt to-room-id)
     :flags flags}))

(defn raw-data->zone
  [raw-data]
  (loop [lines (string/split-lines raw-data)
         room nil
         rooms []]
    (let [[line & rest] lines
          rooms (if (and room (= "" (string/trim-newline line)))
                  (conj rooms room)
                  rooms)
          room (cond
                 (string/starts-with? line "ID: ") {:id (Integer/parseInt (subs line 4))}
                 (string/starts-with? line "Title: ") (assoc room :title (subs line 7))
                 (string/starts-with? line "Desc: ") (assoc room :description (subs line 6))
                 (string/starts-with? line "North Exit: ") (assoc-in room [:exits :north] (raw-data->exit (subs line 12)))
                 (string/starts-with? line "South Exit: ") (assoc-in room [:exits :south] (raw-data->exit (subs line 12)))
                 (string/starts-with? line "East Exit: ") (assoc-in room [:exits :east] (raw-data->exit (subs line 11)))
                 (string/starts-with? line "West Exit: ") (assoc-in room [:exits :west] (raw-data->exit (subs line 11)))
                 (string/starts-with? line "Up Exit: ") (assoc-in room [:exits :up] (raw-data->exit (subs line 9)))
                 (string/starts-with? line "Down Exit: ") (assoc-in room [:exits :down] (raw-data->exit (subs line 11))))]
      (if (seq rest)
        (recur rest room rooms)
        (sort-by :id
                 (map last (vals (group-by :id rooms)))))))) ; removes duplicates

(defn convert-raw-zone
  [zone-name]
  (let [full-path (str "data/zones/raw/" zone-name ".txt")
        zone (raw-data->zone (slurp full-path))
        out-path (str "data/zones/edn/" zone-name ".edn")]
    (io/make-parents out-path)
    (spit out-path (pr-str zone))))
