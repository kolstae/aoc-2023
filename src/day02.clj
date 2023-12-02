(ns day02
  (:require [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(defn parse-game [s]
  (let [[g s] (str/split s #"\s*:\s*")]
    [(parse-long (re-find #"\d+" g))
     (->> (str/split s #";\s+")
          (mapv (fn [s]
                  (into {}
                        (map (fn [s]
                               (let [[n color] (str/split s #"\s+")]
                                 [(keyword color) (parse-long n)])))
                        (str/split s #",\s*"))))
          (apply merge-with max))]))


(comment

  (let [games (->> input #_small-input
                   str/split-lines
                   (mapv parse-game))
        required {:red 12, :green 13, :blue 14}
        possible? (fn [cubes]
                    (->> cubes
                         (map (juxt val (comp required key)))
                         (every? (partial apply <=))))]
    (->> games
         (filter (comp possible? second))
         (map first)
         (reduce +)))
  ;; 2348

  (let [games (->> input #_small-input
                   str/split-lines
                   (mapv parse-game))]
    (->> (map (comp (partial apply *) vals second) games)
         (reduce +)))
  ;; 76008

  )
