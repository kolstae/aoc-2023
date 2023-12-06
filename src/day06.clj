(ns day06
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "Time:      7  15   30\nDistance:  9  40  200\n\n")

(defn parse-input [input]
  (->> (str/split-lines input)
       (map (partial re-seq #"\d+"))
       (mapv (partial mapv parse-long))))

(defn calc-win-count [t d]
  (->> (range 1 t)
       (map #(* % (- t %)))
       (filter (partial < d))
       count))

(comment

  (let [[times dists] (parse-input input)]
    (->> (mapv calc-win-count times dists)
         (reduce *)))
  ;;1084752

  (->> (parse-input input)
       (map (comp parse-long str/join))
       (apply calc-win-count))
  ;;28228952
  )
