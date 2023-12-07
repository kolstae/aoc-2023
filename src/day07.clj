(ns day07
  (:require [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483")

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv #(-> %
                  (str/split #"\s+")
                  (update 1 parse-long)))))

(def card-score (into {}
                      (map-indexed #(vector %2 (- %1)))
                      [\A \K \Q \J \T \9 \8 \7 \6 \5 \4 \3 \2]))
(def type-score (into {} (map-indexed #(vector %2 (- %1))) [[5] [4] [2 3] [3] [2 2] [2] []]))
(def card-score-2 (assoc card-score \J -20))

(defn score-type [c-freq]
  (->> c-freq vals (filter (partial < 1)) sort vec type-score))

(comment

  (->> (parse-input input #_small-input)
       (sort-by (fn [[cards]]
                  [(->> cards frequencies score-type)
                   (mapv card-score cards)]))
       (map second)
       (map-indexed #(* (inc %1) %2))
       (reduce +))
  ;;245794640

  (letfn [(max-out-jokers [c-freq]
            (let [j-cnt (get c-freq \J 0)]
              (if (< 0 j-cnt 5)
                (let [freqs (dissoc c-freq \J)
                      card (->> freqs
                                (sort-by (juxt (comp - val)
                                               (comp - card-score-2 key)))
                                ffirst)]
                  (update freqs card + j-cnt))
                c-freq)))]
    (->> (parse-input input #_small-input)
         (sort-by (fn [[cards]]
                    [(-> cards frequencies max-out-jokers score-type)
                     (mapv card-score-2 cards)]))
         (map second)
         (map-indexed #(* (inc %1) %2))
         (reduce +)))
  ;;247899149
  )
