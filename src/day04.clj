(ns day04
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(defn str->card-no+win [l]
  (let [[c & css] (str/split l #"\s*[:+\|]\s*")]
    [(parse-long (re-find #"\d+" c))
     (->> css
          (mapv #(into #{} (keep parse-long) (str/split % #"\s+")))
          (apply set/intersection)
          count)]))

(comment

  (->> (str/split-lines #_small-input input)
       (transduce (comp
                    (map str->card-no+win)
                    (map second)
                    (filter pos?)
                    (map dec)
                    (map (partial bit-shift-left 1)))
                  +))
  ;; 21105

  (let [cs (->> (str/split-lines #_small-input input)
                (into {}
                      (comp (map str->card-no+win)
                            (map #(update % 1 (partial vector 1))))))
        max-end (inc (apply max (keys cs)))]
    (->> (keys cs)
         sort
         (reduce (fn [cs i]
                   (let [[cnt len] (get cs i)]
                     (reduce (fn [cs i] (update cs i update 0 + cnt))
                             cs
                             (range (inc i) (min max-end (+ len (inc i)))))))
                 cs)

         vals
         (map first)
         (reduce +)))
  ;; 5329815
  )
