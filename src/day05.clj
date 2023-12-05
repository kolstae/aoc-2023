(ns day05
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4")

(defn parse-map [ls]
  (->> (next ls)                                            ;; skip name
       (mapv (fn [s] (mapv parse-long (str/split s #"\s+"))))
       (sort-by first)
       vec))

(defn parse-input [input]
  (let [[seed-str _ & rest] (str/split-lines input)]
    [(mapv parse-long (re-seq #"\d+" seed-str))
     (->> rest
          (partition-by empty?)
          (filter next)
          (mapv parse-map))]))

;; Stolen from https://gitlab.com/maximoburrito/advent2023/-/blob/main/src/day05/main.clj
(defn apply-map-range [n maps]
  (or (first
        (for [[dst src len] maps
              :when (<= src n (+ len src -1))]              ;; find map holding n
          [(+ n (- dst src)) (- len (- n src))]))           ;; translate into dest range
      (first
        (for [[_dst src _len] (sort-by second maps)
              :when (> src n)]                              ;; find map after n (hole)
          [n (- src n)]))                                   ;; make range for hole
      [n ##Inf]))

(defn expand-range [[n len] m]
  (let [[n' len'] (apply-map-range n m)
        consumed (min len len')]
    (cond-> [[n' consumed]]
      (< len' len)                                          ;; need to consume the full length
      (into (expand-range [(+ n consumed) (- len consumed)] m)))))

(comment

  (let [[seeds all-maps] (parse-input input #_small-input)]
    (->> seeds
         (mapv #(reduce (fn [seed rs]
                          (reduce (fn [seed [dst src len]]
                                    (if (<= src seed (+ src len))
                                      (reduced (+ seed (- dst src)))
                                      seed))
                                  seed rs))
                        % all-maps))
         (reduce min)))
  ;; 178159714

  (let [[seeds all-maps] (parse-input input #_small-input)]
    (->> all-maps
         (reduce (fn [ranges maps]
                   (mapcat #(expand-range % maps) ranges))
                 (partition 2 seeds))
         (mapv first)
         sort
         first))
  ;; 100165128
  )
