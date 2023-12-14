(ns day14
  (:require [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#....")

(defn parse-input [input]
  (->> (str/split-lines input)
       (map-indexed (fn [y l] (keep-indexed (fn [x c] (when (#{\# \O} c) [[y x] c])) l)))
       (mapcat identity)
       (into {})))

(defn print-platform [m]
  (let [max-y (apply max (map ffirst m))
        max-x (apply max (map (comp second first) m))]
    (dorun
      (for [y (range (inc max-y))]
        (println (str/join (for [x (range (inc max-x))] (m [y x] \.))))))
    (println)
    m))

(defn calc-load [m]
  (->> (filter (comp #{\O} second) m)
       (mapv ffirst)
       (map (partial - (inc (apply max (map ffirst m)))))
       (reduce +)))

(defn tilt [max-y max-x dir]
  (fn [m [y x]]
    (let [k (->> (case dir
                   :N (range (dec y) -1 -1)
                   :S (range (inc y) (inc max-y))
                   :W (range (dec x) -1 -1)
                   :E (range (inc x) (inc max-x)))
                 (map (case dir
                        (:N :S) #(vector % x)
                        (:W :E) #(vector y %)))
                 (take-while (complement m))
                 last)]
      #_(when k (prn [y x] k))
      (cond-> m
        k (-> (dissoc [y x]) (assoc k \O))))))

(comment

  (let [platform (parse-input input)]
    (->> platform
         (filter (comp #{\O} second))
         (map first)
         sort
         (reduce (tilt nil nil :N) platform)
         calc-load))
  ;; 103333

  (let [platform (parse-input input)
        max-y (apply max (map ffirst platform))
        max-x (apply max (map (comp second first) platform))
        all-tilts (memoize
                    (fn [m]
                      (->> [:N :W :S :E]
                           (reduce (fn [m dir]
                                     (->> m
                                          (filter (comp #{\O} second))
                                          (map first)
                                          (sort-by (case dir
                                                     (:N :S) first
                                                     (:W :E) second)
                                                   (case dir
                                                     (:N :W) <
                                                     (:S :E) >))
                                          (reduce (tilt max-y max-x dir) m)))
                                   m)
                           #_print-platform)))
        from-1000 (->> (iterate all-tilts platform)
                       (drop 1000)
                       (take 100)
                       (map calc-load))
        cycle-size (->> from-1000
                        frequencies
                        count)]
    (nth from-1000 (mod (- 1000000000 1000) cycle-size)))
  ;; 97241
  )
