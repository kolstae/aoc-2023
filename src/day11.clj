(ns day11
  (:require [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#.....")

(defn parse-input [input]
  (str/split-lines input))

(defn ->points [star-map]
  (->> star-map
       (map-indexed (fn [y l]
                      (keep-indexed (fn [x c]
                                      (when (= \# c)
                                        [[x y] c])) l)))
       (mapcat identity)
       (mapv first)))


(comment

  (let [star-map (parse-input input #_small-input)
        ys (->> star-map
                (keep-indexed (fn [i s] (when (every? #{\.} s) i))))
        xs (->> star-map
                (apply mapv vector)
                (keep-indexed (fn [i s] (when (every? #{\.} s) i))))
        factor (dec 2)
        ps (->points star-map)]
    (->> (for [[i [x y]] (map-indexed vector ps)
               [ox oy] (subvec ps (inc i))
               :let [[x1 x2] (sort [x ox])
                     [y1 y2] (sort [y oy])]]
           (+ (- x2 x1) (- y2 y1)
              (* factor (count (filterv #(< x1 % x2) xs)))
              (* factor (count (filterv #(< y1 % y2) ys)))))
         (reduce +)))
  ;; 9556896

  (let [star-map (parse-input input #_small-input)
        ys (->> star-map
                (keep-indexed (fn [i s] (when (every? #{\.} s) i))))
        xs (->> star-map
                (apply mapv vector)
                (keep-indexed (fn [i s] (when (every? #{\.} s) i))))
        factor (dec 1000000)
        ps (->points star-map)]
    (->> (for [[i [x y]] (map-indexed vector ps)
               [ox oy] (subvec ps (inc i))
               :let [[x1 x2] (sort [x ox])
                     [y1 y2] (sort [y oy])]]
           (+ (- x2 x1) (- y2 y1)
              (* factor (count (filterv #(< x1 % x2) xs)))
              (* factor (count (filterv #(< y1 % y2) ys)))))
         (reduce +)))
  ;; 685038186836
  )
