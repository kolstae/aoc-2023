(ns day17
  (:require [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "2413432311323\n3215453535623\n3255245654254\n3446585845452\n4546657867536\n1438598798454\n4457876987766\n3637877979653\n4654967986887\n4564679986453\n1224686865563\n2546548887735\n4322674655533")

(defn min-heat-loss [input min-straight max-straight]
  (let [heat-map (str/split-lines input)
        height (count heat-map)
        width (count (first heat-map))
        d (fn [[y x]] (- (int (get-in heat-map [y x])) (int \0)))
        h (fn [[y x]] (+ (- height y) (- width x)))
        accum-w (fn [[_ weight] p] [p (+ weight (d p))])
        max-straight (inc max-straight)
        neighbors (fn [cur-score [y x dir]]
                    (concat (->> (case dir
                                   (:left :right) (->> (range (inc y) (min height (+ y max-straight)))
                                                       (map #(vector % x :down)))
                                   (:up :down) (->> (range (inc x) (min width (+ x max-straight)))
                                                    (map #(vector y % :right))))
                                 (reductions accum-w [nil cur-score])
                                 (drop min-straight))
                            (->> (case dir
                                   (:left :right) (->> (range (dec y) (max -1 (- y max-straight)) -1)
                                                       (map #(vector % x :up)))
                                   (:up :down) (->> (range (dec x) (max -1 (- x max-straight)) -1)
                                                    (map #(vector y % :left))))
                                 (reductions accum-w [nil cur-score])
                                 (drop min-straight))))
        target [(dec height) (dec width)]]
    (loop [open #{[0 0 :right] [0 0 :down]}
           came-from {}
           g-score (into {} (map vector open (repeat 0)))
           f-score (into {} (map (juxt identity h) open))]
      (let [[y x :as cur] (apply min-key f-score open)]
        #_(prn :at y x dir)
        (if (= target [y x])
          (g-score cur)
          (let [neighbors (->> (neighbors (g-score cur) cur)
                               (filterv (fn [[p w]] (< w (g-score p ##Inf)))))]
            (recur (-> open
                       (disj cur)
                       (into (map first) neighbors))
                   (into came-from (map (comp #(vector % cur) first)) neighbors)
                   (into g-score neighbors)
                   (into f-score (map (fn [[p w]] [p (+ w (h p))])) neighbors))))))))

(comment

  (time
    (min-heat-loss input 1 3))
  ;; 1128

  (time
    (min-heat-loss input 4 10))
  ;; 1268
  )
