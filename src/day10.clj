(ns day10
  (:require [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "..F7.\n.FJ|.\nSJ.L7\n|F--J\nLJ...")
(def small-input-2 "...........\n.S-------7.\n.|F-----7|.\n.||.....||.\n.||.....||.\n.|L-7.F-J|.\n.|..|.|..|.\n.L--J.L--J.\n...........")

(defn parse-input [input]
  (let [lines (str/split-lines input)]
    (into {} (for [[y l] (map-indexed vector lines)
                   [x c] (map-indexed vector l)
                   :when (not= \. c)]
               [[x y] c]))))

(def dir->possibles {:N {\| :N \F :E \7 :W}
                     :E {\- :E \7 :S \J :N}
                     :S {\| :S \J :W \L :E}
                     :W {\- :W \L :N \F :S}})

(defn all-pipes [chart]
  (let [[[x y]] (first (filter (comp #{\S} val) chart))
        start-dir (keep (fn [[pos dir]]
                          (when (get-in dir->possibles [dir (chart pos)])
                            dir))
                        {[(dec x) y] :W [(inc x) y] :E [x (inc y)] :S [x (dec y)] :N})]
    (->> [[x y] (first start-dir)]
         (iterate (fn [[[x y] dir]]
                    (let [next-pos (case dir
                                     :N [x (dec y)]
                                     :S [x (inc y)]
                                     :W [(dec x) y]
                                     :E [(inc x) y])]
                      [next-pos (get-in dir->possibles [dir (chart next-pos)])])))
         (take-while second))))

; https://en.wikipedia.org/wiki/Point_in_polygon : Ray casting algorithm
(defn count-x-intersections [coll]
  (loop [[[idx c] & more] coll crossing 0 res []]
    (if c
      (if-some [[[n-idx n-c] :as more] more]
        (cond
          ;; U-turns doesn't increase intersection count
          (or (and (= \F c) (= \7 n-c))
              (and (= \L c) (= \J n-c)))
          (recur more (dec crossing) res)

          ;; S-bends only increase intersection count by one
          (or (and (= \F c) (= \J n-c))
              (and (= \L c) (= \7 n-c)))
          (recur more crossing res)

          :else
          (recur more (inc crossing) (conj res [(inc crossing) [idx n-idx]])))
        res)
      res)))

(comment

  (-> (parse-input input) all-pipes count (/ 2))
  ;; 6927

  (let [chart (parse-input input #_small-input-2
                           #_".F----7F7F7F7F-7....\n.|F--7||||||||FJ....\n.||.FJ||||||||L7....\nFJL7L7LJLJ||LJ.L-7..\nL--J.L7...LJS7F-7L7.\n....F-J..F7FJ|L7L7L7\n....L7.F7||L7|.L7L7|\n.....|FJLJ|FJ|F7|.LJ\n....FJL-7.||.||||...\n....L---J.LJ.LJLJ..."
                           #_"FF7FSF7F7F7F7F7F---7\nL|LJ||||||||||||F--J\nFL-7LJLJ||||||LJL-77\nF--JF--7||LJLJ7F7FJ-\nL---JF-JLJ.||-FJLJJ7\n|F|F-JF---7F7-L7L|7|\n|FFJF7L7F-JF7|JL---7\n7-L-JL7||F7|L7F-7F7|\nL.L7LFJ|||||FJL7||LJ\nL7JLJL-JLJLJL--JLJ.L")
        chart (->> (all-pipes chart)
                   (mapv first)
                   (select-keys chart))]                    ;; only include pipe in chart
    (->> chart
         (filterv (comp not #{\-} second))                  ;; collapse bends and U-turns
         (sort-by (comp second key))
         (partition-by (comp second key))                   ;; partition-by line
         next
         butlast                                            ;; first/last is pipe at best
         (mapv (partial mapv (juxt ffirst second)))         ;; only need index + pipe-type
         (mapcat (comp count-x-intersections sort))
         (filterv (comp odd? first))                        ;; Ray casting algorithm: odd
         (mapv (comp dec (partial apply -) reverse second))
         (reduce +)))
  ;; 467
  )
