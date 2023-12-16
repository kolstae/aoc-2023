(ns day16
  (:require [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input ".|...\\....\n|.-.\\.....\n.....|-...\n........|.\n..........\n.........\\\n..../.\\\\..\n.-.-/..|..\n.|....-|.\\\n..//.|....")

(defn parse-input [input]
  (into {} (comp (map-indexed (fn [y l]
                                (keep-indexed (fn [x c] (when-not (= \. c) [[y x] c])) l)))
                 cat)
        (str/split-lines input)))

(defn take-until [pred coll]
  (loop [more coll res []]
    (if (seq more)
      (let [v (first more)]
        (if (pred v)
          (conj res v)
          (recur (next more) (conj res v))))
      res)))

(defn next-beams [[pos l] dir]
  (mapv (partial vector pos)
        (case l
          \| (case dir
               (:right :left) [:up :down]
               [dir])
          \- (case dir
               (:up :down) [:left :right]
               [dir])
          \\ (case dir
               :right [:down]
               :left [:up]
               :up [:left]
               :down [:right])
          \/ (case dir
               :right [:up]
               :left [:down]
               :up [:right]
               :down [:left]))))

(defn move-beam [max-y max-x [[y x] dir]]
  (case dir
    :right (mapv (partial vector y) (range (inc x) (inc max-x)))
    :left (mapv (partial vector y) (range (dec x) -1 -1))
    :up (mapv #(vector % x) (range (dec y) -1 -1))
    :down (mapv #(vector % x) (range (inc y) (inc max-y)))))

(defn count-exited-fn [lenses]
  (let [max-y (apply max (map first (keys lenses)))
        max-x (apply max (map second (keys lenses)))]
    (fn [beam]
      (->> (loop [beam beam beams #{} exited #{} seen-beams #{}]
             (let [moves (take-until lenses (move-beam max-y max-x beam))
                   lens (find lenses (peek moves))
                   beams (cond-> beams
                           lens (into (remove seen-beams) (next-beams lens (second beam))))]
               (if-some [beam (first beams)]
                 (recur beam (disj beams beam) (into exited moves) (conj seen-beams beam))
                 (into exited moves))))
           count))))

(comment

  (let [lenses (parse-input input)
        count-exited (count-exited-fn lenses)]
    (count-exited [[0 -1] :right]))
  ;; 8249

  (let [lenses (parse-input input)
        max-y (apply max (map first (keys lenses)))
        max-x (apply max (map second (keys lenses)))
        count-exited (count-exited-fn lenses)]
    (time (reduce max (concat (for [x (range 0 (inc max-x))
                                    beam [[[-1 x] :down] [[(inc max-y) x] :up]]]
                                (count-exited beam))
                              (for [y (range 0 (inc max-y))
                                    beam [[[y -1] :right] [[y (inc max-x)] :left]]]
                                (count-exited beam))))))
  ;; 8444
  )
