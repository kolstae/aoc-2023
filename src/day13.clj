(ns day13
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "#.##..##.\n..#.##.#.\n##......#\n##......#\n..#.##.#.\n..##..##.\n#.#.##.#.\n\n#...##..#\n#....#..#\n..##..###\n#####.##.\n#####.##.\n..##..###\n#....#..#")

(defn parse-input [input]
  (->> (str/split-lines input)
       (partition-by empty?)
       (filterv next)
       (mapv vec)))

(defn line-reflections [l]
  (let [len (count l)
        mid (/ len 2)]
    (for [i (range 1 len)
          :let [s (if (< mid i) (subs l (- i (- len i)) i) (subs l 0 i))
                r (str/join (reverse (if (< mid i) (subs l i len) (subs l i (+ i i)))))]
          :when (= s r)]
      i)))

(defn reflections [unwanted ls]
  (or (some->> ls
               (mapv (comp set line-reflections))
               (reduce set/intersection)
               (remove #{unwanted})
               first)
      (some->> ls
               (apply mapv vector)
               (mapv (comp set line-reflections str/join))
               (reduce set/intersection)
               (remove #{(long (/ unwanted 100))})
               first
               (* 100))))

(defn other-reflection [ls]
  (let [original (reflections -1 ls)]
    (first
      (for [y (range (count ls)) x (range (count (first ls)))
            :let [n-ls (update ls y #(-> % vec (update x {\. \# \# \.}) str/join))
                  r (reflections original n-ls)]
            :when r]
        r))))

(comment

  (->> (parse-input input)
       (mapv (partial reflections -1))
       (reduce +))
  ;; 35210

  (->> (parse-input input)
       (mapv other-reflection)
       (reduce +)
       time)
  ;; 31974
  )
