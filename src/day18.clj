(ns day18
  (:require [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "R 6 (#70c710)\nD 5 (#0dc571)\nL 2 (#5713f0)\nD 2 (#d2c081)\nR 2 (#59c680)\nD 2 (#411b91)\nL 5 (#8ceee2)\nU 2 (#caa173)\nL 1 (#1b58a2)\nU 2 (#caa171)\nR 2 (#7807d2)\nU 3 (#a77fa3)\nL 2 (#015232)\nU 2 (#7a21e3)\n")

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv (fn [s]
               (let [[dir cnt color] (re-seq #"\w+" s)]
                 [dir (parse-long cnt)
                  (subs color 5) (Long/parseLong (subs color 0 5) 16)])))))

(defn- shoelace [[[x1 y1] [x2 y2]]]
  (- (* x1 y2) (* y1 x2)))

(defn- calculate [ps]
  (reduce + (map shoelace (partition 2 1 ps))))

(def dir->delta {"R" [1 0] "D" [0 1] "L" [-1 0] "U" [0 -1]
                 "0" [1 0] "1" [0 1] "2" [-1 0] "3" [0 -1]})

(comment

  (let [inst (parse-input input)]
    (-> (reduce (fn [ps [dir cnt]]
                  (conj ps (mapv + (peek ps) (mapv * [cnt cnt] (dir->delta dir)))))
                [[0 0]]
                inst)
        calculate
        (+ (reduce + (map second inst)))
        (quot 2)
        inc))
  ;; 46394

  (let [inst (mapv #(subvec % 2) (parse-input input))]
    (-> (reduce (fn [ps [dir cnt]]
                  (conj ps (mapv + (peek ps) (mapv * [cnt cnt] (dir->delta dir)))))
                [[0 0]]
                inst)
        calculate
        (+ (reduce + (map second inst)))
        (quot 2)
        inc))
  ;; 201398068194715
  )
