(ns day09
  (:require [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45")

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv (comp (partial mapv parse-long)
                   #(str/split % #"\s+")))))

(defn take-until [pred coll]
  (loop [more coll res []]
    (if (seq more)
      (let [v (first more)]
        (if (pred v)
          (conj res v)
          (recur (next more) (conj res v))))
      res)))

(defn diffs [ns]
  (->> ns
       (iterate (fn [xs]
                  (->> xs
                       (partition 2 1)
                       (mapv (comp (partial apply -) reverse)))))
       (take-until #(= 1 (count (set %))))))

(comment

  (->> (parse-input input #_small-input)
       (mapv (fn [ns]
               (->> (diffs ns)
                    (mapv peek)
                    (reduce +))))
       (reduce +))
  ;; 2101499000

  (->> (parse-input input #_small-input)
       (mapv (fn [ns]
               (->> (diffs ns)
                    (mapv first)
                    reverse
                    (reduce #(- %2 %1)))))
       (reduce +))
  ;; 1089
  )
