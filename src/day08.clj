(ns day08
  (:require [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "RL\n\nAAA = (BBB, CCC)\nBBB = (DDD, EEE)\nCCC = (ZZZ, GGG)\nDDD = (DDD, DDD)\nEEE = (EEE, EEE)\nGGG = (GGG, GGG)\nZZZ = (ZZZ, ZZZ)")

(defn parse-input [input]
  (let [[inst & ls] (str/split-lines input)]
    [inst
     (into {}
           (map (fn [s]
                  (let [[at & ss] (str/split s #"\W+")]
                    [at (vec ss)])))
           (next ls))]))

(defn next-pos [ps at dir]
  (cond-> (get ps at)
    (= \R dir) second
    (= \L dir) first))

(defn count-steps [ps dirs start]
  (reduce (fn [[steps at] dir]
            (let [next-pos (next-pos ps at dir)]
              (if (= (last next-pos) \Z)
                (reduced (inc steps))
                [(inc steps) next-pos])))
          [0 start]
          (cycle dirs)))

(defn gcd [a b]
  (if (pos? (min a b))
    (recur b (mod a b))
    (max a b)))

(defn lcm [a b]
  (* a (/ b (gcd a b))))

(comment

  (let [[dirs ps] (parse-input input #_small-input #_"LLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)")]
    (count-steps ps dirs "AAA"))
  ;;19099

  (let [[dirs ps] (parse-input input #_"LR\n\n11A = (11B, XXX)\n11B = (XXX, 11Z)\n11Z = (11B, XXX)\n22A = (22B, XXX)\n22B = (22C, 22C)\n22C = (22Z, 22Z)\n22Z = (22B, 22B)\nXXX = (XXX, XXX)")]
    (->> (filterv (comp #{\A} last) (keys ps))
         (mapv (partial count-steps ps dirs))
         (reduce lcm)))
  ;;17099847107071
  )
