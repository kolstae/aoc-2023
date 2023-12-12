(ns day12
  (:require [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "???.### 1,1,3\n.??..??...?##. 1,1,3\n?#?#?#?#?#?#?#? 1,3,1,6\n????.#...#... 4,1,1\n????.######..#####. 1,6,5\n?###???????? 3,2,1")

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv (fn [l]
               (let [[s & ns] (str/split l #"[\s,]")]
                 [(str/replace s #"\.+" ".")
                  (mapv parse-long ns)])))))

(defn valid-suffixes [s ns]
  (for [i (range (inc (- (count s) ns)))
        :while (every? #{\. \?} (subs s 0 i))
        :let [suffix (subs s i)]
        :when (and (#{\. \?} (nth suffix ns \.))
                   (every? #{\# \?} (subs suffix 0 ns)))]
    (when (< (inc ns) (count suffix))
      (subs suffix (inc ns)))))

(declare mem-count-perm)

(defn count-perm* [[s ns]]
  (if-some [[n & nrs] (seq ns)]
    (->> (valid-suffixes s n)
         (map #(mem-count-perm [% nrs]))
         (reduce + ))
    (if (every? #{\. \?} s) 1 0)))

(def mem-count-perm (memoize count-perm*))

(comment

  (->> (parse-input input)
       (mapv (partial apply mem-count-perm))
       (reduce +))
  ;; 7771

  (->> (parse-input input)
       (mapv (fn [[row ns]]
               [(str/join \? (repeat 5 row))
                (into [] cat (repeat 5 ns))]))
       (mapv mem-count-perm)
       (reduce +)
       time)
  ;; 10861030975833
  )
