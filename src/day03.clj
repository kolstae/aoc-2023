(ns day03
  (:require [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598..")


(comment

  (let [ls (str/split-lines input)]
    (->> ls
         (keep-indexed
           (fn [l-idx l]
             (some->> (re-seq #"\d+|[^.\d]" l)
                      (reduce (fn [[acc i] s]
                                (if-some [n (parse-long s)]
                                  (let [idx (str/index-of l s i)]
                                    [(conj acc [(or n s) l-idx [idx (+ idx (count s))]])
                                     (+ idx (count s))])
                                  [acc (+ i (count s))]))
                              [[] 0])
                      first)))
         (mapcat identity)
         (filter (fn [[_n l-idx [from to]]]
                   (seq (for [l (range (dec l-idx) (+ 2 l-idx))
                              i (range (dec from) (inc to))
                              :when (re-matches #"[^.\d]" (str (get-in ls [l i] "")))]
                          true))))
         (mapv first)
         (reduce +)))
  ;; 560670

  (let [ls (str/split-lines input)]
    (->> ls
         (keep-indexed
           (fn [l-idx l]
             (some->> (re-seq #"\d+|[^.\d]" l)
                      (reduce (fn [[acc i] s]
                                (if-some [n (parse-long s)]
                                  (let [idx (str/index-of l s i)]
                                    [(conj acc [(or n s) l-idx [idx (+ idx (count s))]])
                                     (+ idx (count s))])
                                  [acc (+ i (count s))]))
                              [[] 0])
                      first)))
         (mapcat identity)
         (mapcat (fn [[n l-idx [from to]]]
                   (for [l (range (dec l-idx) (+ 2 l-idx))
                         i (range (dec from) (inc to))
                         :when (= \* (get-in ls [l i]))]
                     [[l i] n])))
         (group-by first)
         vals
         (filter #(= 2 (count %)))
         (mapv (comp (partial apply *) (partial map second)))
         (reduce +)))
  ;; 91622824
  )
