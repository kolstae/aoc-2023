(ns day01
  (:require [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet")
(def small-input-2 "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen")

(def digit-names (zipmap ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine"]
                         ["1e" "t2o" "t3e" "4" "5e" "si6x" "se7ven" "ei8ght" "n9e"]))
(def digit-name-re (re-pattern (str/join "|" (keys digit-names))))

(defn replace-all [s]
  (let [s' (str/replace s digit-name-re digit-names)]
    (if (= s' s)
      s
      (recur s'))))

(defn first+last-digit [s]
  ((juxt first peek) (filterv #(Character/isDigit ^char %) s)))

(comment

  (->> input
       (str/split-lines)
       (map (fn [s]
              (->> s
                   first+last-digit
                   str/join
                   parse-long)))
       (apply +))
  ;; 56397


  (->> input
       (str/split-lines)
       (map (fn [s]
              (->> (replace-all s)
                   first+last-digit
                   #_(vector s (first+last-digit (str/replace s digit-name-re digit-names)))

                   str/join
                   parse-long)))
       #_(remove (comp (partial apply =) next))
       (apply +))
  ;; 55701
  )
