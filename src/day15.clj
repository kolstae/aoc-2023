(ns day15
  (:require [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(defn parse-input [input]
  (-> (str/trim input) (str/split #",")))

(defn hash [s] (reduce (fn [cur c] (mod (* 17 (+ cur (int c))) 256)) 0 s))

(comment

  (->> (parse-input input)
       (mapv hash)
       (reduce +))
  ;; 513643

  (->> (parse-input input)
       (mapv #(str/split % #"[=-]"))
       (reduce (fn [boxes [lbl focal]]
                     (let [h (hash lbl)]
                       (update boxes h (if focal
                                         (fn [ls]
                                           (if-some [i (first (keep-indexed (fn [i [l]] (when (= l lbl) i)) ls))]
                                             (assoc ls i [lbl (parse-long focal)])
                                             (conj (or ls []) [lbl (parse-long focal)])))
                                         (partial filterv (comp not #{lbl} first))))))
                   {})
       (mapv (fn [[b-no ls]]
               (->> ls
                    (map-indexed (fn [l-no [_ focal]]
                                      (* (inc b-no) (inc l-no) focal)))
                    (reduce +))))
       (reduce +))
  ;; 265345
  )
