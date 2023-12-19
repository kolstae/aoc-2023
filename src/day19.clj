(ns day19
  (:require [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "px{a<2006:qkq,m>2090:A,rfg}\npv{a>1716:R,A}\nlnx{m>1548:A,A}\nrfg{s<537:gd,x>2440:R,A}\nqs{s>3448:A,lnx}\nqkq{x<1416:A,crn}\ncrn{x>2662:A,R}\nin{s<1351:px,qqz}\nqqz{s>2770:qs,m<1801:hdj,R}\ngd{a>3333:R,R}\nhdj{m>838:A,pv}\n\n{x=787,m=2655,a=1222,s=2876}\n{x=1679,m=44,a=2067,s=496}\n{x=2036,m=264,a=79,s=2244}\n{x=2461,m=1339,a=466,s=291}\n{x=2127,m=1623,a=2188,s=1013}")

(defn parse-input [input]
  (let [[wrk _ parts] (->> (str/split-lines input)
                           (partition-by (comp boolean second)))]
    {:wrk (into {} (map (fn [s] (let [[id & es] (str/split s #"[{,}]")]
                                  [id (mapv (fn [e] (let [[id-or-f cmp n id] (re-seq #"\w+|[<>]" e)
                                                          n (some-> n parse-long)
                                                          cmp (case cmp
                                                                "<" #'<
                                                                ">" #'>
                                                                nil)]
                                                      (if cmp
                                                        [id-or-f cmp n id]
                                                        [id-or-f]))) es)])))
                wrk)
     :parts (->> (mapv (comp (partial into {} (map (comp vec next)))
                             #(re-seq #"(\w)=(\d+)" %)) parts)
                 (mapv #(update-vals % parse-long)))}))


(comment

  (let [{:keys [wrk parts]} (parse-input input)
        do-work (fn [[id-or-f cmp n id :as op] part]
                  #_(prn op part)
                  (if cmp
                    (when (cmp (get part id-or-f) n)
                      id)
                    id-or-f))]
    (->> parts
         (filterv (fn [p]
                    (loop [id "in"]
                      (if (#{"A" "R"} id)
                        (= id "A")
                        (recur (reduce (fn [_ op] (when-some [id (do-work op p)]
                                                    (reduced id)))
                                       nil (wrk id)))))))
         (mapcat vals)
         (reduce +)))
  ;; 432788

  (let [{:keys [wrk]} (parse-input input)
        do-work (fn [[id-or-f cmp n id :as op] part]
                  (if cmp
                    (if (= cmp #'<)
                      [[id (update part id-or-f assoc 1 (dec n))] (update part id-or-f assoc 0 n)]
                      [[id (update part id-or-f assoc 0 (inc n))] (update part id-or-f assoc 1 n)])
                    [[id-or-f part]]))]
    (->> (loop [ps {"in" {"x" [1 4000] "m" [1 4000] "a" [1 4000] "s" [1 4000]}}
                accepted []]
           (if (empty? ps)
             accepted
             (let [[id part] (first ps)
                   next-ps (reduce (fn [ps op]
                                     (into (pop ps) (do-work op (peek ps))))
                                   [part]
                                   (wrk id))]
               (recur (into (dissoc ps id)
                            (remove (comp #{"A" "R"} first))
                            next-ps)
                      (into accepted
                            (comp (filter (comp #{"A"} first))
                                  (map second))
                            next-ps)))))
         (map #(->> %
                    vals
                    (map (fn [[f t]] (- t (dec f))))
                    (reduce *)))
         (reduce +)))
  ;; 142863718918201
  )
