(ns day20
  (:require [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "broadcaster -> a, b, c\n%a -> b\n%b -> c\n%c -> inv\n&inv -> a")
(def small-input-2 "broadcaster -> a\n%a -> inv, con\n&inv -> b\n%b -> con\n&con -> output")

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv (fn [s]
               (let [[type-or-id id :as op] (mapv keyword (re-seq #"[%&]|\w+" s))]
                 (if (#{:% :&} type-or-id)
                   [id {:op type-or-id :outputs (vec (nnext op))}]
                   [type-or-id {:outputs (vec (next op))}]))))
       (into {})))

(defn init-state [ops]
  (into {}
        (keep (fn [[id {:keys [op]}]]
                (case op
                  :% [id false]
                  :& [id (into {}
                               (comp (filter (comp #{:& :%} :op val))
                                     (filter (comp (partial some #{id}) :outputs val))
                                     (map (juxt key (constantly :low))))
                               ops)]
                  nil)))
        ops))

(defn push-button [ops state]
  (loop [[[src pulse id] & pulses] [[:button :low :broadcaster]]
         state (update-in state [:sig-count :low] (fnil inc 0))]
    (if id
      (let [{:keys [op outputs]} (get ops id)
            [state out-pulse] (case op
                                :& (let [state (cond-> (assoc-in state [id src] pulse)
                                                 (and (= id :dg) (= :high pulse))
                                                 (update :dg-high (fnil conj #{}) src))]
                                     [state (if (every? #{:high} (vals (state id)))
                                              :low
                                              :high)])
                                :% (if (= :low pulse)
                                     [(update state id not)
                                      (if (state id) :low :high)]
                                     [state])
                                [state pulse])]
        (recur (into (vec pulses)
                     (when out-pulse
                       (map (partial vector id out-pulse) outputs)))
               (cond-> state
                 out-pulse (update-in [:sig-count out-pulse] (fnil + 0) (count outputs)))))
      state)))

(defn gcd [a b]
  (if (pos? (min a b))
    (recur b (mod a b))
    (max a b)))

(defn lcm [a b]
  (* a (/ b (gcd a b))))

(comment

  (let [ops (parse-input input)]
    (->> (init-state ops)
         (iterate (partial push-button ops))
         (drop 1000)
         first
         :sig-count
         vals
         (reduce *)))
  ;; 866435264

  (let [ops (parse-input input)]
    (->> (init-state ops)
         (iterate (partial push-button ops))
         (map-indexed #(vector %1 (:dg-high %2)))
         (reduce (fn [phases [iter dg]]
                   (if (= #{:sp :lk :xt :zv} (set (keys phases)))
                     (reduced phases)
                     (into phases
                           (comp (remove phases)
                                 (map #(vector % iter)))
                           dg)))
                 {})
         vals
         (reduce lcm)))
  ;; 229215609826339
  )
