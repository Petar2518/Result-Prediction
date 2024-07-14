(ns project.core)

;(defn vozimMecku [sequence number]
;
;  (if (< (* (- 1) number) 0)
;    (loop [remaining sequence result [] i (* (- 1) number)]
;      (if (<= i 0)
;        (do (conj (first remaining) result)
;            (recur (next remaining) result (+ i 1))
;
;            )
;        (println result)
;
;        )
;
;
;      )
;    )
;  )
;
;(= (vozimMecku -2 [1 2 3 4 5]) '(4 5 1 2 3))

(defn parse
  "Convert csv file into rows of columns"
  [file]
  (map #(clojure.string/split % #",")
       (clojure.string/split (slurp file) #"\r\n")))
(def initial-data (parse "ae.csv"))

(def game-keys [:homeTeam :homeGoals :awayGoals :awayTeam :date])
(defn string->int
  [value]
  (Integer. value))

(def conversions {:homeTeam identity
                  :homeGoals string->int
                  :awayGoals string->int
                  :awayTeam identity
                  :date identity})

(defn convert [game-key value]
  ((get conversions game-key) value))

(defn add-labels
  [row]
  (map (fn [row-to-map]
         (reduce (fn [row-map [game-key value]] (assoc row-map game-key (convert game-key value)))
         {}
         (map vector game-keys row-to-map)))
  row))

;(defn add-labels2
;  [row]
;  [{:homeTeam (get row 0)
;    :homeGoals (get row 1)
;    :awayGoals (get row 2)
;    :awayTeam (get row 3)
;    :date (get row 4)}] )
;(defn prepare-data
;  [data]
;  (loop [remaining-data data prepared-data []]
;    (if (empty? remaining-data)
;      prepared-data
;      (let [[for-labeling & remaining] remaining-data]
;        (recur remaining
;               (into prepared-data
;                     (add-labels2 for-labeling)))))))
(def data-for-analysis (add-labels (parse "ae.csv")))

(defn get-games
  [host-team away-team]
  (reverse
    (sort-by :date
             (filter
               #(or
                  (= (:homeTeam %) host-team)
                  (= (:awayTeam %) host-team)
                  (= (:homeTeam %) away-team)
                  (= (:awayTeam %) away-team)
                  )
               (add-labels (parse "ae.csv"))
               ))))

(def data-between-teams (get-games "Chelsea" "Brighton"))

(defn get-all-for-team
  "Dodati data-between-teams"
  [team]
  (reverse (sort-by :date (filter #(or (= (:awayTeam %) team) (= (:homeTeam %) team)) data-between-teams))))

(def data-for-host (get-all-for-team "Chelsea"))
(def data-for-away (get-all-for-team "Arsenal"))

(defn get-by-home-team
  "Dodati data-for-host"
  [home-team]
  (reverse (sort-by :date (filter #(= (:homeTeam %) home-team) data-for-host))))

(defn get-by-away-team
  "Dodati data-for-away"
  [away-team]
  (reverse (sort-by :date (filter #(= (:awayTeam %) away-team) data-for-away))))
(defn get-mutual-games-same-stadium
  [host-team away-team]
  (reverse
    (sort-by :date
             (filter
               #(and
                  (= (:homeTeam %) host-team)
                  (= (:awayTeam %) away-team)
                  )
               (add-labels (parse "ae.csv"))
               ))))

(defn get-mutual-games-no-matter-stadium
  [games-team1,games-team2]
  (clojure.set/intersection (set games-team1)  (set  games-team2)))
(def host-games (get-by-home-team "Chelsea"))
(def away-games (get-by-home-team "Arsenal"))
(def mutual-games-same-host (get-mutual-games-same-stadium "Chelsea" "Arsenal"))
(def mutual-games-any-host (get-mutual-games-no-matter-stadium (get-all-for-team "Chelsea") (get-all-for-team "Arsenal")))

(defn count-any-team-goals-based-on-their-form
  "dodati ulazni parametar podatke"
  [team]
  (loop [remaining-data (take 10 (get-all-for-team team))  calculated-data 0 coefficient 0.19]
    (if (empty? remaining-data)
      (* calculated-data 0.1)
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining (+ calculated-data (* (if (= (:homeTeam for-calculating)  team) (:homeGoals for-calculating)  (:awayGoals for-calculating) ) coefficient)) (- coefficient 0.02)
               )))))

(defn count-any-team-goals-based-on-opponent-form
  "dodati ulazni parametar podatke"
  [team]
  (loop [remaining-data (take 10 (get-all-for-team team))  calculated-data 0 coefficient 0.19]
    (if (empty? remaining-data)
      (* calculated-data 0.05)
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining (+ calculated-data (* (if (= (:homeTeam for-calculating)  team) (:awayGoals for-calculating) (:homeGoals for-calculating) ) coefficient)) (- coefficient 0.02)
               )))))

(defn count-goals-for-host-based-on-home-form
  "dodati ulazni parametar podatke"
  []
  (loop [remaining-data (map :homeGoals (take 5 host-games))  calculated-data 0 coefficient 0.32]
    (if (empty? remaining-data)
      (* calculated-data 0.2)
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining (+ calculated-data (* for-calculating coefficient)) (- coefficient 0.06)
               )))))

(defn count-goals-for-host-based-on-away-form-of-away-team
  "dodati ulazni parametar podatke"
  []
  (loop [remaining-data (map :homeGoals (take 5 away-games))  calculated-data 0 coefficient 0.32]
    (if (empty? remaining-data)
      (* calculated-data 0.15)
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining (+ calculated-data (* for-calculating coefficient)) (- coefficient 0.06)
               )))))

(defn count-goals-for-away-based-on-away-form
  "dodati ulazni parametar podatke"
  []
  (loop [remaining-data (map :awayGoals (take 5 away-games))  calculated-data 0 coefficient 0.32]
    (if (empty? remaining-data)
      (* calculated-data 0.2)
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining (+ calculated-data (* for-calculating coefficient)) (- coefficient 0.06)
               )))))

(defn count-goals-for-away-based-on-host-form-of-host-team
  "dodati ulazni parametar podatke"
  []
  (loop [remaining-data (map :awayGoals (take 5 host-games))  calculated-data 0 coefficient 0.32]
    (if (empty? remaining-data)
      (* calculated-data 0.15)
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining (+ calculated-data (* for-calculating coefficient)) (- coefficient 0.06)
               )))))

;(defn count-goals-for-host-based-on-home-form-between-teams
;  "dodati ulazni parametar podatke"
;  []
;  (loop [remaining-data (map :homeGoals (take 4 mutual-games-same-host))  calculated-data 0 coefficient 0.325]
;    (if (empty? remaining-data)
;      (* calculated-data 0.35)
;      (let [[for-calculating & remaining] remaining-data]
;        (recur remaining (+ calculated-data (* for-calculating coefficient)) (- coefficient 0.05)
;               )))))
;
;(defn count-goals-for-away-based-on-away-form-between-teams
;  "dodati ulazni parametar podatke"
;  []
;  (loop [remaining-data (map :awayGoals (take 4 mutual-games-same-host))  calculated-data 0 coefficient 0.325]
;    (if (empty? remaining-data)
;      (* calculated-data 0.35)
;      (let [[for-calculating & remaining] remaining-data]
;        (recur remaining (+ calculated-data (* for-calculating coefficient)) (- coefficient 0.05)
;               )))))



(defn count-home-team-goals-based-on-home-form-between-teams
  "dodati ulazni parametar podatke"
  []
  (loop [remaining-data (map :homeGoals (take 4 mutual-games-same-host))  calculated-data 0 coefficient 0.325]
    (if (empty? remaining-data)
      (* calculated-data 0.35)
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining (+ calculated-data (* for-calculating coefficient)) (- coefficient 0.05)
               )))))

(defn count-away-team-goals-based-on-away-form-between-teams
  "dodati ulazni parametar podatke"
  []
  (loop [remaining-data (map :awayGoals (take 4 mutual-games-same-host))  calculated-data 0 coefficient 0.325]
    (if (empty? remaining-data)
      (* calculated-data 0.35)
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining (+ calculated-data (* for-calculating coefficient)) (- coefficient 0.05)
               )))))

(defn count-any-team-goals-based-on-form-between-teams
  "dodati ulazni parametar podatke"
  [team]
  (loop [remaining-data (take 8 mutual-games-any-host)  calculated-data 0 coefficient 0.195]
    (if (empty? remaining-data)
      (* calculated-data 0.15)
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining (+ calculated-data (* (if (= (:homeTeam for-calculating)  team) (:homeGoals for-calculating)  (:awayGoals for-calculating) ) coefficient)) (- coefficient 0.02)
               )))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (parse "ae.csv"))
  (println "a"))
;(+ 1 2)
;(def filename "ae.csv")
;(slurp filename)