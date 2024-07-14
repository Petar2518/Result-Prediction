(ns project.core
  (:require   [clojure.set :as presek]
              [clojure.math :as math]))


(defn parse
  "Convert csv file into rows of columns"
  [file]
  (map #(clojure.string/split % #",")
       (clojure.string/split (slurp file) #"\r\n")))


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

(def data-between-teams (get-games "Chelsea" "Arsenal"))

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
               (get-games host-team away-team)
               ))))

(defn get-mutual-games-no-matter-stadium
  [home-team,away-team]
  (presek/intersection (set (get-all-for-team home-team))  (set  (get-all-for-team away-team))))

(defn count-any-team-goals-based-on-their-form
  "dodati ulazni parametar podatke"
  [team]
  (loop [remaining-data (take 10 (get-all-for-team team))  calculated-data 0 coefficient-sum 0 coefficient 0.19 ]
    (if (empty? remaining-data)
      (if (= 0 coefficient-sum) 0 (/  (* calculated-data 0.1) coefficient-sum) )
      (let [[for-calculating & remaining] remaining-data]
        (recur
          remaining
          (+ calculated-data (* (if (= (:homeTeam for-calculating)  team) (:homeGoals for-calculating)  (:awayGoals for-calculating) ) coefficient))
          (+ coefficient-sum coefficient)
          (- coefficient 0.02)
               )))))

(defn count-any-team-goals-based-on-opponent-form
  "dodati ulazni parametar podatke"
  [team]
  (loop [remaining-data (take 10 (get-all-for-team team))  calculated-data 0 coefficient-sum 0 coefficient 0.19 ]
    (if (empty? remaining-data)
      (if (= 0 coefficient-sum) 0 (/ (* calculated-data 0.05) coefficient-sum))
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining
               (+ calculated-data (* (if (= (:homeTeam for-calculating)  team) (:awayGoals for-calculating) (:homeGoals for-calculating) ) coefficient))
               (+ coefficient-sum coefficient)
               (- coefficient 0.02)
               )))))

(defn count-goals-for-host-based-on-home-form
  "dodati ulazni parametar podatke"
  [team]
  (loop [remaining-data (map :homeGoals (take 5 (get-by-home-team team)))  calculated-data 0 coefficient-sum 0 coefficient 0.32]
    (if (empty? remaining-data)
      (if (= 0 coefficient-sum) 0 (/ (* calculated-data 0.2) coefficient-sum))

      (let [[for-calculating & remaining] remaining-data]
        (recur remaining
               (+ calculated-data (* for-calculating coefficient))
               (+ coefficient-sum coefficient)
               (- coefficient 0.06)
               )))))

(defn count-goals-for-host-based-on-away-form-of-away-team
  "dodati ulazni parametar podatke"
  [team]
  (loop [remaining-data (map :homeGoals (take 5 (get-by-away-team team)))  calculated-data 0 coefficient-sum 0 coefficient 0.32]
    (if (empty? remaining-data)
      (if (= 0 coefficient-sum) 0 (/(* calculated-data 0.15) coefficient-sum))

      (let [[for-calculating & remaining] remaining-data]
        (recur remaining
               (+ calculated-data (* for-calculating coefficient))
               (+ coefficient-sum coefficient)
               (- coefficient 0.06)
               )))))

(defn count-goals-for-away-based-on-away-form
  "dodati ulazni parametar podatke"
  [team]
  (loop [remaining-data (map :awayGoals (take 5 (get-by-away-team team)))  calculated-data 0 coefficient-sum 0 coefficient 0.32]
    (if (empty? remaining-data)
      (if (= 0 coefficient-sum) 0 (/ (* calculated-data 0.2) coefficient-sum))
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining
               (+ calculated-data (* for-calculating coefficient))
               (+ coefficient-sum coefficient)
               (- coefficient 0.06)
               )))))

(defn count-goals-for-away-based-on-host-form-of-host-team
  "dodati ulazni parametar podatke"
  [team]
  (loop [remaining-data (map :awayGoals (take 5 (get-by-home-team team)))  calculated-data 0 coefficient-sum 0 coefficient 0.32]
    (if (empty? remaining-data)
      (if (= 0 coefficient-sum) 0 (/ (* calculated-data 0.15) coefficient-sum))
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining
               (+ calculated-data (* for-calculating coefficient))
               (+ coefficient-sum coefficient)
               (- coefficient 0.06)
               )))))

(defn count-home-team-goals-based-on-home-form-between-teams
  "dodati ulazni parametar podatke"
  [home-team away-team]
  (loop [remaining-data (map :homeGoals (take 4 (get-mutual-games-same-stadium home-team away-team)))  calculated-data 0 coefficient-sum 0 coefficient 0.325]
    (if (empty? remaining-data)
      (if (= 0 coefficient-sum) 0 (/ (* calculated-data 0.35) coefficient-sum))
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining
               (+ calculated-data (* for-calculating coefficient))
               (+ coefficient-sum coefficient)
               (- coefficient 0.05)
               )))))

(defn count-away-team-goals-based-on-away-form-between-teams
  "dodati ulazni parametar podatke"
  [home-team away-team]
  (loop [remaining-data (map :awayGoals (take 4 (get-mutual-games-same-stadium home-team away-team)))  calculated-data 0 coefficient-sum 0 coefficient 0.325]
    (if (empty? remaining-data)
      (if (= 0 coefficient-sum) 0 (/ (* calculated-data 0.35) coefficient-sum))
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining
               (+ calculated-data (* for-calculating coefficient))
               (+ coefficient-sum coefficient)
               (- coefficient 0.05)
               )))))

(defn count-home-team-goals-based-on-form-between-teams-no-matter-host
  "dodati ulazni parametar podatke"
  [home-team away-team]
  (loop [remaining-data (take 8 (get-mutual-games-no-matter-stadium home-team away-team))  calculated-data 0 coefficient-sum 0 coefficient 0.195]
    (if (empty? remaining-data)
      (if (= 0 coefficient-sum) 0 (/ (* calculated-data 0.15) coefficient-sum))
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining
               (+ calculated-data (* (if (= (:homeTeam for-calculating)  home-team) (:homeGoals for-calculating)  (:awayGoals for-calculating) ) coefficient))
               (+ coefficient-sum coefficient)
               (- coefficient 0.02)
               )))))

(defn count-away-team-goals-based-on-form-between-teams-no-matter-host
  "dodati ulazni parametar podatke"
  [home-team away-team]
  (loop [remaining-data (take 8 (get-mutual-games-no-matter-stadium home-team away-team))  calculated-data 0 coefficient-sum 0 coefficient 0.195]
    (if (empty? remaining-data)
      (if (= 0 coefficient-sum) 0 (/ (* calculated-data 0.15) coefficient-sum))
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining
               (+ calculated-data (* (if (= (:homeTeam for-calculating)  away-team) (:homeGoals for-calculating)  (:awayGoals for-calculating) ) coefficient))
               (+ coefficient-sum coefficient)
               (- coefficient 0.02)
               )))))

(defn count-expected-goals-home-team
  [host-team away-team]
  (+ (count-any-team-goals-based-on-their-form host-team)
     (count-any-team-goals-based-on-opponent-form away-team)
     (count-goals-for-host-based-on-home-form host-team)
     (count-goals-for-host-based-on-away-form-of-away-team away-team)
     (count-home-team-goals-based-on-form-between-teams-no-matter-host host-team away-team)
     (count-home-team-goals-based-on-home-form-between-teams host-team away-team)
     ))

(defn count-expected-goals-away-team
  [host-team away-team]
  (+ (count-any-team-goals-based-on-their-form away-team)
     (count-any-team-goals-based-on-opponent-form host-team)
     (count-goals-for-away-based-on-away-form away-team)
     (count-goals-for-away-based-on-host-form-of-host-team host-team)
     (count-away-team-goals-based-on-form-between-teams-no-matter-host host-team away-team)
     (count-away-team-goals-based-on-away-form-between-teams host-team away-team)
     ))

(defn factorial [n]
  (reduce * (range 1 (inc n))))

(defn round2
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn poisson-distribution
  [expected-goals k]

  (* (/ (Math/pow expected-goals k) (factorial k)) (Math/pow math/E (- expected-goals))))

(defn get-goal-probabilities-home
  [home-team away-team]
  (map #(poisson-distribution (count-expected-goals-home-team home-team away-team)  %) [0 1 2 3 4 5 6 7 8]))

(defn get-goal-probabilities-away
  [home-team away-team]
  (map #(poisson-distribution (count-expected-goals-away-team home-team away-team)  %) [0 1 2 3 4 5 6 7 8]))

(defn get-result-probabilities
  [s1 s2]
  (for [x1 s1
        x2 s2]

       (round2 4 (* x1 x2)) ))


(defn add-result-probabilities
  [row home-team away-team]
  (loop [remaining-data row final-data [] counter 0  ]
    (if (empty? remaining-data) final-data
      (let [[for-labeling & remaining] remaining-data]
        (recur remaining

               (into final-data [{:homeTeam home-team
                                  :homeGoals (quot counter 9)
                                  :awayGoals (mod counter 9)
                                  :awayTeam away-team
                                  :probability for-labeling}] )
               (+ counter 1))))))
(def data (add-result-probabilities (get-result-probabilities (get-goal-probabilities-home "Chelsea" "Arsenal")(get-goal-probabilities-away "Chelsea" "Arsenal")) "Chelsea" "Arsenal"))

(defn calculate-win-probabilities
  [row]
  (loop [remaining-data row home-win 0 away-win 0 equal 0]
    (if (empty? remaining-data) [{:homeTeamWins (round2 2 (* 100 home-win))
                                  :draw (round2 2 (* 100 equal))
                                  :awayTeamWins (round2 2 (* 100 away-win)) }]
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining
               (if (> (:homeGoals for-calculating)  (:awayGoals for-calculating)  ) (+ home-win (:probability for-calculating) ) (+ home-win 0))
               (if (< (:homeGoals for-calculating)  (:awayGoals for-calculating)  ) (+ away-win (:probability for-calculating) ) (+ away-win 0))
               (if (= (:homeGoals for-calculating)  (:awayGoals for-calculating)  ) (+ equal (:probability for-calculating) ) (+ equal 0) ))))))

[{:homeTeamWins 0.8248, :draw 0.1051, :awayTeamWins 0.0432}]

(defn show-results
  [home-team away-team]
  (def data-between-teams (get-games home-team away-team))
  (def data-for-host (get-all-for-team home-team))
  (def data-for-away (get-all-for-team away-team))
  (def data (add-result-probabilities (get-result-probabilities (get-goal-probabilities-home home-team away-team)(get-goal-probabilities-away home-team away-team)) home-team away-team))
  (println data)

  (calculate-win-probabilities data))

(defn -main
  "I don't do a whole lot ... yet."
  []
  (println (show-results "Chelsea" "Arsenal")))

;(+ 1 2)
;(def filename "ae.csv")
;(slurp filename)