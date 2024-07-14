(ns project.core
  (:require   [clojure.set :as intersection]
              [clojure.math :as math]))


(defn parse
  "Convert csv file into rows of columns"
  [file]
  (map #(clojure.string/split % #",")
       (clojure.string/split (slurp file) #"\r\n")))


(def game-keys [:homeTeam :homeGoals :awayGoals :awayTeam :date])

(defn string->int
  "Transform string to integer"
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
  "Add keys to values"
  [row]
  (map (fn [row-to-map]
         (reduce (fn [row-map [game-key value]] (assoc row-map game-key (convert game-key value)))
                 {}
                 (map vector game-keys row-to-map)))
       row))
(def prepared-data (add-labels (parse "data.csv")))

(defn get-games
  "Get all games that were played by either home or away team"
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
               prepared-data))))

(defn get-all-games-for-team
  "Get all games played by team passed as argument"
  [team data]
  (reverse (sort-by :date (filter #(or (= (:awayTeam %) team) (= (:homeTeam %) team)) data))))


(defn get-home-games-for-team
  "Get all games played at home by team passed as argument"
  [home-team data]
  (reverse (sort-by :date (filter #(= (:homeTeam %) home-team) data))))

(defn get-away-games-for-team
  "Get all games played away from home by team passed as argument"
  [away-team data]
  (reverse (sort-by :date (filter #(= (:awayTeam %) away-team) data))))


(defn get-mutual-games-same-stadium
  "Get all games played between two teams that are passed as arguments where host was same"
  [host-team away-team data]
  (reverse
    (sort-by :date
             (filter
               #(and
                  (= (:homeTeam %) host-team)
                  (= (:awayTeam %) away-team)
                  )
               data
               ))))

(defn get-mutual-games-no-matter-stadium
  "Get all games played between two teams that are passed as arguments"
  [home-team-games away-team-games]
  (intersection/intersection (set home-team-games) (set away-team-games)))

(defn count-any-team-goals-based-on-their-form
  "Calculate coefficient based on amount of goals scored on previous 10 games"
  [team data]
  (loop [remaining-data (take 10 data)  calculated-data 0 coefficient-sum 0 coefficient 0.19 ]
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
  "Calculate coefficient based on amount of goals opponent conceded on previous 10 games"
  [team data]
  (loop [remaining-data (take 10 data)  calculated-data 0 coefficient-sum 0 coefficient 0.19 ]
    (if (empty? remaining-data)
      (if (= 0 coefficient-sum) 0 (/ (* calculated-data 0.05) coefficient-sum))
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining
               (+ calculated-data (* (if (= (:homeTeam for-calculating)  team) (:awayGoals for-calculating) (:homeGoals for-calculating) ) coefficient))
               (+ coefficient-sum coefficient)
               (- coefficient 0.02)
               )))))

(defn count-goals-for-host-based-on-home-form
  "Calculate coefficient based on amount of goals host team scored on previous 5 games that they hosted"
  [data]
  (loop [remaining-data (map :homeGoals (take 5 data))  calculated-data 0 coefficient-sum 0 coefficient 0.32]
    (if (empty? remaining-data)
      (if (= 0 coefficient-sum) 0 (/ (* calculated-data 0.2) coefficient-sum))

      (let [[for-calculating & remaining] remaining-data]
        (recur remaining
               (+ calculated-data (* for-calculating coefficient))
               (+ coefficient-sum coefficient)
               (- coefficient 0.06)
               )))))

(defn count-goals-for-host-based-on-away-form-of-away-team
  "Calculate coefficient based on amount of goals opponent conceded on previous 5 games where they played away from home"
  [data]
  (loop [remaining-data (map :homeGoals (take 5 data))  calculated-data 0 coefficient-sum 0 coefficient 0.32]
    (if (empty? remaining-data)
      (if (= 0 coefficient-sum) 0 (/(* calculated-data 0.15) coefficient-sum))

      (let [[for-calculating & remaining] remaining-data]
        (recur remaining
               (+ calculated-data (* for-calculating coefficient))
               (+ coefficient-sum coefficient)
               (- coefficient 0.06)
               )))))

(defn count-goals-for-away-based-on-away-form
  "Calculate coefficient based on amount of goals opponent scored on previous 5 games where they played away from home"
  [data]
  (loop [remaining-data (map :awayGoals (take 5 data))  calculated-data 0 coefficient-sum 0 coefficient 0.32]
    (if (empty? remaining-data)
      (if (= 0 coefficient-sum) 0 (/ (* calculated-data 0.2) coefficient-sum))
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining
               (+ calculated-data (* for-calculating coefficient))
               (+ coefficient-sum coefficient)
               (- coefficient 0.06)
               )))))

(defn count-goals-for-away-based-on-host-form-of-host-team
  "Calculate coefficient based on amount of goals opponent conceded on previous 5 games where they played home"
  [data]
  (loop [remaining-data (map :awayGoals (take 5 data))  calculated-data 0 coefficient-sum 0 coefficient 0.32]
    (if (empty? remaining-data)
      (if (= 0 coefficient-sum) 0 (/ (* calculated-data 0.15) coefficient-sum))
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining
               (+ calculated-data (* for-calculating coefficient))
               (+ coefficient-sum coefficient)
               (- coefficient 0.06)
               )))))

(defn count-home-team-goals-based-on-home-form-between-teams
  "Calculate coefficient based on amount of goals home team scored on previous 4 games between those two teams with same host"
  [data]
  (loop [remaining-data (map :homeGoals (take 4 data))  calculated-data 0 coefficient-sum 0 coefficient 0.325]
    (if (empty? remaining-data)
      (if (= 0 coefficient-sum) 0 (/ (* calculated-data 0.35) coefficient-sum))
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining
               (+ calculated-data (* for-calculating coefficient))
               (+ coefficient-sum coefficient)
               (- coefficient 0.05)
               )))))

(defn count-away-team-goals-based-on-away-form-between-teams
  "Calculate coefficient based on amount of goals away team scored on previous 4 games between those two teams with same host"
  [data]
  (loop [remaining-data (map :awayGoals (take 4 data))  calculated-data 0 coefficient-sum 0 coefficient 0.325]
    (if (empty? remaining-data)
      (if (= 0 coefficient-sum) 0 (/ (* calculated-data 0.35) coefficient-sum))
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining
               (+ calculated-data (* for-calculating coefficient))
               (+ coefficient-sum coefficient)
               (- coefficient 0.05)
               )))))

(defn count-home-team-goals-based-on-form-between-teams-no-matter-host
  "Calculate coefficient based on amount of goals home team scored on previous 8 games between those two teams"
  [home-team data]
  (loop [remaining-data (take 8 data)  calculated-data 0 coefficient-sum 0 coefficient 0.195]
    (if (empty? remaining-data)
      (if (= 0 coefficient-sum) 0 (/ (* calculated-data 0.15) coefficient-sum))
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining
               (+ calculated-data (* (if (= (:homeTeam for-calculating)  home-team) (:homeGoals for-calculating)  (:awayGoals for-calculating) ) coefficient))
               (+ coefficient-sum coefficient)
               (- coefficient 0.02)
               )))))

(defn count-away-team-goals-based-on-form-between-teams-no-matter-host
  "Calculate coefficient based on amount of goals away team scored on previous 8 games between those two teams"
  [away-team data]
  (loop [remaining-data (take 8 data)  calculated-data 0 coefficient-sum 0 coefficient 0.195]
    (if (empty? remaining-data)
      (if (= 0 coefficient-sum) 0 (/ (* calculated-data 0.15) coefficient-sum))
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining
               (+ calculated-data (* (if (= (:homeTeam for-calculating)  away-team) (:homeGoals for-calculating)  (:awayGoals for-calculating) ) coefficient))
               (+ coefficient-sum coefficient)
               (- coefficient 0.02)
               )))))



(defn count-expected-goals-home-team
  "Calculate expected goals for home team in upcoming match"
  [host-team away-team]
  (let [data (get-games host-team away-team)
        all-games-for-home-team (get-all-games-for-team host-team data)
        all-games-for-away-team (get-all-games-for-team away-team data)
        home-games-for-team (get-home-games-for-team host-team all-games-for-home-team)
        away-games-for-team (get-away-games-for-team away-team all-games-for-away-team)
        mutual-games (get-mutual-games-no-matter-stadium all-games-for-home-team all-games-for-away-team)
        mutual-games-same-host (get-mutual-games-same-stadium host-team away-team data)]
  (+ (count-any-team-goals-based-on-their-form host-team all-games-for-home-team)
     (count-any-team-goals-based-on-opponent-form away-team all-games-for-away-team)
     (count-goals-for-host-based-on-home-form home-games-for-team)
     (count-goals-for-host-based-on-away-form-of-away-team away-games-for-team)
     (count-home-team-goals-based-on-form-between-teams-no-matter-host host-team mutual-games)
     (count-home-team-goals-based-on-home-form-between-teams mutual-games-same-host)
     )))

(defn count-expected-goals-away-team
  "Calculate expected goals for away team in upcoming match"
  [host-team away-team]
  (let [data (get-games host-team away-team)
        all-games-for-home-team (get-all-games-for-team host-team data)
        all-games-for-away-team (get-all-games-for-team away-team data)
        home-games-for-team (get-home-games-for-team host-team all-games-for-home-team)
        away-games-for-team (get-away-games-for-team away-team all-games-for-away-team)
        mutual-games (get-mutual-games-no-matter-stadium all-games-for-home-team all-games-for-away-team)
        mutual-games-same-host (get-mutual-games-same-stadium host-team away-team data)]
  (+ (count-any-team-goals-based-on-their-form away-team all-games-for-away-team)
     (count-any-team-goals-based-on-opponent-form host-team all-games-for-home-team)
     (count-goals-for-away-based-on-away-form away-games-for-team)
     (count-goals-for-away-based-on-host-form-of-host-team home-games-for-team)
     (count-away-team-goals-based-on-form-between-teams-no-matter-host away-team mutual-games)
     (count-away-team-goals-based-on-away-form-between-teams mutual-games-same-host)
     )))

(defn factorial [n]
  "Calculate factorial of passed argument"
  (reduce * (range 1 (inc n))))

(defn round2
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn poisson-distribution
  "Apply Poisson's distribution with passed arguments for expected goals and amount of goals"
  [expected-goals k]
  (* (/ (Math/pow expected-goals k) (factorial k)) (Math/pow math/E (- expected-goals))))

(defn get-goal-probabilities-home
  "Calculate probabilities for home team to score certain amount of goals between 0 and 7"
  [home-team away-team]
  (map #(poisson-distribution (count-expected-goals-home-team home-team away-team)  %) [0 1 2 3 4 5 6 7 8]))

(defn get-goal-probabilities-away
  "Calculate probabilities for away team to score certain amount of goals between 0 and 7"
  [home-team away-team]
  (map #(poisson-distribution (count-expected-goals-away-team home-team away-team)  %) [0 1 2 3 4 5 6 7 8]))

(defn get-result-probabilities
  "Calculate probabilities for ceratin results between 0:0 and 7:7"
  [s1 s2]
  (for [x1 s1
        x2 s2]
    (round2 4 (* x1 x2)) ))

(defn add-result-probabilities
  "Assign probabilities to results"
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

(defn calculate-win-probabilities
  "Calculate probabilities for home team win / draw / away team win based on probabilities of results"
  [row]
  (loop [remaining-data row home-win 0 away-win 0 equal 0]
    (if (empty? remaining-data) [{:homeTeamWins (str  (round2 2 (* 100 home-win)) "%")
                                  :draw (str (round2 2 (* 100 equal)) "%")
                                  :awayTeamWins (str (round2 2 (* 100 away-win)) "%")  }]
                                (let [[for-calculating & remaining] remaining-data]
                                  (recur remaining
                                         (if (> (:homeGoals for-calculating)  (:awayGoals for-calculating)  ) (+ home-win (:probability for-calculating) ) (+ home-win 0))
                                         (if (< (:homeGoals for-calculating)  (:awayGoals for-calculating)  ) (+ away-win (:probability for-calculating) ) (+ away-win 0))
                                         (if (= (:homeGoals for-calculating)  (:awayGoals for-calculating)  ) (+ equal (:probability for-calculating) ) (+ equal 0) ))))))

(defn show-result-probabilities
  [row]
  (loop [remaining-data row final-data []]
    (if (empty? remaining-data)
    (sort-by :probability final-data)
    (let [[for-labeling & remaining] remaining-data
          home-team (:homeTeam for-labeling)
          away-team (:awayTeam for-labeling)
          home-goals (:homeGoals for-labeling)
          away-goals (:awayGoals for-labeling)
          probability (round2 2 (* 100 (:probability for-labeling)))]
      (if (> probability 1)(recur remaining
                                  (into final-data [{:homeTeam home-team
                                                     :result (str home-goals ":" away-goals)
                                                     :awayTeam away-team
                                                     :probability (str  probability "%")}]) )
                           (recur remaining final-data)

                       )))))

(defn menu
  "Start menu"
  []
  (println "")
  (println "Welcome!")
  (println "Choose an option:")
  (println "1. Predict winner of the game!")
  (println "2. Show result probabilities for game")
  (println "0. Exit")
  (print "Enter your choice: ")
  (flush))

(defn get-choice
  "Read user choice"
  []
  (read-line))

(defn prompt-user
  "Displays the given prompt to the user and reads input from the console."
  [prompt]
  (println prompt)
  (read-line))


(defn show-win-probabilities
  []
  (let [home-team (prompt-user "Enter home team: ")
        away-team (prompt-user "Enter away team: ")]
  (println (calculate-win-probabilities (add-result-probabilities (get-result-probabilities (get-goal-probabilities-home home-team away-team)(get-goal-probabilities-away home-team away-team)) home-team away-team)))))

(defn show-results-probabilities
  []
  (let [home-team (prompt-user "Enter home team: ")
        away-team (prompt-user "Enter away team: ")]
     (run! println (show-result-probabilities (add-result-probabilities (get-result-probabilities (get-goal-probabilities-home home-team away-team)(get-goal-probabilities-away home-team away-team)) home-team away-team)))))

(defn process-user-choice
  "Process users choice."
  [choice]
  (cond
    (= choice "1") (do
                     (show-win-probabilities)
                     (menu)
                     (process-user-choice (get-choice)))
    (= choice "2") (do
                     (show-results-probabilities)
                     (menu)
                     (process-user-choice (get-choice)))
    (= choice "0") (do
                     (println "Exiting...")
                     (System/exit 0))
    :else (do
            (println "Invalid choice. Please enter a valid option.")
            (menu)
            (process-user-choice (get-choice)))
    ))

(defn -main
  "I don't do a whole lot ... yet."
  []
  (menu)
  (process-user-choice (get-choice)))