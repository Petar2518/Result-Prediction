(ns project.calculate-goals
  (:require [project.find-games :as games]
            [project.util :as util]))


(defn count-any-team-goals-based-on-their-form
  "Calculate coefficient based on amount of goals scored on previous 10 games"
  [team data]
  (loop [remaining-data (take 10 data) calculated-data 0 coefficient-sum 0 coefficient 0.19]
    (if (empty? remaining-data)
      (if (= 0 coefficient-sum) 0 (/ (* calculated-data 0.1) coefficient-sum))
      (let [[for-calculating & remaining] remaining-data]
        (recur
          remaining
          (+ calculated-data (* (if (= (:homeTeam for-calculating) team) (:homeGoals for-calculating) (:awayGoals for-calculating)) coefficient))
          (+ coefficient-sum coefficient)
          (- coefficient 0.02)
          )))))

(defn count-any-team-goals-based-on-opponent-form
  "Calculate coefficient based on amount of goals opponent conceded on previous 10 games"
  [team data]
  (loop [remaining-data (take 10 data) calculated-data 0 coefficient-sum 0 coefficient 0.19]
    (if (empty? remaining-data)
      (if (= 0 coefficient-sum) 0 (/ (* calculated-data 0.05) coefficient-sum))
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining
               (+ calculated-data (* (if (= (:homeTeam for-calculating) team) (:awayGoals for-calculating) (:homeGoals for-calculating)) coefficient))
               (+ coefficient-sum coefficient)
               (- coefficient 0.02)
               )))))

(defn count-goals-for-host-based-on-home-form
  "Calculate coefficient based on amount of goals host team scored on previous 5 games that they hosted"
  [data]
  (loop [remaining-data (map :homeGoals (take 5 data)) calculated-data 0 coefficient-sum 0 coefficient 0.32]
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
  (loop [remaining-data (map :homeGoals (take 5 data)) calculated-data 0 coefficient-sum 0 coefficient 0.32]
    (if (empty? remaining-data)
      (if (= 0 coefficient-sum) 0 (/ (* calculated-data 0.15) coefficient-sum))

      (let [[for-calculating & remaining] remaining-data]
        (recur remaining
               (+ calculated-data (* for-calculating coefficient))
               (+ coefficient-sum coefficient)
               (- coefficient 0.06)
               )))))

(defn count-goals-for-away-based-on-away-form
  "Calculate coefficient based on amount of goals opponent scored on previous 5 games where they played away from home"
  [data]
  (loop [remaining-data (map :awayGoals (take 5 data)) calculated-data 0 coefficient-sum 0 coefficient 0.32]
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
  (loop [remaining-data (map :awayGoals (take 5 data)) calculated-data 0 coefficient-sum 0 coefficient 0.32]
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
  (loop [remaining-data (map :homeGoals (take 4 data)) calculated-data 0 coefficient-sum 0 coefficient 0.325]
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
  (loop [remaining-data (map :awayGoals (take 4 data)) calculated-data 0 coefficient-sum 0 coefficient 0.325]
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
  (loop [remaining-data (take 8 data) calculated-data 0 coefficient-sum 0 coefficient 0.195]
    (if (empty? remaining-data)
      (if (= 0 coefficient-sum) 0 (/ (* calculated-data 0.15) coefficient-sum))
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining
               (+ calculated-data (* (if (= (:homeTeam for-calculating) home-team) (:homeGoals for-calculating) (:awayGoals for-calculating)) coefficient))
               (+ coefficient-sum coefficient)
               (- coefficient 0.02)
               )))))

(defn count-away-team-goals-based-on-form-between-teams-no-matter-host
  "Calculate coefficient based on amount of goals away team scored on previous 8 games between those two teams"
  [away-team data]
  (loop [remaining-data (take 8 data) calculated-data 0 coefficient-sum 0 coefficient 0.195]
    (if (empty? remaining-data)
      (if (= 0 coefficient-sum) 0 (/ (* calculated-data 0.15) coefficient-sum))
      (let [[for-calculating & remaining] remaining-data]
        (recur remaining
               (+ calculated-data (* (if (= (:homeTeam for-calculating) away-team) (:homeGoals for-calculating) (:awayGoals for-calculating)) coefficient))
               (+ coefficient-sum coefficient)
               (- coefficient 0.02)
               )))))



(defn count-expected-goals-home-team
  "Calculate expected goals for home team in upcoming match"
  [host-team away-team]
  (let [data (games/get-games host-team away-team)
        all-games-for-home-team (games/get-all-games-for-team host-team data)
        all-games-for-away-team (games/get-all-games-for-team away-team data)
        home-games-for-team (games/get-home-games-for-team host-team all-games-for-home-team)
        away-games-for-team (games/get-away-games-for-team away-team all-games-for-away-team)
        mutual-games (games/get-mutual-games-no-matter-stadium all-games-for-home-team all-games-for-away-team)
        mutual-games-same-host (games/get-mutual-games-same-stadium host-team away-team data)]
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
  (let [data (games/get-games host-team away-team)
        all-games-for-home-team (games/get-all-games-for-team host-team data)
        all-games-for-away-team (games/get-all-games-for-team away-team data)
        home-games-for-team (games/get-home-games-for-team host-team all-games-for-home-team)
        away-games-for-team (games/get-away-games-for-team away-team all-games-for-away-team)
        mutual-games (games/get-mutual-games-no-matter-stadium all-games-for-home-team all-games-for-away-team)
        mutual-games-same-host (games/get-mutual-games-same-stadium host-team away-team data)]
    (+ (count-any-team-goals-based-on-their-form away-team all-games-for-away-team)
       (count-any-team-goals-based-on-opponent-form host-team all-games-for-home-team)
       (count-goals-for-away-based-on-away-form away-games-for-team)
       (count-goals-for-away-based-on-host-form-of-host-team home-games-for-team)
       (count-away-team-goals-based-on-form-between-teams-no-matter-host away-team mutual-games)
       (count-away-team-goals-based-on-away-form-between-teams mutual-games-same-host)
       )))

(defn get-goal-probabilities-home
  "Calculate probabilities for home team to score certain amount of goals between 0 and 7"
  [home-team away-team]
  (map #(util/poisson-distribution (count-expected-goals-home-team home-team away-team) %) [0 1 2 3 4 5 6 7 8]))

(defn get-goal-probabilities-away
  "Calculate probabilities for away team to score certain amount of goals between 0 and 7"
  [home-team away-team]
  (map #(util/poisson-distribution (count-expected-goals-away-team home-team away-team) %) [0 1 2 3 4 5 6 7 8]))