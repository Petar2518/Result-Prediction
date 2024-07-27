(ns project.calculate-result
  (:require [project.calculate-goals :as goals]
            [project.util :as util]))


(defn get-result-probabilities
  "Calculate probabilities for ceratin results between 0:0 and 7:7"
  [home-team away-team data]
  (for [x1 (goals/get-goal-probabilities-home home-team away-team data)
        x2 (goals/get-goal-probabilities-away home-team away-team data)]
    (util/round2 4 (* x1 x2))))

(defn add-result-probabilities
  "Assign probabilities to results"
  [home-team away-team data]
  (loop [remaining-data (get-result-probabilities home-team away-team data) final-data [] counter 0]
    (if (empty? remaining-data) final-data
                                (let [[for-labeling & remaining] remaining-data]
                                  (recur remaining

                                         (into final-data [{:homeTeam    home-team
                                                            :homeGoals   (quot counter 9)
                                                            :awayGoals   (mod counter 9)
                                                            :awayTeam    away-team
                                                            :probability for-labeling}])
                                         (+ counter 1))))))

(defn calculate-win-probabilities
  "Calculate probabilities for home team win / draw / away team win based on probabilities of results"
  [home-team away-team data]
  (loop [remaining-data (add-result-probabilities home-team away-team data) home-win 0 away-win 0 equal 0]
    (if (empty? remaining-data) [{:homeTeamWins (str (util/round2 2 (* 100 home-win)) "%")
                                  :draw         (str (util/round2 2 (* 100 equal)) "%")
                                  :awayTeamWins (str (util/round2 2 (* 100 away-win)) "%")}]
                                (let [[for-calculating & remaining] remaining-data]
                                  (recur remaining
                                         (if (> (:homeGoals for-calculating) (:awayGoals for-calculating)) (+ home-win (:probability for-calculating)) (+ home-win 0))
                                         (if (< (:homeGoals for-calculating) (:awayGoals for-calculating)) (+ away-win (:probability for-calculating)) (+ away-win 0))
                                         (if (= (:homeGoals for-calculating) (:awayGoals for-calculating)) (+ equal (:probability for-calculating)) (+ equal 0)))))))

(defn show-result-probabilities
  [home-team away-team data]
  (loop [remaining-data (add-result-probabilities home-team away-team data) final-data []]
    (if (empty? remaining-data)
      (sort-by :probability final-data)
      (let [[for-labeling & remaining] remaining-data
            home-team (:homeTeam for-labeling)
            away-team (:awayTeam for-labeling)
            home-goals (:homeGoals for-labeling)
            away-goals (:awayGoals for-labeling)
            probability (util/round2 2 (* 100 (:probability for-labeling)))]
        (if (> probability 1) (recur remaining
                                     (into final-data [{:homeTeam    home-team
                                                        :result      (str home-goals ":" away-goals)
                                                        :awayTeam    away-team
                                                        :probability probability}]))
                              (recur remaining final-data)

                              )))))



