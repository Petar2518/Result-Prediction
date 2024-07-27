(ns project.find-games
  (:require [clojure.set :as intersection])
  )


(defn get-games
  "Get all games that were played by either home or away team"
  [host-team away-team data]
  (reverse
    (sort-by :date
             (filter
               #(or
                  (= (:homeTeam %) host-team)
                  (= (:awayTeam %) host-team)
                  (= (:homeTeam %) away-team)
                  (= (:awayTeam %) away-team)
                  )
               data))))

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