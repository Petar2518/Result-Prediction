(ns project.calculate-result-test
  (:require [midje.sweet :refer :all]
            [project.calculate-goals :as goals]
            [project.calculate-result :as result]
            [project.find-games :as games]
            [project.util :as util]
            [project.file_operations :as file]))

(def data (file/add-labels (file/parse "test.csv")))

(fact "Testing get-result-probabilities function"
      (first (result/get-result-probabilities "Chelsea" "Liverpool" data))  => 0.0498)
(fact "Testing get-result-probabilities function"
      (second (result/get-result-probabilities "Chelsea" "Liverpool" data))  => 0.0498)
(fact "Testing get-result-probabilities function"
      (last (result/get-result-probabilities "Chelsea" "Liverpool" data))  => 0.0)

(fact "Testing add-result-probabilities function"
      (first (result/add-result-probabilities "Chelsea" "Liverpool" data))  => {:homeTeam "Chelsea" :homeGoals 0 :awayGoals 0 :awayTeam "Liverpool" :probability 0.0498})
(fact "Testing add-result-probabilities function"
      (second (result/add-result-probabilities "Chelsea" "Liverpool" data))  => {:homeTeam "Chelsea" :homeGoals 0 :awayGoals 1 :awayTeam "Liverpool" :probability 0.0498})
(fact "Testing add-result-probabilities function"
      (last (result/add-result-probabilities "Chelsea" "Liverpool" data))  => {:homeTeam "Chelsea" :homeGoals 8 :awayGoals 8 :awayTeam "Liverpool" :probability 0.0})

(fact "Testing calculate-win-probabilities function"
      (last (result/calculate-win-probabilities "Chelsea" "Liverpool" data))  => {:homeTeamWins "60.56%" :draw "21.18%" :awayTeamWins "18.24%"})

(fact "Testing show-result-probabilities function"
      (last (result/show-result-probabilities "Chelsea" "Liverpool" data))  => {:homeTeam "Chelsea" :awayTeam "Liverpool" :result "2:1" :probability 9.96})
(fact "Testing show-result-probabilities function"
      (second (reverse (result/show-result-probabilities "Chelsea" "Liverpool" data)))  => {:homeTeam "Chelsea" :awayTeam "Liverpool" :result "2:0" :probability 9.96})
