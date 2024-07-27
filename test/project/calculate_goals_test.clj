(ns project.calculate-goals-test
  (:require [midje.sweet :refer :all]
            [project.calculate-goals :as goals]
            [project.find-games :as games]
            [project.util :as util]
            [project.file_operations :as file]))
(def data (file/add-labels (file/parse "test.csv")))
(def liverpool-all-games (games/get-all-games-for-team "Liverpool" data))
(def chelsea-all-games (games/get-all-games-for-team "Chelsea" data))
(def chelsea-home-games (games/get-home-games-for-team "Chelsea" chelsea-all-games))
(def liverpool-away-games (games/get-away-games-for-team "Liverpool" liverpool-all-games))
(fact "Testing count-any-team-goals-based-on-their-form function"
      (util/round2 2 ( goals/count-any-team-goals-based-on-their-form "Liverpool" liverpool-all-games) ) => 0.1)
(fact "Testing count-any-team-goals-based-on-their-form function"
      (util/round2 2 ( goals/count-any-team-goals-based-on-their-form "Chelsea" chelsea-all-games) ) => 0.2)

(fact "Testing count-any-team-goals-based-on-opponent-form function"
      (util/round2 2 (goals/count-any-team-goals-based-on-opponent-form "Liverpool" liverpool-all-games)) => 0.1)
(fact "Testing count-any-team-goals-based-on-opponent-form function"
      (util/round2 2 (goals/count-any-team-goals-based-on-opponent-form "Chelsea" chelsea-all-games)) => 0.05)

(fact "Testing count-goals-for-host-based-on-home-form function"
      (util/round2 2 (goals/count-goals-for-host-based-on-home-form chelsea-home-games)) => 0.4)

(fact "Testing count-goals-for-host-based-on-home-form function"
      (util/round2 2 (goals/count-goals-for-host-based-on-away-form-of-away-team liverpool-away-games)) => 0.3)

(fact "Testing count-goals-for-host-based-on-away-form function"
      (util/round2 2 (goals/count-goals-for-away-based-on-away-form liverpool-away-games)) => 0.2)

(fact "Testing count-goals-for-away-based-on-host-form-of-host-team function"
      (util/round2 2 (goals/count-goals-for-away-based-on-host-form-of-host-team chelsea-home-games)) => 0.15)

(fact "Testing count-home-team-goals-based-on-home-form-between-teams function"
      (util/round2 2 (goals/count-home-team-goals-based-on-home-form-between-teams data)) => 0.7)

(fact "Testing count-away-team-goals-based-on-away-form-between-teams function"
      (util/round2 2 (goals/count-away-team-goals-based-on-away-form-between-teams data)) => 0.35)

(fact "Testing count-home-team-goals-based-on-form-between-teams-no-matter-host function"
      (util/round2 2 (goals/count-home-team-goals-based-on-form-between-teams-no-matter-host "Chelsea" data)) => 0.3)

(fact "Testing count-away-team-goals-based-on-form-between-teams-no-matter-host function"
      (util/round2 2 (goals/count-away-team-goals-based-on-form-between-teams-no-matter-host "Liverpool" data)) => 0.15)

(fact "Testing count-expected-goals-home-team function"
      (goals/count-expected-goals-home-team "Chelsea" "Liverpool" data)=> 2.0)

(fact "Testing count-expected-goals-away-team function"
      (goals/count-expected-goals-away-team "Chelsea" "Liverpool" data)=> 1.0)

(fact "Testing get-goal-probabilities-home function"
      (util/round2 2 (first(goals/get-goal-probabilities-home "Chelsea" "Liverpool")))=> 0.29)

(fact "Testing get-goal-probabilities-home function"
      (util/round2 2 (second(goals/get-goal-probabilities-home "Chelsea" "Liverpool")))=> 0.36)

(fact "Testing get-goal-probabilities-away function"
      (util/round2 2 (first(goals/get-goal-probabilities-away "Chelsea" "Liverpool")))=> 0.27)

(fact "Testing get-goal-probabilities-away function"
      (util/round2 2 (second(goals/get-goal-probabilities-away "Chelsea" "Liverpool")))=> 0.35)



