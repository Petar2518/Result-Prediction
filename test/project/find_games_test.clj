(ns project.find-games-test
  (:require [midje.sweet :refer :all]
            [project.find-games :as games]
            [project.file_operations :as file]))

(def data (file/add-labels (file/parse "data.csv")))
(fact "Testing all games function (19 games expected)"
      (count (games/get-games "Chelsea" "" data))=> 19)
(fact "Testing all games function (19+19 - 1(mutual so duplicate is removed) = 37 expected)"
      (count (games/get-games "West Ham" "Brentford" data))=> 37)
(def games (games/get-games "Chelsea" "Arsenal" data))
(fact "Testing get all games for team function"
      (count (games/get-all-games-for-team "Chelsea" games)) => 19)
(fact "Testing get all games for team function"
      (count (games/get-all-games-for-team "Arsenal" games)) => 19)
(fact "Testing get all games for team function"
      (count (games/get-all-games-for-team "Arsenal" games)) => 19)
(fact "Testing get home games for team function"
      (count (games/get-home-games-for-team "Chelsea" games)) => 10)
(fact "Testing get home games for team function"
      (count (games/get-home-games-for-team "Arsenal" games)) => 10)
(fact "Testing get away games for team function"
      (count (games/get-away-games-for-team "Chelsea" games)) => 9)
(fact "Testing get away games for team function"
      (count (games/get-away-games-for-team "Arsenal" games)) => 9)
(fact "Testing get mutual games same stadium function"
      (count (games/get-mutual-games-same-stadium "Chelsea" "Arsenal" games)) => 1)
(fact "Testing get mutual games same stadium function"
      (count (games/get-mutual-games-same-stadium "Arsenal" "Chelsea" games)) => 0)
(fact "Testing get mutual games no matter stadium function"
      (count (games/get-mutual-games-no-matter-stadium (games/get-all-games-for-team "Arsenal" games) (games/get-all-games-for-team "Chelsea" games))) => 1)
(fact "Testing get mutual games no matter stadium function"
      (games/get-mutual-games-no-matter-stadium (games/get-all-games-for-team "Arsenal" games) (games/get-all-games-for-team "Chelsea" games)  ) => #{{:awayGoals 2
                                                                                                                                                     :awayTeam "Arsenal"
                                                                                                                                                     :date "2024-03-27"
                                                                                                                                                     :homeGoals 2
                                                                                                                                                     :homeTeam "Chelsea"}})
