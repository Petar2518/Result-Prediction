(ns project.file-operations-test
  (:require [midje.sweet :refer :all]
            [project.file_operations :as file]))

(fact "Testing parse function"
      (file/parse "test.csv") => (list ["Chelsea" "2" "1" "Liverpool" "2024-01-01"]) )

(fact "Testing add labels function"
      (file/add-labels (file/parse "test.csv")) => (list {:homeTeam "Chelsea" :homeGoals 2 :awayGoals 1 :awayTeam "Liverpool" :date "2024-01-01"}))
